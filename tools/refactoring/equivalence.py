#!/usr/bin/env python3
"""Records what a compiler binary produces for every program in the repository,
so that two builds of the compiler can be compared.

  equivalence.py snapshot COMPILER OUTDIR    run COMPILER over the corpus
  equivalence.py compare OUTDIR_A OUTDIR_B   list the programs that differ
  equivalence.py compare --normalize-ids OUTDIR_A OUTDIR_B
                                             the same, after renumbering generated names

The corpus is every end-to-end test (test-programs/suites/*/*), the standard
library with its tests (the module list in standard/Makefile), every program
under examples/ (the command its Makefile runs) and every command-line test
(test-programs/cli/*, run in a directory holding a copy of its input files). For each program
the snapshot keeps the exit status, the generated C, stdout, stderr, the
error.html the compiler writes on failure, and the names of the files it leaves
in its working directory.

A refactoring step must leave the snapshot unchanged. `compare` exits 1 when
any program differs, and prints a diff of the first few.

The compiler numbers the names it generates (decl_286, mono_13, _t127, tmp4) from
global counters, in the order it allocates them. A step that only changes that
order changes the numbers and nothing else. `--normalize-ids` renumbers each
kind of generated name by first appearance in the file before comparing. The
renumbering is one-to-one, so it cannot hide any other difference.
"""
import concurrent.futures
import difflib
import hashlib
import os
import pathlib
import re
import shutil
import signal
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
SUITES = ROOT / "test-programs" / "suites"
STANDARD = ROOT / "standard"
EXAMPLES = ROOT / "examples"
CLI_TESTS = ROOT / "test-programs" / "cli"
CLI_NOT_INPUTS = {"args.txt", "expected.txt", "README.md"}
FILES = ("exit", "out.c", "stdout", "stderr", "error.html", "files")
# Generated names: a fixed prefix or a suffix, then the counter's value.
GENERATED = re.compile(r"\b(?:(decl_|mono_|meth_|_t|tmp)|([A-Za-z_][A-Za-z0-9_]*?_tmp_ref))(\d+)\b")


def normalize_ids(text: str) -> str:
    seen: dict[tuple[str, str], int] = {}

    def rename(match: re.Match) -> str:
        kind = match.group(1) or "_tmp_ref"
        stem = match.group(1) or match.group(2)
        index = seen.setdefault((kind, match.group(3)), len([k for k in seen if k[0] == kind]))
        return f"{stem}{index}"

    return GENERATED.sub(rename, text)


def suite_commands() -> list[tuple[str, list[str]]]:
    commands = []
    for test_dir in sorted(p for p in SUITES.glob("*/*") if p.is_dir()):
        cli = test_dir / "cli.txt"
        if cli.exists():
            # The first word names the compiler; the runner's other placeholders are kept.
            words = cli.read_text().strip().split(" ")[1:]
        else:
            words = [
                "compile",
                "$DIR/Test.aum",
                "--entrypoint=Test:main",
                "--target-type=c",
                "--output=$C_PATH",
                "--error-format=json",
            ]
        commands.append((f"suites/{test_dir.parent.name}/{test_dir.name}", [w.replace("$DIR", str(test_dir)) for w in words]))
    return commands


def standard_command() -> tuple[str, list[str]]:
    makefile = (STANDARD / "Makefile").read_text()
    modules = re.findall(r"^\t\t(\S+,\S+)", makefile, re.M)
    if not modules:
        sys.exit("no module list found in standard/Makefile")
    paths = [",".join(str(STANDARD / part) for part in pair.split(",")) for pair in modules]
    return "standard", ["compile", *paths, "--entrypoint=Standard.Test:Main", "--target-type=c", "--output=$C_PATH", "--error-format=json"]


def example_commands() -> list[tuple[str, list[str]]]:
    commands = []
    for example in sorted(p for p in EXAMPLES.iterdir() if (p / "Makefile").exists()):
        # `make -n` prints the compile command with the Makefile's variables expanded.
        printed = subprocess.run(["make", "-n", "-C", str(example)], capture_output=True, text=True).stdout
        line = next((l for l in printed.splitlines() if "austral compile" in l), None)
        if line is None:
            sys.exit(f"no compile command in {example}/Makefile")
        words = []
        for word in line.split()[1:]:
            if word.startswith("--output="):
                words += ["--target-type=c", "--output=$C_PATH"]
            elif word.startswith("--") or word == "compile":
                words.append(word)
            else:
                words.append(",".join(str((example / part).resolve()) for part in word.split(",")))
        commands.append((f"examples/{example.name}", [*words, "--error-format=json"]))
    return commands


def cli_commands() -> list[tuple[str, list[str], pathlib.Path]]:
    tests = sorted(p for p in CLI_TESTS.iterdir() if (p / "args.txt").exists()) if CLI_TESTS.exists() else []
    return [(f"cli/{t.name}", (t / "args.txt").read_text().split(), t) for t in tests]


def run_one(compiler: str, outdir: pathlib.Path, name: str, words: list[str], inputs: pathlib.Path | None = None) -> str:
    target = outdir / name
    target.mkdir(parents=True, exist_ok=True)
    # The compiler writes error.html into its working directory.
    with tempfile.TemporaryDirectory() as cwd:
        copied = set()
        if inputs is not None:
            for f in inputs.iterdir():
                if f.name not in CLI_NOT_INPUTS:
                    shutil.copy(f, cwd)
                    copied.add(f.name)
        c_path = os.path.join(cwd, "out.c")
        command = [compiler, *(w.replace("$C_PATH", c_path) for w in words)]
        done = subprocess.run(command, cwd=cwd, capture_output=True, timeout=600)
        generated = pathlib.Path(c_path).read_bytes() if os.path.exists(c_path) else b""
        # The compiler names its temporary C file at random.
        scrub = lambda data: re.sub(rb"/tmp/austral_[0-9a-f]+\.c", b"$TMP.c", data.replace(cwd.encode(), b"$CWD").replace(str(ROOT).encode(), b"$ROOT"))
        error_page = pathlib.Path(cwd, "error.html")
        left_behind = sorted(f.name for f in pathlib.Path(cwd).iterdir() if f.name != "out.c" and f.name not in copied)
        (target / "exit").write_text(f"{done.returncode}\n")
        (target / "out.c").write_bytes(scrub(generated))
        (target / "stdout").write_bytes(scrub(done.stdout))
        (target / "stderr").write_bytes(scrub(done.stderr))
        (target / "error.html").write_bytes(scrub(error_page.read_bytes()) if error_page.exists() else b"")
        (target / "files").write_text("".join(name + "\n" for name in left_behind))
    return name


def snapshot(compiler: str, outdir: pathlib.Path) -> None:
    compiler = str(pathlib.Path(compiler).resolve())
    if outdir.exists():
        shutil.rmtree(outdir)
    outdir.mkdir(parents=True)
    commands = [*suite_commands(), standard_command(), *example_commands(), *cli_commands()]
    # coverage.sh --errors --only DIR runs one suite, to see what its tests reach.
    only = os.environ.get("ZUSTRAL_EQUIVALENCE_ONLY")
    if only:
        prefix = "suites/" + pathlib.Path(only).name + "/"
        commands = [c for c in commands if c[0].startswith(prefix)]
    with concurrent.futures.ThreadPoolExecutor(max_workers=os.cpu_count() or 4) as pool:
        names = list(pool.map(lambda c: run_one(compiler, outdir, *c), commands))
    lines = []
    for name in sorted(names):
        digests = [hashlib.sha256((outdir / name / f).read_bytes()).hexdigest()[:16] for f in FILES]
        lines.append("\t".join([name, *digests]))
    (outdir / "index.tsv").write_text("\n".join(lines) + "\n")
    with_c = sum(1 for n in names if (outdir / n / "out.c").stat().st_size > 0)
    print(f"programs {len(names)}, with generated C {with_c}, without {len(names) - with_c}")


def view(outdir: pathlib.Path, name: str, part: str, normalize: bool) -> str:
    """The text of one recorded part, as the comparison sees it."""
    text = (outdir / name / part).read_text(errors="replace")
    if normalize and part == "out.c":
        text = normalize_ids(text)
    return text


def read_index(outdir: pathlib.Path, normalize: bool) -> dict[str, list[str]]:
    rows = [line.split("\t") for line in (outdir / "index.tsv").read_text().splitlines()]
    if not normalize:
        return {row[0]: row[1:] for row in rows}
    digest = lambda name, part: hashlib.sha256(view(outdir, name, part, normalize).encode()).hexdigest()[:16]
    return {row[0]: [digest(row[0], part) for part in FILES] for row in rows}


def compare(a: pathlib.Path, b: pathlib.Path, normalize: bool = False) -> int:
    index_a, index_b = read_index(a, normalize), read_index(b, normalize)
    differing = []
    for name in sorted(index_a.keys() | index_b.keys()):
        if index_a.get(name) != index_b.get(name):
            blank = [""] * len(FILES)
            parts = [f for f, x, y in zip(FILES, index_a.get(name, blank), index_b.get(name, blank)) if x != y]
            differing.append((name, parts))
    print(f"programs {len(index_a.keys() | index_b.keys())}, differing {len(differing)}")
    for name, parts in differing:
        print(f"DIFFERS {name}: {' '.join(parts)}")
    for name, parts in differing[:3]:
        if name not in index_a or name not in index_b:
            print(f"only in {'A' if name in index_a else 'B'}: {name}")
            continue
        for part in parts:
            old = view(a, name, part, normalize).splitlines()
            new = view(b, name, part, normalize).splitlines()
            for line in list(difflib.unified_diff(old, new, f"a/{name}/{part}", f"b/{name}/{part}", lineterm="", n=1))[:30]:
                print(line)
    return 1 if differing else 0


if __name__ == "__main__":
    # Let `| head` close the pipe without a traceback.
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)
    if len(sys.argv) == 4 and sys.argv[1] == "snapshot":
        snapshot(sys.argv[2], pathlib.Path(sys.argv[3]))
    elif len(sys.argv) >= 4 and sys.argv[1] == "compare":
        flags, dirs = sys.argv[2:-2], sys.argv[-2:]
        if set(flags) - {"--normalize-ids"}:
            sys.exit(__doc__)
        sys.exit(compare(pathlib.Path(dirs[0]), pathlib.Path(dirs[1]), "--normalize-ids" in flags))
    else:
        sys.exit(__doc__)
