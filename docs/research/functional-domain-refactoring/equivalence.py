#!/usr/bin/env python3
"""Records what a compiler binary produces for every program in the repository,
so that two builds of the compiler can be compared.

  equivalence.py snapshot COMPILER OUTDIR    run COMPILER over the corpus
  equivalence.py compare OUTDIR_A OUTDIR_B   list the programs that differ

The corpus is every end-to-end test (test-programs/suites/*/*) and the standard
library with its tests (the module list in standard/Makefile). For each program
the snapshot keeps the exit status, the generated C, stdout and stderr.

A refactoring step must leave the snapshot unchanged. `compare` exits 1 when
any program differs, and prints a diff of the first few.
"""
import concurrent.futures
import difflib
import hashlib
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[3]
SUITES = ROOT / "test-programs" / "suites"
STANDARD = ROOT / "standard"
FILES = ("exit", "out.c", "stdout", "stderr")


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


def run_one(compiler: str, outdir: pathlib.Path, name: str, words: list[str]) -> str:
    target = outdir / name
    target.mkdir(parents=True, exist_ok=True)
    # The compiler writes error.html and calltree.html into its working directory.
    with tempfile.TemporaryDirectory() as cwd:
        c_path = os.path.join(cwd, "out.c")
        command = [compiler, *(w.replace("$C_PATH", c_path) for w in words)]
        done = subprocess.run(command, cwd=cwd, capture_output=True, timeout=600)
        generated = pathlib.Path(c_path).read_bytes() if os.path.exists(c_path) else b""
        scrub = lambda data: data.replace(cwd.encode(), b"$CWD").replace(str(ROOT).encode(), b"$ROOT")
        (target / "exit").write_text(f"{done.returncode}\n")
        (target / "out.c").write_bytes(scrub(generated))
        (target / "stdout").write_bytes(scrub(done.stdout))
        (target / "stderr").write_bytes(scrub(done.stderr))
    return name


def snapshot(compiler: str, outdir: pathlib.Path) -> None:
    compiler = str(pathlib.Path(compiler).resolve())
    if outdir.exists():
        shutil.rmtree(outdir)
    outdir.mkdir(parents=True)
    commands = [*suite_commands(), standard_command()]
    with concurrent.futures.ThreadPoolExecutor(max_workers=os.cpu_count() or 4) as pool:
        names = list(pool.map(lambda c: run_one(compiler, outdir, *c), commands))
    lines = []
    for name in sorted(names):
        digests = [hashlib.sha256((outdir / name / f).read_bytes()).hexdigest()[:16] for f in FILES]
        lines.append("\t".join([name, *digests]))
    (outdir / "index.tsv").write_text("\n".join(lines) + "\n")
    with_c = sum(1 for n in names if (outdir / n / "out.c").stat().st_size > 0)
    print(f"programs {len(names)}, with generated C {with_c}, without {len(names) - with_c}")


def read_index(outdir: pathlib.Path) -> dict[str, list[str]]:
    rows = (line.split("\t") for line in (outdir / "index.tsv").read_text().splitlines())
    return {row[0]: row[1:] for row in rows}


def compare(a: pathlib.Path, b: pathlib.Path) -> int:
    index_a, index_b = read_index(a), read_index(b)
    differing = []
    for name in sorted(index_a.keys() | index_b.keys()):
        if index_a.get(name) != index_b.get(name):
            parts = [f for f, x, y in zip(FILES, index_a.get(name, [""] * 4), index_b.get(name, [""] * 4)) if x != y]
            differing.append((name, parts))
    print(f"programs {len(index_a.keys() | index_b.keys())}, differing {len(differing)}")
    for name, parts in differing:
        print(f"DIFFERS {name}: {' '.join(parts)}")
    for name, parts in differing[:3]:
        for part in parts:
            old = (a / name / part).read_text(errors="replace").splitlines()
            new = (b / name / part).read_text(errors="replace").splitlines()
            for line in list(difflib.unified_diff(old, new, f"a/{name}/{part}", f"b/{name}/{part}", lineterm="", n=1))[:30]:
                print(line)
    return 1 if differing else 0


if __name__ == "__main__":
    if len(sys.argv) == 4 and sys.argv[1] == "snapshot":
        snapshot(sys.argv[2], pathlib.Path(sys.argv[3]))
    elif len(sys.argv) == 4 and sys.argv[1] == "compare":
        sys.exit(compare(pathlib.Path(sys.argv[2]), pathlib.Path(sys.argv[3])))
    else:
        sys.exit(__doc__)
