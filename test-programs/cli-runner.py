#!/usr/bin/env python3
"""Runs the command-line tests in test-programs/cli.

  python3 test-programs/cli-runner.py [--compiler PATH] [--record] [NAME...]

A test is a directory with `args.txt` (the arguments, on one line) and
`expected.txt`. Every other file in the directory is an input. The runner copies
the inputs into an empty directory, runs the compiler there with the arguments,
and compares the exit status, the names of the files the compiler left behind,
stdout and stderr with `expected.txt`. `--record` writes `expected.txt` instead.

Some tests fix behaviour that an issue calls a bug. Their names end in the issue
number. The fix for the issue rewrites their `expected.txt`.
"""
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
TESTS = HERE / "cli"
NOT_INPUTS = {"args.txt", "expected.txt", "README.md"}


def tests() -> list[pathlib.Path]:
    return sorted(p for p in TESTS.iterdir() if (p / "args.txt").exists())


def arguments(test: pathlib.Path) -> list[str]:
    return (test / "args.txt").read_text().split()


def scrub(text: str, cwd: str) -> str:
    text = text.replace(cwd, "$CWD")
    # The compiler names its temporary C file at random.
    text = re.sub(r"/tmp/austral_[0-9a-f]+\.c", "$TMP.c", text)
    # What the C compiler prints depends on the C compiler.
    return re.sub(r"(Standard error:\n).*", r"\1$CC_STDERR\n", text, flags=re.S)


def run(compiler: str, test: pathlib.Path) -> str:
    with tempfile.TemporaryDirectory() as cwd:
        inputs = [f for f in test.iterdir() if f.name not in NOT_INPUTS]
        for f in inputs:
            shutil.copy(f, cwd)
        done = subprocess.run([compiler, *arguments(test)], cwd=cwd, capture_output=True, timeout=600)
        left = sorted(f.name for f in pathlib.Path(cwd).iterdir() if f.name not in {i.name for i in inputs})
        return (
            f"exit: {done.returncode}\n"
            f"files: {' '.join(left)}\n"
            f"--- stdout\n{scrub(done.stdout.decode(errors='replace'), cwd)}"
            f"--- stderr\n{scrub(done.stderr.decode(errors='replace'), cwd)}"
        )


def main() -> int:
    args = sys.argv[1:]
    compiler = str((HERE.parent / "austral").resolve())
    if "--compiler" in args:
        i = args.index("--compiler")
        compiler = str(pathlib.Path(args[i + 1]).resolve())
        del args[i : i + 2]
    record = "--record" in args
    names = [a for a in args if not a.startswith("--")]
    failed = 0
    selected = [t for t in tests() if not names or t.name in names]
    for test in selected:
        actual = run(compiler, test)
        expected_path = test / "expected.txt"
        if record:
            expected_path.write_text(actual)
            print(f"recorded {test.name}")
        elif not expected_path.exists() or expected_path.read_text() != actual:
            failed += 1
            print(f"FAIL {test.name}")
            print("\n".join("    " + line for line in actual.splitlines()[:12]))
        else:
            print(f"PASS {test.name}")
    if not record:
        print(f"{len(selected) - failed} passed, {failed} failed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
