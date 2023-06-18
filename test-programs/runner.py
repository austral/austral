#!/usr/bin/env python3
# Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
# See LICENSE file for details.
#
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
"""
This script runs the end-to-end tests of the compiler.
"""
import os
import subprocess

#
# Constants
#

# The `test-programs/` directory.
DIR: str = "test-programs/"


#
# Error Reporting
#


def die(message: str):
    print(message)
    exit(-1)


def report(properties, outputs):
    print("\n\n--- BEGIN ERROR ---")
    for (name, value) in properties:
        print(f"{name}: {value}")
    for (name, value) in outputs:
        print(f"--- BEGIN {name} ---")
        print(value)
        print(f"--- END {name} ---\n")
    print("--- END ERROR ---")
    exit(-1)


#
# Classes
#


class Test(object):
    """
    Base class of test objects.
    """
    pass


class TestSuccess(Test):
    """
    A test that is expected to succeed.
    """
    def __init__(self, name, suite_name, directory, cli, expected_output):
        self.name = name
        self.suite_name = suite_name
        self.directory = directory
        self.cli = cli
        self.expected_output = expected_output


class TestFailure(Test):
    """
    A test that is expected to fail when compiling the code.
    """
    def __init__(self, name, suite_name, directory, cli, expected_compiler_error):
        self.name = name
        self.suite_name = suite_name
        self.directory = directory
        self.cli = cli
        self.expected_compiler_error = expected_compiler_error


class TestProgramFailure(Test):
    """
    A test that is expected to compile successfully, but the compiled program
    should return a failure exit code, and print to stderr.
    """
    def __init__(self, name, suite_name, directory, cli, expected_program_stderr):
        self.name = name
        self.suite_name = suite_name
        self.directory = directory
        self.cli = cli
        self.expected_program_stderr = expected_program_stderr


class Suite(object):
    def __init__(self, name, tests):
        self.name = name
        self.tests = tests


#
# Collection
#


def collect_suites() -> list:
    """
    Get the list of all test suites.
    """
    # Result aggregator.
    suites: list = []
    # Find the `suites/`` directory.
    suites_dir: str = os.path.join(DIR, "suites")
    # Find the test suites.
    suite_names: list = sorted(os.listdir(suites_dir))
    # Iterate over each test suite.
    for suite_name in suite_names:
        # Find the tests in this suite.
        suite_dir: str = os.path.join(suites_dir, suite_name)
        test_names: list = sorted(
            [name for name in os.listdir(suite_dir) if name != "README.md"]
        )
        tests: list = []
        # Iterate over each test.
        for test_name in test_names:
            test_dir: str = os.path.join(suite_dir, test_name)
            expected_error = _get_file_contents(test_dir, "austral-stderr.txt")
            expected_output = _get_file_contents(test_dir, "program-stdout.txt")
            program_stderr = _get_file_contents(test_dir, "program-stderr.txt")
            cli = _get_file_contents(test_dir, "cli.txt")
            if (expected_error is not None) and (expected_output is not None):
                die(
                    "Can't have both `austral-stderr.txt` and `program-stdout.txt` in the same test."
                )
            elif (expected_error is not None) and (expected_output is None):
                # The test should fail to compile, and the compiler output must
                # match the contents of the file.
                tests.append(
                    TestFailure(
                        name=test_name,
                        suite_name=suite_name,
                        directory=test_dir,
                        cli=cli,
                        expected_compiler_error=expected_error,
                    )
                )
            elif (expected_error is None) and (expected_output is not None):
                # The program should compile, run successfully, produce stdout
                # that matches the contents of the file.
                tests.append(
                    TestSuccess(
                        name=test_name,
                        suite_name=suite_name,
                        directory=test_dir,
                        cli=cli,
                        expected_output=expected_output,
                    )
                )
            # There is neither a `austral-stderr.txt` nor an `program-stdout.txt `file.
            else:
                if program_stderr is None:
                    # The program should compile and run successfully and produce no
                    # stdout.
                    tests.append(
                        TestSuccess(
                            name=test_name,
                            suite_name=suite_name,
                            directory=test_dir,
                            cli=cli,
                            expected_output=None,
                        )
                    )
                else:
                    # The program should compile successfully, return a failure
                    # exit code, and print stderr.
                    tests.append(
                        TestProgramFailure(
                            name=test_name,
                            suite_name=suite_name,
                            directory=test_dir,
                            cli=cli,
                            expected_program_stderr=program_stderr,
                        )
                    )
                    # Add the suite.
        suites.append(
            Suite(
                name=suite_name,
                tests=tests,
            )
        )
    return suites


def _get_file_contents(test_dir: str, filename: str):
    if os.path.isfile(os.path.join(test_dir, filename)):
        with open(os.path.join(test_dir, filename), "r") as stream:
            data: str = stream.read().strip()
            if not data:
                die(f"`{filename}` exists, but it is empty.")
            return data
    else:
        return None


#
# Test Execution
#


def run_test(test: Test, replace_stderr: bool):
    if isinstance(test, TestSuccess):
        _run_success_test(test)
    elif isinstance(test, TestFailure):
        _run_failure_test(test, replace_stderr)
    elif isinstance(test, TestProgramFailure):
        _run_program_failure_test(test)
    else:
        die("Unknown test type.")


def _test_cmd(test: Test) -> list:
    if test.cli:
        cli: str = test.cli.replace("$DIR", test.directory)
        return cli.split(" ")
    else:
        body_path: str = os.path.join(test.directory, "Test.aum")
        return [
            "./austral",
            "compile",
            f"{body_path}",
            "--entrypoint=Test:main",
            "--target-type=c",
            "--output=test-programs/output.c",
            "--error-format=json",
        ]


def _run_success_test(test: Test):
    # Find the source files.
    expected_output = test.expected_output
    suite_name: str = test.suite_name
    test_name: str = test.name
    # Construct the compiler command.
    compile_cmd: list = _test_cmd(test)
    # Call the compiler.
    result: subprocess.CompletedProcess = subprocess.run(
        compile_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    code: int = result.returncode
    if code != 0:
        # Compilation failed: print the output.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                ("Description", "Austral compiler failed."),
                ("Command", " ".join(compile_cmd)),
                ("Return code", code),
            ],
            outputs=[
                ("COMPILER STDOUT", result.stdout.decode("utf-8")),
                ("STDERR", result.stderr.decode("utf-8")),
            ],
        )
    # The compiler executed successfully. Compile the program with GCC.
    gcc_cmd: list = [
        "gcc",
        "-fwrapv",  # Modular arithmetic semantics
        "-Wno-builtin-declaration-mismatch",
        "test-programs/output.c",
        "-lm",  # Math stdlib,
        "-o",
        "test-programs/testbin",
    ]
    # Call GCC.
    result: subprocess.CompletedProcess = subprocess.run(
        gcc_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    code: int = result.returncode
    if code != 0:
        # GCC compilation failed.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                ("Description", "GCC compiler failed."),
                ("Command", " ".join(gcc_cmd)),
                ("Return code", code),
            ],
            outputs=[
                ("GCC STDOUT", result.stdout.decode("utf-8")),
                ("GCC STDERR", result.stderr.decode("utf-8")),
            ],
        )
    # GCC compilation succeeded. Run the program.
    result: subprocess.CompletedProcess = subprocess.run(
        ["./test-programs/testbin"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    code: int = result.returncode
    if code != 0:
        # Program did not terminate successfully.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                ("Description", "program terminated abnormally."),
                ("Command", " ".join(["./test-programs/testbin"])),
                ("Return code", code),
            ],
            outputs=[
                ("STDOUT", result.stdout.decode("utf-8")),
                ("STDERR", result.stderr.decode("utf-8")),
            ],
        )
    # Program ran successfully. Did it produce output?
    stdout: str = result.stdout.decode("utf-8").strip()
    if stdout and expected_output:
        # Have output, expected output. Check they match.
        if stdout == expected_output:
            # Output matches. Pass.
            print("PASS")
        else:
            # Output mismatch. Error.
            report(
                properties=[
                    ("Suite", suite_name),
                    ("Test", test_name),
                    (
                        "Description",
                        "program produced stdout, but it was not " "what we expected.",
                    ),
                ],
                outputs=[
                    ("ACTUAL STDOUT", stdout),
                    ("EXPECTED STDOUT", expected_output),
                ],
            )
    elif stdout and (not expected_output):
        # Have output, did not expect it. Error.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                (
                    "Description",
                    "program produced stdout, but we expected " "none.",
                ),
            ],
            outputs=[
                ("STDOUT", stdout),
            ],
        )
    elif (not stdout) and expected_output:
        # Don't have output, expected it. Error.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                (
                    "Description",
                    "did not produce stdout, but we expected output.",
                ),
            ],
            outputs=[
                ("EXPECTED OUTPUT", expected_output),
            ],
        )
    else:
        # Don't have output, didn't expect it. Pass.
        print("PASS")
    # At this point, the test has passed. Delete the C code and the test binary.
    os.remove("test-programs/output.c")
    os.remove("test-programs/testbin")

def trim_lines(text):
    return "\n".join([line if line.strip() else "" for line in text.strip().split("\n")])

def _run_failure_test(test: TestFailure, replace_stderr: bool):
    suite_name: str = test.suite_name
    test_name: str = test.name
    # Construct the compiler command.
    compile_cmd: list = _test_cmd(test)
    # Call the compiler.
    result: subprocess.CompletedProcess = subprocess.run(
        compile_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    code: int = result.returncode
    if code == 0:
        # Compilation succeeded.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                (
                    "Description",
                    "Austral compiler succeeded, but was expected to fail.",
                ),
                ("Command", " ".join(compile_cmd)),
                ("Return code", code),
            ],
            outputs=[
                ("EXPECTED STDERR", test.expected_compiler_error),
            ],
        )
    # Compilation failed. Does the actual stderr match expected?
    if replace_stderr:
        # If replace_stderr is true, just take the actual stderr and overwrite the expected stderr.
        stderr: str = trim_lines(result.stderr.decode("utf-8"))
        with open(test.directory + "/austral-stderr.txt", "w") as stream:
            stream.write(stderr)
    else:
        stderr: str = trim_lines(result.stderr.decode("utf-8"))
        if stderr != trim_lines(test.expected_compiler_error):
            report(
                properties=[
                    ("Suite", suite_name),
                    ("Test", test_name),
                    (
                        "Description",
                        "Austral compiler failed, but compiler output does not "
                        "match what we expected.",
                    ),
                    ("Command", " ".join(compile_cmd)),
                    ("Return code", code),
                ],
                outputs=[
                    ("EXPECTED STDERR", test.expected_compiler_error),
                    ("ACTUAL STDERR", stderr),
                ],
            )
    # Compilation failed and output matches.
    print("PASS")


def _run_program_failure_test(test: Test):
    # Find the source files.
    expected_stderr: str = test.expected_program_stderr
    suite_name: str = test.suite_name
    test_name: str = test.name
    # Construct the compiler command.
    compile_cmd: list = _test_cmd(test)
    # Call the compiler.
    result: subprocess.CompletedProcess = subprocess.run(
        compile_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    code: int = result.returncode
    if code != 0:
        # Compilation failed: print the output.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                ("Description", "Austral compiler failed."),
                ("Command", " ".join(compile_cmd)),
                ("Return code", code),
            ],
            outputs=[
                ("COMPILER STDOUT", result.stdout.decode("utf-8")),
                ("STDERR", result.stderr.decode("utf-8")),
            ],
        )
    # The compiler executed successfully. Compile the program with GCC.
    gcc_cmd: list = [
        "gcc",
        "-fwrapv",  # Modular arithmetic semantics
        "-Wno-builtin-declaration-mismatch",
        "test-programs/output.c",
        "-lm",  # Math stdlib,
        "-o",
        "test-programs/testbin",
    ]
    # Call GCC.
    result: subprocess.CompletedProcess = subprocess.run(
        gcc_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    code: int = result.returncode
    if code != 0:
        # GCC compilation failed.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                ("Description", "GCC compiler failed."),
                ("Command", " ".join(gcc_cmd)),
                ("Return code", code),
            ],
            outputs=[
                ("GCC STDOUT", result.stdout.decode("utf-8")),
                ("GCC STDERR", result.stderr.decode("utf-8")),
            ],
        )
    # GCC compilation succeeded. Run the program.
    result: subprocess.CompletedProcess = subprocess.run(
        ["./test-programs/testbin"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    code: int = result.returncode
    if code == 0:
        # Program did terminated successfully, but we didn't want that.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                (
                    "Description",
                    "program terminated successfully, but we expected a failure.",
                ),
                ("Command", " ".join(["./test-programs/testbin"])),
                ("Return code", code),
            ],
            outputs=[
                ("STDOUT", result.stdout.decode("utf-8")),
                ("STDERR", result.stderr.decode("utf-8")),
            ],
        )
    # Did it produce output?
    stderr: str = result.stderr.decode("utf-8").strip()
    if stderr:
        # Have stderr, expected stderr. Check they match.
        if stderr == expected_stderr:
            # Output matches. Pass.
            print("PASS")
        else:
            # Output mismatch. Error.
            report(
                properties=[
                    ("Suite", suite_name),
                    ("Test", test_name),
                    (
                        "Description",
                        "program produced stderr, but it was not what we expected.",
                    ),
                ],
                outputs=[
                    ("ACTUAL STDERR", stderr),
                    ("EXPECTED STDERR", expected_stderr),
                ],
            )
    else:
        # Don't have output, expected it. Error.
        report(
            properties=[
                ("Suite", suite_name),
                ("Test", test_name),
                (
                    "Description",
                    "did not produce stderr, but we expected stderr.",
                ),
            ],
            outputs=[
                ("EXPECTED STDERR", expected_stderr),
            ],
        )
    # At this point, the test has passed. Delete the C code and the test binary.
    os.remove("test-programs/output.c")
    os.remove("test-programs/testbin")


#
# Test Runner
#


def run_all_tests(suites: list, suite_pattern: str = "", name_pattern: str = "", replace_stderr: bool = False):
    """
    Run the given suites.

    If suite_pattern is given, only tests in suites containing the given string are considered.
    If name_pattern is given, only tests that have names containing the given string are run.
    An empty pattern means match all.
    """
    for suite in suites:
        if len(suite_pattern) == 0 or suite.name.find(suite_pattern) != -1:
            print(suite.name)
            for test in suite.tests:
                if len(name_pattern) == 0 or test.name.find(name_pattern) != -1:
                    print(f"\t{test.name.ljust(45)}", end="")
                    run_test(test, replace_stderr)


#
# Entrypoint
#

if __name__ == "__main__":
    import sys

    replace_stderr: bool = "--replace-stderr" in sys.argv
    args: list[str] = [arg for arg in sys.argv if not arg.startswith("--")]
    suite_pattern = args[1] if len(args) > 1 else ""
    name_pattern = args[2] if len(args) > 2 else ""
    run_all_tests(collect_suites(), suite_pattern, name_pattern, replace_stderr)
