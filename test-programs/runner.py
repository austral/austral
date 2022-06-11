#!/usr/bin/env python3
"""
This script runs the end-to-end tests of the compiler.
"""
import os
import subprocess

# The `test-programs/` directory.
DIR: str = "test-programs/"

def run_all_tests():
    """
    Find and run all the tests.
    """
    # Find the `suites/`` directory.
    suites_dir: str = os.path.join(DIR, "suites")
    # Find the test suites.
    suite_names: list[str] = sorted(os.listdir(suites_dir))
    # Iterate over each test suite.
    for suite_name in suite_names:
        print(f"{suite_name}")
        # Find the tests in this suite.
        suite_dir: str = os.path.join(suites_dir, suite_name)
        test_names: list[str] = sorted([name for name in os.listdir(suite_dir) if name != "README.md"])
        # Iterate over each test.
        for test_name in test_names:
            print(f"\t{test_name.ljust(45)}", end="")
            test_dir: str = os.path.join(suite_dir, test_name)
            # Is there an `error.txt` file?
            if os.path.isfile(os.path.join(test_dir, "error.txt")):
                # If so, the test should fail to compile, and the compiler output must match the contents of the file.
                with open(os.path.join(test_dir, "error.txt"), "r") as stream:
                    expected_errors: str = stream.read().strip()
                    if not expected_errors:
                        print("\nERROR: `error.txt` is empty")
                        exit(-1)
                    compile_failed(suite_name, test_name, expected_errors)
            # Is there an `output.txt` file?
            elif os.path.isfile(os.path.join(test_dir, "output.txt")):
                # If so, the program should compile, run successfully, produce stdout that matches the contents of the file.
                with open(os.path.join(test_dir, "output.txt"), "r") as stream:
                    expected_output: str = stream.read()
                    if not expected_output:
                        print("\nERROR: `output.txt` is empty")
                        exit(-1)
                    compile_successfully(suite_name, test_name, expected_output)
            # There is neither an `error.txt` nor an `output.txt `file.
            else:
                # The program should compile and run successfully and produce no stdout.
                compile_successfully(suite_name, test_name, None)

def compile_successfully(suite_name: str, test_name: str, expected_output):
    """
    Given the name of a suite and a test, compile the code, check that compilation succeeds, and run the program, checking that it terminates successfully and the expected output matches.
    """
    # Find the source files.
    test_dir: str = os.path.join(DIR, "suites", suite_name, test_name)
    interface_path: str = os.path.join(test_dir, "Test.aui")
    body_path: str = os.path.join(test_dir, "Test.aum")
    # Construct the compiler command.
    compile_cmd: list[str] = [
        "./_build/default/bin/austral.exe",
        "compile",
        f"--module={interface_path},{body_path}",
        "--entrypoint=Test:Main",
        "--output=test-programs/output.c",
    ]
    # Call the compiler.
    result: subprocess.CompletedProcess = subprocess.run(compile_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    code: int = result.returncode
    if code == 0:
        # The compiler executed successfully. Compile the program with GCC.
        gcc_cmd: list[str] = [
            "gcc",
            "-fwrapv", # Modular arithmetic semantics
            "-Wno-builtin-declaration-mismatch",
            "lib/prelude.c",
            "test-programs/output.c",
            "-lm", # Math stdlib,
            "-o",
            "test-programs/testbin",
        ]
        # Call GCC.
        result: subprocess.CompletedProcess = subprocess.run(gcc_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        code: int = result.returncode
        if code == 0:
            # GCC compilation succeeded. Run the program.
            result: subprocess.CompletedProcess = subprocess.run(["./test-programs/testbin"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            code: int = result.returncode
            if code == 0:
                # Program ran successfully. Did it produce output?
                stdout: str = result.stdout.decode("utf-8")
                if stdout and expected_output:
                    # Have output, expected output. Check they match.
                    if stdout == expected_output:
                        # Output matches. Pass.
                        print("PASS")
                    else:
                        # Output mismatch. Error.
                        print("\n\n--- BEGIN ERROR ---")
                        print(f"Suite: {suite_name}")
                        print(f"Test: {test_name}")
                        print("Description: program produced stdout, but it was not what we expected.")
                        print("Program output:\n")
                        print("\t--- BEGIN STDOUT ---")
                        print(indent(stdout))
                        print("\t--- END STDOUT ---")
                        print("\nExpected output:\n")
                        print("\t--- BEGIN EXPECTED ---")
                        print(indent(expected_output))
                        print("\t--- END EXPECTED ---")
                        print("--- END ERROR ---")
                        exit(-1)
                elif stdout and (not expected_output):
                    # Have output, did not expect it. Error.
                    print("\n\n--- BEGIN ERROR ---")
                    print(f"Suite: {suite_name}")
                    print(f"Test: {test_name}")
                    print("Description: program produced stdout, but we expected none.")
                    print("Program output:\n")
                    print("\t--- BEGIN STDOUT ---")
                    print(indent(stdout))
                    print("\t--- END STDOUT ---")
                    print("--- END ERROR ---")
                    exit(-1)
                elif (not stdout) and expected_output:
                    # Don't have output, expected it. Error.
                    print("\n\n--- BEGIN ERROR ---")
                    print(f"Suite: {suite_name}")
                    print(f"Test: {test_name}")
                    print("Description: program did not produce stdout, but we expected output.")
                    print("Expected output:\n")
                    print("\t--- BEGIN EXPECTED ---")
                    print(indent(expected_output))
                    print("\t--- END EXPECTED ---")
                    print("--- END ERROR ---")
                    exit(-1)
                else:
                    # Don't have output, didn't expect it. Pass.
                    print("PASS")
            else:
                # Program did not terminate successfully.
                print("\n\n--- BEGIN ERROR ---")
                print(f"Suite: {suite_name}")
                print(f"Test: {test_name}")
                print("Description: program terminated abnormally.")
                print("Command:", " ".join(["./test-programs/testbin"]))
                print("Return code:", code)
                print("Program stdout: \n")
                print("\t--- BEGIN STDOUT ---")
                print(indent(result.stdout.decode("utf-8")))
                print("\t--- END STDOUT ---")
                print("\Program stderr:\n")
                print("\t--- BEGIN STDERR ---")
                print(indent(result.stderr.decode("utf-8")))
                print("\t--- END STDERR ---")
                print("--- END ERROR ---")
                exit(-1)
        else:
            # GCC compilation failed.
            print("\n\n--- BEGIN ERROR ---")
            print(f"Suite: {suite_name}")
            print(f"Test: {test_name}")
            print("Description: GCC compiler failed.")
            print("Command:", " ".join(gcc_cmd))
            print("Generated code in: test-programs/output.c")
            print("Return code:", code)
            print("Compiler stdout: \n")
            print("\t--- BEGIN STDOUT ---")
            print(indent(result.stdout.decode("utf-8")))
            print("\t--- END STDOUT ---")
            print("\nCompiler stderr:\n")
            print("\t--- BEGIN STDERR ---")
            print(indent(result.stderr.decode("utf-8")))
            print("\t--- END STDERR ---")
            print("--- END ERROR ---")
            exit(-1)
    else:
        # Compilation failed: print the output.
        print("\n\n--- BEGIN ERROR ---")
        print(f"Suite: {suite_name}")
        print(f"Test: {test_name}")
        print("Description: Austral compiler failed.")
        print("Command:", " ".join(compile_cmd))
        print("Return code:", code)
        print("Compiler stdout: \n")
        print("\t--- BEGIN STDOUT ---")
        print(indent(result.stdout.decode("utf-8")))
        print("\t--- END STDOUT ---")
        print("\nCompiler stderr:\n")
        print("\t--- BEGIN STDERR ---")
        print(indent(result.stderr.decode("utf-8")))
        print("\t--- END STDERR ---")
        print("--- END ERROR ---")
        exit(-1)
    # At this point, the test has passed. Delete the C code and the test binary.
    os.remove("test-programs/output.c")
    os.remove("test-programs/testbin")

def compile_failed(suite_name: str, test_name: str, expected_errors: str):
    """
    Given the name of a suite and a test, compile the code, check that compilation fails and the compiler produces the expected stderr text.
    """
    # Find the source files.
    test_dir: str = os.path.join(DIR, "suites", suite_name, test_name)
    interface_path: str = os.path.join(test_dir, "Test.aui")
    body_path: str = os.path.join(test_dir, "Test.aum")
    # Construct the compiler command.
    compile_cmd: list[str] = [
        "./_build/default/bin/austral.exe",
        "compile",
        f"--module={interface_path},{body_path}",
        "--entrypoint=Test:Main",
        "--output=test-programs/output.c",
    ]
    # Call the compiler.
    result: subprocess.CompletedProcess = subprocess.run(compile_cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    code: int = result.returncode
    if code == 0:
        # Compilation succeeded.
        print("\n\n--- BEGIN ERROR ---")
        print(f"Suite: {suite_name}")
        print(f"Test: {test_name}")
        print("Description: Austral compiler succeeded, but was expected to fail.")
        print("Command:", " ".join(compile_cmd))
        print("Return code:", code)
        print("Expected stderr: \n")
        print("\t--- BEGIN EXPECTED ---")
        print(indent(expected_errors))
        print("\t--- END EXPECTED ---")
        print("--- END ERROR ---")
        exit(-1)
    else:
        # Compilation failed. Does the actual stderr match expected?
        stderr: str = result.stderr.decode("utf-8").strip()
        if stderr == expected_errors:
            print("PASS")
        else:
            print("\n\n--- BEGIN ERROR ---")
            print(f"Suite: {suite_name}")
            print(f"Test: {test_name}")
            print("Description: Austral compiler failed, but compiler output does not match what we expected.")
            print("Command:", " ".join(compile_cmd))
            print("Return code:", code)
            print("Expected stderr: \n")
            print("\t--- BEGIN EXPECTED ---")
            print(indent(expected_errors))
            print("\t--- END EXPECTED ---")
            print("\nActual stderr: \n")
            print("\t--- BEGIN STDERR ---")
            print(indent(stderr))
            print("\t--- END STDERR ---\n")
            print("--- END ERROR ---")
            exit(-1)

# Utilities

def indent(text: str) -> str:
    return "\n".join(["\t" + line for line in text.split("\n")])

if __name__ == "__main__":
    run_all_tests()
