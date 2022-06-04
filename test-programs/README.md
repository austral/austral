# Test Programs

End-to-end tests of the compiler.

The file hierarchy is:

```
test-programs/
    suites/
        XXX-suite/      # Represents a test suite
            YYY-test/      # Represents a test
                Test.aui   # Module interface
                Test.aum   # Module body
                output.txt # Expected program output, if the program is meant to compile.
                errors.txt # Expected compiler output, if the program is meant to compile.
```

The `suites` directory has numbered subdirectories, each representing a test suite: a collection of tests covering specific compiler features.

Each test directory contains some Austral source files (in the simplest cases, `Test.aui` and `Test.aum`).

If the compilation is expected to fail, there should be a file called `errors.txt` containing the compiler's expected output.

If compilation is expected to succeed and the program is expected to produce text output, there should be a file called `output.txt` containing the program's expected output.

If compilation is expected to succeed but no program output is expected, there's no need for an `output.txt` file.

Suite and test directories are numbered so that tests run in a predictable order.

The `runner.py` script runs the tests.