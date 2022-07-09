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
                error.txt  # Expected compiler output, if the program is meant to compile.
```

The `suites` directory has numbered subdirectories, each representing a test suite: a collection of tests covering specific compiler features.

Each test directory contains some Austral source files (in the simplest cases, `Test.aui` and `Test.aum`) and some control files:

1. `austral-stderr.txt` is the compiler's error message if the test is expected to fail.

2. `program-stdout.txt` is the compiled program's stdout if the compiler is expected to succeed and the program is expected to produce stdout and return normally.

3. If neither file is present, the test is expected to compile and run successfully but produce no output.

Suite and test directories are numbered so that tests run in a predictable order.

The `runner.py` script runs the tests.
