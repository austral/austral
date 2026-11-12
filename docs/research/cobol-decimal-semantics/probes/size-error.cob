*> When the size error condition is raised, and what the receiver holds.
IDENTIFICATION DIVISION.
PROGRAM-ID. SIZEERROR.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 X PIC 9(3).
01 Y PIC 9(3)V9.
01 Z PIC 9(3).
PROCEDURE DIVISION.
    MOVE 999 TO X
    ADD 1 TO X
    DISPLAY "add 1 to 999 without phrase=" X
    MOVE 999 TO X
    ADD 1 TO X
        ON SIZE ERROR DISPLAY "add 1 to 999 with phrase=size-error, X=" X
        NOT ON SIZE ERROR DISPLAY "add 1 to 999 with phrase=" X
    END-ADD
    MOVE 500 TO X
    COMPUTE X = X * 3
    DISPLAY "500 * 3 without phrase=" X
    MOVE 500 TO X
    COMPUTE X = X * 3
        ON SIZE ERROR DISPLAY "500 * 3 with phrase=size-error, X=" X
    END-COMPUTE
    MOVE 7 TO X
    MOVE 0 TO Z
    DIVIDE X BY Z GIVING X
        ON SIZE ERROR DISPLAY "7 / 0 with phrase=size-error, X=" X
        NOT ON SIZE ERROR DISPLAY "7 / 0 with phrase=" X
    END-DIVIDE
    MOVE 0 TO Y
    COMPUTE Y ROUNDED = 999.96
        ON SIZE ERROR DISPLAY "999.96 rounded into 9(3)V9=size-error, Y=" Y
        NOT ON SIZE ERROR DISPLAY "999.96 rounded into 9(3)V9=" Y
    END-COMPUTE
    MOVE 0 TO Y
    COMPUTE Y = 999.96
        ON SIZE ERROR DISPLAY "999.96 truncated into 9(3)V9=size-error, Y=" Y
        NOT ON SIZE ERROR DISPLAY "999.96 truncated into 9(3)V9=" Y
    END-COMPUTE
    STOP RUN.
