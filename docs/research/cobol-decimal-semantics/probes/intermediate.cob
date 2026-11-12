*> Results that depend on how many digits the intermediate results keep.
IDENTIFICATION DIVISION.
PROGRAM-ID. INTERMEDIATE.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 R   PIC 9V9(6).
01 BIG PIC 9(36) VALUE 999999999999999999999999999999999999.
01 RB  PIC 9(36).
PROCEDURE DIVISION.
    COMPUTE R = 1 / 3 * 3
    DISPLAY "1 / 3 * 3=" R
    COMPUTE R = 1 * 3 / 3
    DISPLAY "1 * 3 / 3=" R
    COMPUTE R = 2 / 3
    DISPLAY "2 / 3=" R
    COMPUTE R ROUNDED = 2 / 3
    DISPLAY "2 / 3 rounded=" R
    COMPUTE R = 1 / 3 + 1 / 3 + 1 / 3
    DISPLAY "1 / 3 + 1 / 3 + 1 / 3=" R
    COMPUTE RB = BIG * BIG / BIG
    DISPLAY "36 nines squared then divided=" RB
    STOP RUN.
