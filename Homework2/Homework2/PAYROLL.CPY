      *******  PAYROLL COPY FILE
       FD PAYROLL-FILE
          RECORD CONTAINS 18 CHARACTERS
          RECORDING MODE IS F.
       01 PAYROLL-REC.
          05  PR-EMPNUM        PIC 9(4).
          05  PR-CHKDIG        PIC 9.
          05  PR-SALARY        PIC 9(5)V99     PACKED-DECIMAL.
          05  PR-HOURS         PIC 99V99       PACKED-DECIMAL.
              88 VALID-HOURS           VALUE 0 THRU 60.00.
          05  PR-SHIFT         PIC XX.
          05  PR-PAYCODE       PIC X.
              88 HOURLY                VALUE 'H'.
              88 SALARY                VALUE 'S'.
          05  PR-OVERTIME      PIC 9.
              88 VALID-CODES           VALUE 1 THRU 7.
              88 OVER-CODE             VALUE 3, 4.
          05  PR-EMPCODE       PIC 99.
