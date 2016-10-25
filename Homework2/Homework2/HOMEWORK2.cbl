***********************************************************       
* HOMEWORK #2
* KODY BRAND
* This program reads in payroll data and calculates payroll
* and then generating a report based on the data.
***********************************************************       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. KODY BRAND.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO UT-S-PAYIN 
               ORGANIZATION IS INDEXED
                ACCESS IS SEQUENTIAL
                RECORD KEY IS PR-EMPNUM.
           SELECT PAYROLL-RPT ASSIGN TO UT-S-PAYOUT.
       DATA DIVISION.
       FILE SECTION.
       COPY "PAYROLL.CPY".
       FD  PAYROLL-RPT                                                              
           LABEL RECORDS OMITTED                                                
           RECORDING MODE F.                                                    
       01  OUT-PUT      PIC X(132).
       WORKING-STORAGE SECTION.
       01  END-OF-FILE PIC X           VALUE 'N'.
       77 UT-S-PAYIN   PIC X(50)       
               VALUE 'C:\Projects\CS4230\HW2.DAT'.
       77 UT-S-PAYOUT  PIC X(50)       
               VALUE 'C:\Projects\CS4230\HW2.RPT'.
       77 PAGE-COUNT   PIC 9999        VALUE 1.
       77 WS-R-HOURS   PIC 99V99       PACKED-DECIMAL.
       77 WS-OT-HOURS  PIC 99V99       VALUE 0.
       77 WS-OT-RATE   PIC 99V9        VALUE 0.
       77 WS-RATE      PIC 9(5)V99     VALUE 0.
       77 WS-GROSS     PIC 9(5)V99     VALUE 0.
       77 WS-FED-TAX   PIC 9(5)V99     VALUE 0.
       77 WS-FED-RATE  PIC V999.
       77 WS-NET       PIC 9(5)V99     VALUE 0.
       77 WS-VALID     PIC 9           VALUE 0.                         '0 VALID 1 -INVALID
       77 WS-CHKDIG    PIC 9           value 0.
       01 R-RECORD.
           05  R-EMPNUM        PIC 9(4).
           05  R-CHKDIG        PIC 9.
           05  R-SALARY        PIC 9(5)V99     PACKED-DECIMAL.
           05  R-HOURS         PIC 99V99       PACKED-DECIMAL.
              88 VALID-HOURS           VALUE 0 THRU 60.00.
           05  R-SHIFT         PIC XX.
           05  R-PAYCODE       PIC X.
              88 HOURLY                VALUE 'H'.
              88 SALARY                VALUE 'S'.
           05  R-OVERTIME      PIC 9.
              88 VALID-CODES           VALUE 1 THRU 7.
              88 OVER-CODE             VALUE 3, 4.
           05  R-EMPCODE       PIC 99.
       01  WS-CURRENT-DATE-FIELDS.
             05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR    PIC  9(4).
                 10  WS-CURRENT-MONTH   PIC  9(2).
                 10  WS-CURRENT-DAY     PIC  9(2).
       01 TX-RATE-DATA.
           05  FILLER  PIC X(11)   VALUE '00000100000'.
           05  FILLER  PIC X(11)   VALUE '01010200170'.
           05  FILLER  PIC X(11)   VALUE '02010300190'.
           05  FILLER  PIC X(11)   VALUE '03010400220'.
           05  FILLER  PIC X(11)   VALUE '04010500240'.
           05  FILLER  PIC X(11)   VALUE '05010600255'.
           05  FILLER  PIC X(11)   VALUE '06010700265'.
           05  FILLER  PIC X(11)   VALUE '07010800280'.
           05  FILLER  PIC X(11)   VALUE '08010900295'.
       01 TX-RATE-TABLE REDEFINES TX-RATE-DATA.
           05 TX-RATE OCCURS 9 TIMES INDEXED BY INX-A.
               10 T-LOW    PIC 9(4).
               10 T-HIGH   PIC 9(4).
               10 T-RATE   PIC V999.
       01  HEADER-1.
           05  DATE-M      PIC 99.
           05  FILLER      PIC X       VALUE "/".
           05  DATE-D      PIC 99.
           05  FILLER      PIC X       VALUE "/".
           05  DATE-Y      PIC 9999.
           05  FILLER      PIC X(40).
           05  FILLER      PIC X(19)   VALUE "PLATTEVILLE COMPANY".
           05  FILLER      PIC X(53).
           05  FILLER      PIC X(6)    VALUE "PAGE: ".
           05  FILLER  PIC 9999    VALUE PAGE-COUNT.
       01  HEADER-2.
           05  FILLER      PIC X(10)   VALUE "KODY BRAND".
           05  FILLER      PIC X(43).
           05  FILLER      PIC X(14)   VALUE "PAYROLL REPORT".
       01  RECORD-1.
           05  FILLER      PIC X(2).  
           05  FILLER      PIC X(9)    VALUE "EMPL ID: ".
           05  1-EMPNUM    PIC 9999.
           05  FILLER      PIC X(3).
           05  FILLER      PIC X(6)    VALUE "TYPE: ".
           05  1-PAYCODE   PIC X.
       01  RECORD-2.
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(7)    VALUE "HOURS: ".
           05  2-R-HOURS   PIC 99.99.
           05  FILLER      PIC X(8).
           05  FILLER      PIC X(10)   VALUE "OT HOURS: ".
           05  2-OT-HOURS  PIC 99.99.
           05  FILLER      PIC X(3).
           05  FILLER      PIC X(11)   VALUE "GROSS PAY: ".
           05  2-GROSS     PIC $$$$9.99.
           05  FILLER      PIC X(4).
           05  FILLER      PIC X(9)    VALUE "FED TAX: ".
           05  2-FED-TAX   PIC $$$$9.99.
       01  RECORD-3.
           05  FILLER      PIC X(6).
           05  FILLER      PIC X(6)    VALUE "RATE: ".
           05  3-RATE      PIC $$$$9.99.
           05  FILLER      PIC X(4).
           05  FILLER      PIC X(9)    VALUE "OT RATE: ".
           05  3-OT-RATE   PIC 9.99.
           05  FILLER      PIC X(25).
           05  FILLER      PIC X(11)   VALUE "TAX RATE: %".
           05  3-FED-RATE  PIC .999.
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(10)   VALUE "NET PAY: ".
           05  3-NET       PIC $$$$9.99.
       PROCEDURE DIVISION.
      * Starts the program
       000-MAIN.
           OPEN INPUT PAYROLL-FILE.
           OPEN OUTPUT PAYROLL-RPT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-MONTH TO DATE-M.
           MOVE WS-CURRENT-DAY TO DATE-D.
           MOVE WS-CURRENT-YEAR TO DATE-Y.
           MOVE HEADER-1 TO OUT-PUT
           WRITE OUT-PUT
           MOVE HEADER-2 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINES
           PERFORM 100-READ-RECORD THRU 100-EXIT.
      * Reads in 1 records
       100-READ-RECORD.
           READ PAYROLL-FILE AT END PERFORM 999-CLOSE-UP.
           MOVE PAYROLL-REC TO R-RECORD.
           PERFORM 200-CHECK-RECORD THRU 200-EXIT.
       100-EXIT.
           EXIT.
      * Checks the record using IMB Check Digit sub program    
       200-CHECK-RECORD.
           CALL "CHECK" USING 
               BY CONTENT R-EMPNUM
               BY REFERENCE WS-CHKDIG.
           IF R-CHKDIG = WS-CHKDIG THEN    
               PERFORM 300-PROCESS THRU 300-EXIT
           ELSE    
               PERFORM 350-ERROR THRU 350-EXIT
           END-IF
           PERFORM 100-READ-RECORD THRU 100-EXIT.
       200-EXIT.
           EXIT.
      * Figures out how to handle the records based on type    
       300-PROCESS.
           MOVE R-EMPNUM TO 1-EMPNUM.
           MOVE R-PAYCODE TO 1-PAYCODE.
           MOVE RECORD-1 TO OUT-PUT.
           WRITE OUT-PUT AFTER ADVANCING 2 LINES.
           
           IF R-PAYCODE = 'S' THEN    
               PERFORM 400-SALARY THRU 400-EXIT
           ELSE
               PERFORM 450-HOURLY THRU 450-EXIT
           END-IF.
       300-EXIT.
           EXIT.
      
      * Prints out an error
       350-ERROR.
           MOVE "ERROR VALIDATING EMPLOYEE" TO OUT-PUT.
           WRITE OUT-PUT AFTER ADVANCING 2 LINES.
       350-EXIT.
           EXIT.
      
      * Handles the data if salary type    
       400-SALARY.
           PERFORM 500-SEARCH-TABLE THRU 500-EXIT.
           MOVE R-SALARY TO WS-GROSS.
           COMPUTE WS-FED-TAX = WS-GROSS * WS-FED-RATE
           COMPUTE WS-NET = WS-GROSS - WS-FED-TAX
           MOVE R-HOURS TO 2-R-HOURS
           MOVE ZERO TO 2-OT-HOURS
           MOVE WS-GROSS TO 2-GROSS
           MOVE WS-FED-TAX TO 2-FED-TAX
           MOVE RECORD-2 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           
           MOVE R-SALARY TO 3-RATE
           MOVE ZERO TO 3-OT-RATE
           MOVE WS-FED-RATE TO 3-FED-RATE
           MOVE WS-NET TO 3-NET
           MOVE RECORD-3 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
       400-EXIT.
           EXIT.
      
      * Handles the data if the type is hourly.
       450-HOURLY.
           IF R-HOURS > 40 THEN    
               COMPUTE WS-OT-HOURS = R-HOURS - 40
               MOVE 40 TO WS-R-HOURS
               MOVE 1.63 TO WS-OT-RATE
               MOVE R-SALARY TO WS-RATE
               COMPUTE WS-GROSS = (40 * WS-RATE) 
                       + ( WS-OT-HOURS * WS-OT-RATE)
           END-IF
          IF R-HOURS < 40 THEN
               MOVE R-HOURS TO WS-R-HOURS
               MOVE ZERO TO WS-OT-HOURS
               MOVE R-SALARY TO WS-RATE
               COMPUTE WS-GROSS = WS-R-HOURS * WS-RATE
           END-IF
           PERFORM 500-SEARCH-TABLE THRU 500-EXIT.
           COMPUTE WS-FED-TAX = WS-GROSS * WS-FED-RATE
           COMPUTE WS-NET = WS-GROSS - WS-FED-TAX
           
               MOVE WS-R-HOURS TO 2-R-HOURS
               MOVE WS-OT-HOURS TO 2-OT-HOURS
               MOVE WS-GROSS TO 2-GROSS
               MOVE WS-FED-TAX TO 2-FED-TAX
               MOVE WS-FED-RATE TO 3-FED-RATE
               MOVE RECORD-2 TO OUT-PUT
               WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           MOVE R-SALARY TO 3-RATE.
           MOVE ZERO TO 3-OT-RATE.
           MOVE WS-NET TO 3-NET
           MOVE RECORD-3 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 lines.
    
       450-EXIT.
           EXIT.
      
      * Searches the table for tax rate
       500-SEARCH-TABLE.
           MOVE 0 TO WS-FED-RATE
           SET INX-A TO 1
               SEARCH TX-RATE OF TX-RATE-TABLE
                   WHEN T-LOW(INX-A) < WS-GROSS 
                   AND R-SALARY < T-HIGH(INX-A)
                       MOVE T-RATE(INX-A) TO WS-FED-RATE
               END-SEARCH.
           IF WS-FED-RATE = 0 THEN 
               MOVE .340 TO WS-FED-RATE.
       500-EXIT.
           EXIT.
      
      * Closes up the program files.
       999-CLOSE-UP.
           CLOSE PAYROLL-FILE PAYROLL-RPT.
           STOP RUN.
