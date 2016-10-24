       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. KODY BRAND.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO UT-S-PAYIN 
               ORGANIZATION IS SEQUENTIAL.
           SELECT PAYROLL-RPT ASSIGN TO UT-S-PAYOUT.
       DATA DIVISION.
       FILE SECTION.
       COPY "PAYROLL.CPY".
       FD  PAYROLL-RPT                                                              
           LABEL RECORDS OMITTED                                                
           RECORDING MODE F.                                                    
       01  OUT-PUT      PIC X(132).
       WORKING-STORAGE SECTION.
       01  END-OF-FILE PIC X.
       77 UT-S-PAYIN   PIC X(50)       VALUE 'C:\CS4230\HW2.DAT'.
       77 UT-S-PAYOUT  PIC X(50)       VALUE 'C:\CS4230\HW2.RPT'.
       77 PAGE-COUNT   PIC 9999        VALUE 1.
       77 WS-R-HOURS   PIC 99V99       VALUE 0.
       77 WS-OT-HOURS  PIC 99V99       VALUE 0.
       77 WS-OT-RATE   PIC 99V9        VALUE 0.
       77 WS-RATE      PIC 9(5)V99     VALUE 0.
       77 WS-GROSS     PIC 9(5)V99     VALUE 0.
       01  WS-CURRENT-DATE-FIELDS.
             05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR    PIC  9(4).
                 10  WS-CURRENT-MONTH   PIC  9(2).
                 10  WS-CURRENT-DAY     PIC  9(2).
       01 TX-RATE-DATA.
           05  FILLER  PIC X(7)   VALUE '0100000'.
           05  FILLER  PIC X(7)   VALUE '0200170'.
           05  FILLER  PIC X(7)   VALUE '0300190'.
           05  FILLER  PIC X(7)   VALUE '0400220'.
           05  FILLER  PIC X(7)   VALUE '0500240'.
           05  FILLER  PIC X(7)   VALUE '0600255'.
           05  FILLER  PIC X(7)   VALUE '0700265'.
           05  FILLER  PIC X(7)   VALUE '0800280'.
           05  FILLER  PIC X(7)   VALUE '0900295'.
           05  FILLER  PIC X(7)   VALUE '1200315'.
       01 TX-RATE-TABLE REDEFINES TX-RATE-DATA.
           05 TX-RATE OCCURS 9 TIMES INDEXED BY INX-A.
               10 T-PAY    PIC 9(4).
               10 T-RATE   PIC 99.9.
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
           05  PAGE-COUNT  PIC 9999.
       01  HEADER-2.
           05  FILLER      PIC X(10)   VALUE "KODY BRAND".
           05  FILLER      PIC X(43).
           05  FILLER      PIC X(14)   VALUE "PAYROLL REPORT".
       01  RECORD-1.
           05  FILLER      PIC X(2).  
           05  FILLER      PIC X(9)    VALUE "EMPL ID: ".
           05  R-EMPNUM    PIC 9999.
           05  FILLER      PIC X(3).
           05  FILLER      PIC X(6)    VALUE "TYPE: ".
           05  PR-PAYCODE  PIC X.
       01  RECORD-2.
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(7)    VALUE "HOURS: ".
           05  WS-R-HOURS  PIC 99V99.
           05  FILLER      PIC X(8).
           05  FILLER      PIC X(10)   VALUE "OT HOURS: ".
           05  WS-OT-HOURS PIC 99.99.
           05  FILLER      PIC X(3).
           05  FILLER      PIC X(11)   VALUE "GROSS PAY: ".
           05  WS-GROSS    PIC $$$$9.99.
           05  FILLER      PIC X(4).
           05  FILLER      PIC X(9)    VALUE "FED TAX: ".
           05  WS-FED-TAX  PIC $$$$9.99.
       01  RECORD-3.
           05  FILLER      PIC X(6).
           05  FILLER      PIC X(6)    VALUE "RATE: ".
           05  WS-RATE     PIC $$$$9.99.
           05  FILLER      PIC X(4).
           05  FILLER      PIC X(9)    VALUE "OT RATE: ".
           05  WS-OT-RATE  PIC 9.99.
           05  FILLER      PIC X(25).
           05  FILLER      PIC X(11)   VALUE "TAX RATE: %".
           05  WS-FED-RATE PIC 99.99.
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(10)   VALUE "NET PAY: ".
           05  WS-NET      PIC $$$$9.99.
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT PAYROLL-FILE.
           OPEN OUTPUT PAYROLL-RPT.
           READ PAYROLL-FILE INTO PAYROLL-REC.
           MOVE PR-EMPNUM TO OUT-PUT.
           WRITE OUT-PUT.
           CLOSE PAYROLL-FILE PAYROLL-RPT.

           
          
           