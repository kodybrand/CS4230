       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRAVELREPORT.
       AUTHOR. KODY BRAND.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           SELECT TRAVEL-FILE ASSIGN TO UT-S-TRAVEL-IN
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.
           SELECT TRAVEL-REPORT ASSIGN TO UT-S-TRAVEL-RPT.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRAVEL-FILE
           RECORD CONTAINS 77 CHARACTERS.
       01  TRAVEL-REC.
           05  TR-TYPE         PIC 9.
               88  CRUISE          VALUE 1.
               88  AIR             VALUE 2.
               88  TOUR            VALUE 3.
               88  TRAIN           VALUE 4.
               88  OTHER-O         VALUE 5.
           05  TR-C-NUMBER     PIC 9(3).
           05  TR-C-NAME       PIC X(14).
           05  TR-C-ADDRESS    PIC X(40).
           05  TR-COST         PIC 9(5)V99.
           05  TR-C-PHONE      PIC 9(10).
       FD  TRAVEL-REPORT
           REPORT IS TRAVEL-RPT.
           
       
           
       WORKING-STORAGE SECTION.
       77 UT-S-TRAVEL-IN       PIC X(50) VALUE "C:\Cobol\travel.dat".
       77 UT-S-TRAVEL-RPT      PIC X(50) VALUE "C:\Cobol\travel.rpt".
       01  WS-CURRENT-DATE-FIELDS.
             05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR    PIC  9(4).
                 10  WS-CURRENT-MONTH   PIC  9(2).
                 10  WS-CURRENT-DAY     PIC  9(2).
       
       REPORT SECTION.
       RD  TRAVEL-RPT
           CONTROLS ARE FINAL
                       TR-TYPE
           PAGE LIMIT IS 66
           HEADING 1
           FIRST DETAIL 6
           LAST DETAIL 42
           FOOTING 64.
       01 TYPE IS PAGE HEADING.
           05 LINE 1 COLUMN 1          PIC 99 SOURCE WS-CURRENT-MONTH.
           05 LINE 1 COLUMN 3          PIC X   VALUE "/".
           05 LINE 1 COLUMN 4          PIC 99 SOURCE WS-CURRENT-DAY.
           05 LINE 1 COLUMN 6          PIC X   VALUE "/".
           05 LINE 1 COLUMN 7          PIC 99 SOURCE WS-CURRENT-YEAR.
           05 LINE 1 COLUMN 51         PIC X(18) 
                                           VALUE "PLATTEVILLE TRAVEL".
           05 LINE 1 COLUMN 123        PIC X(5) VALUE "PAGE:".
           05 LINE 1 COLUMN 129        PIC Z9 SOURCE PAGE-COUNTER.
           
           05 LINE 2 COLUMN 1          PIC X(10) VALUE "KODY BRAND".
           05 LINE 2 COLUMN 54         PIC X(14) VALUE "TRAVEL REPORT".
           
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT TRAVEL-FILE.
           OPEN OUTPUT TRAVEL-REPORT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.