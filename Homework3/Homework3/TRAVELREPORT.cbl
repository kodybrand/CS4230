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
           05  TR-C-NAME       PIC X(15).
           05  TR-C-ADDRESS    PIC X(41).
           05  TR-COST         PIC 9(5)V99.
           05  TR-C-PHONE      PIC X(10).
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
       01  WS-SWITCHES.
           05 WS-EOF-SW                    PIC X(3).
               88  WS-EOF                            VALUE "YES".
       01  WORK-FIELDS.                                                 
           05  WF-RECORD-COUNTER              PIC 99.        
       01  WS-CLIENT.
           05  WS-C-PHONE.
               10  WS-C-PHONE-1     PIC 999.
               10  WS-C-PHONE-2     PIC 999.
               10  WS-C-PHONE-3     PIC 9999.
           05 WS-C-LOCATION.
               10  WS-C-ADDRESS    PIC X(20).
               10  WS-C-CITY       PIC X(20).
               10  WS-C-STATE      PIC X(20).
               10  WS-C-ZIP        PIC X(10).
           05 WS-CLIENT-TOTAL      PIC 9(5)V99     VALUE ZEROS.
           05 WS-CURRENT           PIC 9(3).
       REPORT SECTION.
       RD  TRAVEL-RPT
           CONTROLS ARE FINAL
                       TR-TYPE
                       TR-C-NUMBER
           PAGE LIMIT IS 45
           HEADING 1
           FIRST DETAIL 8
           LAST DETAIL 42
           FOOTING 45.
       01 TYPE IS PAGE HEADING.
           05 LINE 1.
               10 COLUMN 1          PIC 99 SOURCE WS-CURRENT-MONTH.
               10 COLUMN 3          PIC X   VALUE "/".
               10 COLUMN 4          PIC 99 SOURCE WS-CURRENT-DAY.
               10 COLUMN 6          PIC X   VALUE "/".
               10 COLUMN 7          PIC 99 SOURCE WS-CURRENT-YEAR.
               10 COLUMN 51         PIC X(18) 
                                           VALUE "PLATTEVILLE TRAVEL".
               10 COLUMN 123        PIC X(5) VALUE "PAGE:".
               10 COLUMN 129        PIC Z9 SOURCE PAGE-COUNTER.
           
           05 LINE 2.
               10 COLUMN 1         PIC X(10)   VALUE "KODY BRAND".
               10 COLUMN 54        PIC X(14)   VALUE "TRAVEL REPORT".
               
           05 LINE 4.
               10 COLUMN 1         PIC X(5)    VALUE "TYPE:".
               10 COLUMN 7         PIC X(10)   SOURCE TR-TYPE.
               
           05 LINE 6.
               10 COLUMN 4         PIC X(11)   VALUE "CLIENT NAME".
               10 COLUMN 22        PIC X(7)    VALUE "ADDRESS".
               10 COLUMN 48        PIC X(12)   VALUE "PHONE NUMBER".
               10 COLUMN 65        PIC X(12)   VALUE "CLIENT TOTAL".
       01  DETAIL-LINE
           TYPE IS DETAIL
           LINE IS PLUS 1.
           05 COLUMN 4             PIC X(15)   SOURCE TR-C-NAME
                                                   GROUP INDICATE.
           05 COLUMN 22            PIC X(20)   SOURCE WS-C-ADDRESS
                                                       GROUP INDICATE.
           05 COLUMN 43            PIC XX      SOURCE WS-C-STATE
                                                       GROUP INDICATE.
           05 COLUMN 48            PIC X       VALUE "("
                                                       GROUP INDICATE.
           05 COLUMN 49            PIC 999     SOURCE WS-C-PHONE-1
                                                       GROUP INDICATE.
           05 COLUMN 53            PIC 999     SOURCE WS-C-PHONE-2
                                                       GROUP INDICATE.
           05 COLUMN 52            PIC X       VALUE ")"     
                                                       GROUP INDICATE.
           05 COLUMN 56            PIC X       VALUE "-"
                                                       GROUP INDICATE.  
           05 COLUMN 57            PIC 9999    SOURCE WS-C-PHONE-3
                                                       GROUP INDICATE.
           05 COLUMN 65            PIC $$$$9.99 SOURCE TR-COST.  
       
       01  ITEM-TOTAL-GROUP TYPE IS CONTROL FOOTING TR-C-NUMBER         
           LINE IS PLUS 1.
           05  COLUMN 70           PIC $$$$9.99 SOURCE WS-CLIENT-TOTAL.
       PROCEDURE DIVISION.
       000-MAIN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           PERFORM 100-INIT-RTN THRU 100-INIT-RTN-EXIT.
           INITIATE TRAVEL-RPT.
           PERFORM UNTIL WS-EOF
               READ TRAVEL-FILE
                   AT END MOVE "YES" TO WS-EOF-SW
                   NOT AT END
                       PERFORM 200-PROCESS-RTN THRU 200-EXIT
               END-READ
           END-PERFORM.
           TERMINATE TRAVEL-RPT.
           CLOSE TRAVEL-FILE
                 TRAVEL-REPORT.
           STOP RUN.
           
       100-INIT-RTN.
           OPEN    INPUT TRAVEL-FILE
                   OUTPUT TRAVEL-REPORT
           MOVE "NO" TO WS-EOF-SW.
           MOVE 1 TO WF-RECORD-COUNTER.
       100-INIT-RTN-EXIT.
           EXIT.
           
       200-PROCESS-RTN.
           IF WS-CURRENT NOT EQUAL TR-C-NUMBER
               MOVE ZEROS TO WS-CLIENT-TOTAL
           END-IF
           ADD TR-COST TO WS-CLIENT-TOTAL.
           PERFORM 300-FORMAT THRU 300-EXIT.
           MOVE TR-C-NUMBER TO WS-CURRENT.
           GENERATE DETAIL-LINE. 
       200-EXIT.
           EXIT.
           
       300-FORMAT.
           INITIALIZE WS-C-LOCATION.
           UNSTRING TR-C-ADDRESS DELIMITED BY ','
               INTO    WS-C-ADDRESS
                       WS-C-CITY
                       WS-C-STATE
                       WS-C-ZIP
           END-UNSTRING.
           UNSTRING TR-C-PHONE
               INTO    WS-C-PHONE-1,
                       WS-C-PHONE-2,
                       WS-C-PHONE-3
           END-UNSTRING.
       300-EXIT.
           EXIT.