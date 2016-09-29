***********************************************************
* HOMEWORK #1 Part 2
* This program takes in data from the BANKFILE.DAT and 
* proceses the records. Depending on the bank account
* it will debit or credit it. It will then produce a report
* at BANKREPXXX.RPT. 
***********************************************************
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. BANK.                                                        
       AUTHOR. Kody Brand.
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-Z.                                              
       OBJECT-COMPUTER. IBM-Z.                                               
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
            SELECT BANKIN  ASSIGN TO UT-S-BANKING
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT BANKOUT ASSIGN TO UT-S-BANKOUT.
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  BANKIN                                                               
           LABEL RECORDS OMITTED                                                
           RECORDING MODE F.                                                    
       01  INPUT-RECORD.                                                        
           05  KODE     PIC X.                                               
           05  FILLER   PIC X.                                                 
           05  CUSNAME  PIC X(21).                                             
           05  FILLER   PIC X.                                                
           05  ANUM     PIC X(5).
           05  FILLER   PIC X(10).                                            
           05  DEP      PIC 9(4)V99.                                           
           05  WITHH    PIC 9(4)V99.                                          
       FD  BANKOUT                                                              
           LABEL RECORDS OMITTED                                                
           RECORDING MODE F.                                                    
       01  OUT-PUT      PIC X(132).  
       WORKING-STORAGE SECTION.                                                 
       77  UT-S-BANKING PIC X(50) VALUE 'C:\CS4230\BANKFILE.DAT'.
       77  UT-S-BANKOUT PIC X(50) VALUE 'C:\CS4230\BANKREPXXX.RPT'.
       77  CUR-ACCT     PIC X(5).
       77  CUR-NAME     PIC X(21). 
       77  CUR-TYPE     PIC X.
       77  FIRST-RECORD PIC 9      VALUE 1.
       77  FIRST-RUN    PIC 9      VALUE 1.
       77  A-BALANCE    PIC S9(4)V99    VALUE 0.
       77  A-W          PIC S9(4)V99    VALUE 0.
       77  A-D          PIC S9(4)V99    VALUE 0.
       77  A-F          PIC S9(4)V99    VALUE 0.
       77  T-FEE        PIC S9(4)V99    VALUE 0.
       77  H-B          PIC 9(7)V99     VALUE 0.
       77  H-B-N        PIC X(16).
       77  H-M          PIC 9(7)V99     VALUE 0.
       77  H-M-N        PIC X(16).
       77  ACT-COUNT    PIC 9(3)        VALUE 0.
       77  ACT-TOTAL    PIC 9(7)V99     VALUE 0.
       77  ACT-AVG      PIC 9(7)V99     VALUE 0.
       77  T-INT        PIC 9(7)V99     VALUE 0.
       01  WS-CURRENT-DATE-FIELDS.
             05  WS-CURRENT-DATE.
                 10  WS-CURRENT-YEAR    PIC  9(4).
                 10  WS-CURRENT-MONTH   PIC  9(2).
                 10  WS-CURRENT-DAY     PIC  9(2).
       01  HEADER-1.
           05  DATE-M   PIC 99.  
           05  FILLER   PIC X     VALUE "/".
           05  DATE-D   PIC 99.
           05  FILLER   PIC X     VALUE "/".
           05  DATE-Y   PIC 9999.
           05  FILLER   PIC X(25).
           05  FILLER   PIC X(17) VALUE "BANK OF WISCONSIN".
           05  FILLER   PIC X(28).
           05  FILLER   PIC X(5)   VALUE "PAGE ".
           05  H-PAGE   PIC 9(3)   VALUE 001.
       01  HEADER-2.                               
           05  FILLER   PIC X(80).
           05  FILLER   PIC X(10)   VALUE "KODY BRAND".
       01  ACT-HEADER-1.
           05  FILLER      PIC X(15)   VALUE "ACCOUNT CONTACT".
           05  FILLER      PIC X(8).
           05  FILLER      PIC X(4)    VALUE "TYPE".
           05  FILLER      PIC X(2).
           05  FILLER      PIC X(17)   VALUE "BEGINNING BALANCE".
           05  FILLER      PIC X(3).
           05  FILLER      PIC X(12)   VALUE "WIDTHDRAWALS".
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(8)   VALUE "DEPOSITS".
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(4)    VALUE "FEES".
       01  ACT-HEADER-2.
           05  ACT-NAME    PIC X(16).
           05  FILLER      PIC X(7).
           05  ACT-TYPE    PIC X.
           05  FILLER      PIC X(3).
           05  ACT-BEG     PIC $,$$$,$$9.99.
       01  ACT-REC.
           05  FILLER      PIC X(2).
           05  ACT-ERROR   PIC X(20).
           05  FILLER      PIC X(27).
           05  ACT-WID     PIC $,$$$,$$9.99.
           05  FILLER      PIC X(1).
           05  ACT-DEB     PIC $,$$$,$$9.99.
           05  FILLER      PIC X(1).
           05  ACT-FEE     PIC $,$$$,$$9.99.
       01  ACT-FOOTER-1.
           05  FILLER      PIC X(15)   VALUE "ACCOUNT CONTACT".
           05  FILLER      PIC X(8).
           05  FILLER      PIC X(4)    VALUE "TYPE".
           05  FILLER      PIC X(2).
           05  FILLER      PIC X(17)   VALUE "BEGINNING BALANCE".
           05  FILLER      PIC X(3).
           05  FILLER      PIC X(12)   VALUE "WIDTHDRAWALS".
           05  FILLER      PIC X(5).
           05  FILLER      PIC X(8)    VALUE "DEPOSITS".
           05  FILLER      PIC X(6).
           05  FILLER      PIC X(10)    VALUE "TOTAL FEES".
           05  FILLER      PIC X(6).
           05  FILLER      PIC X(14)    VALUE "EARNED INTREST".
           05  FILLER      PIC X(2).
           05  FILLER      PIC X(14)    VALUE "ENDING BALANCE".
       01  ACT-FOOTER-2.
           05  ACT-NAME    PIC X(16).
           05  FILLER      PIC X(7).
           05  ACT-TYPE    PIC X.
           05  FILLER      PIC X(3).
           05  ACT-BEG     PIC $,$$$,$$9.99.
           05  FILLER      PIC X(10).
           05  ACT-W-TOT   PIC $,$$$,$$9.99.
           05  FILLER      PIC X(1).
           05  ACT-D-TOT   PIC $,$$$,$$9.99.
           05  FILLER      PIC X(4).
           05  ACT-F-TOT   PIC $,$$$,$$9.99.
           05  FILLER      PIC X(6).
           05  ACT-I-TOT   PIC $,$$$,$$9.99.
           05  FILLER      PIC X(4).
           05  ACT-E-TOT   PIC +$,$$$,$$9.99.
       01  END-PRINT-1.
           05  FILLER      PIC X(15)   VALUE "HIGHEST BALANCE".
           05  FILLER      PIC X(8).
           05  FILLER      PIC X(12)   VALUE "MONEY MARKET".
           05  FILLER      PIC X(11).
           05  FILLER      PIC X(15)   VALUE "AVERAGE BALANCE".
       01  END-PRINT-2.
           05  HI-B-NAME   PIC X(16).
           05  FILLER      PIC X(7).
           05  HI-M-NAME   PIC X(16).
           05  FILLER      PIC X(2).
           05  HI-AVG      PIC $,$$$,$$9.99.
       01  END-PRINT-3.
           05  HI-B-AMT    PIC $,$$$,$$9.99.
           05  FILLER      PIC X(10).
           05  HI-M-AMT    PIC $,$$$,$$9.99.
       01  BG-BAL-DATA.
           05  FILLER     PIC X(12) VALUE "C10030035293".
           05  FILLER     PIC X(12) VALUE "C20150014000".
           05  FILLER     PIC X(12) VALUE "C20990043286".
           05  FILLER     PIC X(12) VALUE "C23020101415".
           05  FILLER     PIC X(12) VALUE "M10030069245".
           05  FILLER     PIC X(12) VALUE "M10960768840".
           05  FILLER     PIC X(12) VALUE "M15080264533".
           05  FILLER     PIC X(12) VALUE "M35100405087".
           05  FILLER     PIC X(12) VALUE "M81522947865".
           05  FILLER     PIC X(12) VALUE "S15091005699".
           05  FILLER     PIC X(12) VALUE "S15100028580".
           05  FILLER     PIC X(12) VALUE "S19000013183".
           05  FILLER     PIC X(12) VALUE "S22190032162".
           05  FILLER     PIC X(12) VALUE "S34440015301".
           05  FILLER     PIC X(12) VALUE "S49020075165".
           05  FILLER     PIC X(12) VALUE "S60690702424".
       01  BG-BAL-TABLE REDEFINES BG-BAL-DATA.
           05  BG-BAL OCCURS 16 TIMES INDEXED BY INX-A.
              10  T-ACCT   PIC X(5).
              10  T-BAL    PIC 9(5)V99.
       PROCEDURE DIVISION.                                                      
       MAIN.      
           OPEN INPUT BANKIN.
           OPEN OUTPUT BANKOUT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-MONTH TO DATE-M.
           MOVE WS-CURRENT-DAY TO DATE-D.
           MOVE WS-CURRENT-YEAR TO DATE-Y.
           MOVE HEADER-1 TO OUT-PUT.
           WRITE OUT-PUT.
           MOVE HEADER-2 TO OUT-PUT.
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
       READ-RECORD.
           READ BANKIN AT END PERFORM CLOSEUP.
           IF ANUM = CUR-ACCT PERFORM SAME-ACCOUNT THRU E-SAME-ACCOUNT
           ELSE
               PERFORM GET-INTREST THRU E-INTREST
               IF FIRST-RUN = 0
                   MOVE ACT-FOOTER-1 TO OUT-PUT
                   WRITE OUT-PUT AFTER ADVANCING 1 LINE
                   MOVE ACT-FOOTER-2 TO OUT-PUT
                   MOVE CUR-NAME TO ACT-NAME OF ACT-FOOTER-2
                   MOVE CUR-TYPE TO ACT-TYPE OF ACT-FOOTER-2
                   MOVE A-BALANCE TO ACT-E-TOT
                   MOVE A-D TO ACT-D-TOT
                   MOVE A-W TO ACT-W-TOT
                   MOVE T-FEE TO ACT-F-TOT
                   MOVE ACT-FOOTER-2 TO OUT-PUT
                   WRITE OUT-PUT BEFORE ADVANCING 2 LINE.
           MOVE 0 TO FIRST-RUN
           PERFORM HIGH THRU E-HIGH.
           PERFORM NEW-ACCOUNT THRU E-NEW-ACCOUNT.
       NEW-ACCOUNT.
           MOVE 0 TO A-BALANCE.
           MOVE 0 TO A-D.
           MOVE 0 TO A-W.
           MOVE 0 TO A-F.
           MOVE 0 TO ACT-FEE.
           MOVE 0 TO T-FEE.
           MOVE 0 TO T-INT.
           MOVE ANUM TO CUR-ACCT.
           MOVE CUSNAME TO CUR-NAME.
           MOVE KODE TO CUR-TYPE.
           PERFORM SEARCH-TABLE
           MOVE ACT-HEADER-1 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           MOVE CUSNAME TO ACT-NAME OF ACT-HEADER-2
           MOVE KODE TO ACT-TYPE OF ACT-HEADER-2
           MOVE A-BALANCE TO ACT-BEG OF ACT-HEADER-2
           MOVE A-BALANCE TO ACT-BEG OF ACT-FOOTER-2
           MOVE ACT-HEADER-2 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           PERFORM SAME-ACCOUNT THRU E-SAME-ACCOUNT.
       E-NEW-ACCOUNT.
       EXIT.
       
       SAME-ACCOUNT.
           IF DEP IS ZEROES MOVE ZEROES TO ACT-DEB
           ELSE 
               MOVE DEP TO ACT-DEB
               ADD DEP TO A-BALANCE
               ADD DEP TO A-D
               MOVE 0 TO ACT-FEE.
           IF WITHH IS ZEROES MOVE ZEROES TO ACT-WID
           ELSE 
               MOVE WITHH TO ACT-WID
               SUBTRACT WITHH FROM A-BALANCE
               PERFORM CHECK-FEE THRU E-FEE
               ADD WITHH TO A-W.
           MOVE ACT-REC TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           MOVE "" TO ACT-ERROR.
           PERFORM READ-RECORD.
       E-SAME-ACCOUNT.
       EXIT.
       
       SEARCH-TABLE.
           SET INX-A TO 1
               SEARCH BG-BAL OF BG-BAL-TABLE
                   WHEN T-ACCT(INX-A) = CUR-ACCT
                       MOVE T-BAL(INX-A) TO A-BALANCE
               END-SEARCH.
       
       HIGH.
          ADD A-BALANCE TO ACT-TOTAL.
          ADD 1 TO ACT-COUNT.
          IF A-BALANCE IS GREATER THAN H-B
               MOVE A-BALANCE TO H-B
               MOVE CUR-NAME  TO H-B-N.
          IF CUR-TYPE = "M" AND A-BALANCE IS GREATER THAN H-M
               MOVE A-BALANCE TO H-M
               MOVE CUR-NAME TO H-M-N.
       E-HIGH.
       EXIT.
       
       GET-INTREST.
           IF CUR-TYPE = "M" AND A-BALANCE IS GREATER 150
               MULTIPLY A-BALANCE BY .0163 GIVING T-INT
               ADD T-INT TO A-BALANCE
               MOVE T-INT TO ACT-I-TOT
           ELSE    
               MOVE 0 TO T-INT
               MOVE 0 TO ACT-I-TOT.
       E-INTREST.
       EXIT.
       
       CHECK-FEE.
           IF A-BALANCE IS LESS THAN 150
               MOVE "MINIMUM CHARGE" TO ACT-ERROR
               MOVE 30 TO ACT-FEE
               ADD 30 TO A-F
               ADD 30 TO T-FEE
               SUBTRACT 30 FROM A-BALANCE
           ELSE
               MOVE "" TO ACT-ERROR
               MOVE 0 TO ACT-FEE.
       E-FEE.
       EXIT.
           
       CLOSEUP.
           MOVE ACT-FOOTER-1 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINE
           MOVE ACT-FOOTER-2 TO OUT-PUT
           MOVE CUR-NAME TO ACT-NAME OF ACT-FOOTER-2
           MOVE CUR-TYPE TO ACT-TYPE OF ACT-FOOTER-2
           MOVE A-BALANCE TO ACT-E-TOT
           MOVE A-D TO ACT-D-TOT
           MOVE A-W TO ACT-W-TOT
           MOVE T-FEE TO ACT-F-TOT
           MOVE ACT-FOOTER-2 TO OUT-PUT
           WRITE OUT-PUT BEFORE ADVANCING 2 LINE.
           PERFORM FINAL-PRINT
           CLOSE BANKIN BANKOUT.
           STOP RUN.
       
       FINAL-PRINT.
           MOVE END-PRINT-1 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINE.
           MOVE H-B-N TO HI-B-NAME
           MOVE H-B TO HI-B-AMT
           MOVE H-M-N TO HI-M-NAME
           MOVE H-M TO HI-M-AMT
           DIVIDE ACT-TOTAL BY ACT-COUNT GIVING ACT-AVG.
           MOVE ACT-AVG TO HI-AVG
           MOVE END-PRINT-2 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINE.
           MOVE END-PRINT-3 TO OUT-PUT
           WRITE OUT-PUT AFTER ADVANCING 1 LINE.
           
           
           
           
           
           