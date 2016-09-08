       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. BANK.                                                        
       AUTHOR. ERIC EDGARS.
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-Z.                                              
       OBJECT-COMPUTER. IBM-Z.                                               
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
            SELECT BANKIN  ASSIGN TO UT-S-BANKING.
            SELECT BANKOUT ASSIGN TO UT-S-BANKOUT.
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  BANKIN                                                               
           LABEL RECORDS OMITTED                                                
           RECORDING MODE F.                                                    
       01  INPUT-RECORD.                                                        
           02  KODE PIC X.                                                      
           02  FILLER PIC X.                                                    
           02  CUSNAME PIC X(21).                                               
           02  FILLER PIC X.                                                    
           02  ANUM PIC X(5).
           02  FILLER PIC X(10).                                                
           02  DEP PIC 9(4)V99.                                                 
           02  WITHH PIC 9(4)V99.                                               
       FD  BANKOUT                                                              
           LABEL RECORDS OMITTED                                                
           RECORDING MODE F.                                                    
       01  OUT-PUT PIC X(132).                                                  
       WORKING-STORAGE SECTION.                                                 
      * THIS IS WORKING STORAGE SECTION.                                        
       77  TOTSD PIC 9(4)V99.
       77  NUMA PIC X(5).
       77  C-TOTSD PIC 9(4)V99.
       77  TOTCD PIC 9(4)V99.
       77  A-SW PIC 9(4)V99.                                                    
       77  THESWITCH PIC 9.                                                     
       77  BALANCE PIC 9999V99.                                                 
       77  TOTSW PIC 9(4)V99.
       77  SDD PIC 99 .
       77  MONTH-INT PIC 9999V99.                                               
       77  CWW PIC 99.
       77  TOTCW PIC 99.
       77  P-BAL PIC 9999V99.                                                   
       77  C-TOTCD PIC 9(4)V99.
       77  APR PIC V999.                                                        
       77  C-TOTCW PIC 9(4)V99.
       77  A-CW PIC 9(4)V99.
       77  C-TOTSW PIC 9(4)V99.
       77  FIN-CHG PIC 9999V99.                                                 
       77  A-SD PIC 9(4)V99.
       77  FIRST-RECORD PIC 9 VALUE 1.                                          
       77  SWW PIC 99.
       77  A-CD PIC 9(4)V99.                                                    
       77  CDD PIC 99.
       77  UT-S-BANKING PIC X(50) VALUE "C:\CS4230\BANKFILE.DAT".
       77  UT-S-BANKOUT PIC X(50) VALUE "C:\CS4230\BANKREPXXX.RPT".
       01  HEADER1.                                                             
           02  FILLER PIC X(35).                                                
           02  FILLER PIC X(20) VALUE "BANK OF WISCONSIN".                      
       01  TOTAL1.                                                              
           02  FILLER PIC X(10).                                                
           02  TOTSDO PIC $(4)V99.                                              
           02  FILLER PIC X(12).                                                
           02  TOTCWO PIC $(4)V99.                                              
           02  FILLER PIC X(6).                                                 
           02  TOTCDO PIC $(4)V99.                                              
           02  TOTSWO PIC $(4)V99.                                              
       01  MESS-LINE.                                                           
           03 FILLER PIC X(10).                                                 
           03 MESS-LINE-ITEM  PIC X(50) 
		                             VALUE "Bad Account Code".                                        
       01  AVG1.                                                                
           02 FILLER PIC X(5).                                                  
           02 FILLER PIC X(15) VALUE "AVR SAV DEP = ".                          
           02 SD-A PIC 9(4)V99.                                                 
           02 FILLER PIC X(9).                                                  
           02 FILLER PIC X(15) VALUE "AVR SAV WITH = ".                         
           02 SW-A PIC 9(4)V99.                                                 
           02 CW-A PIC 9(4)V99.                                                 
           02 FILLER PIC X(15) VALUE "AVR CHK WITH = ".                         
           02 FILLER PIC X(20).                                                 
           02 CD-A PIC 9(4)V99.                                                 
       01  OUTLINE.                                                             
           02 FILLER PIC X(5).                                                  
           02 CNAME PIC X(21).                                                  
           02 FILLER PIC X(7).                                                  
           02 ACTNUM PIC X(5).                                                  
           02 FILLER PIC X(12).                                                 
           02 DEPS PIC $ZZZZ.99.                                                
           02 FILLER PIC X(17).                                                 
           02 WITHR PIC $ZZZZV99.                                               
       PROCEDURE DIVISION.                                                      
       BEGINHERE.                                                               
           OPEN INPUT BANKIN.                                                   
           OPEN OUTPUT BANKOUT.                                                 
           MOVE HEADER1 TO OUT-PUT.                                             
           WRITE OUT-PUT AFTER ADVANCING 2 LINES.                               
       CHECK-RTN.                                                               
           MOVE ZEROS TO THESWITCH.                                             
      * CHECK THE SWITCH                                                        
           IF THESWITCH = 0 GO TO STARTHERE.                                    
       ENDIT.                                                                   
           CLOSE BANKIN BANKOUT. STOP RUN.                                      
       STARTHERE.                                                               
           READ BANKIN AT END GO TO GOTHERE.                                    
      * CHECK TO SEE IF IT IS FIRST RECORD                                      
           IF FIRST-RECORD = 1 GO TO OUTTHERE ELSE GO TO SOMEWHERE.             
       PROCESS.                                                                 
           IF ANUM = NUMA IF KODE = "C" GO TO CHECKING ELSE GO TO               
           SAVING ELSE GO TO BREAK-THELOOP.                                     
       CHECKING.                                                                
           ADD DEP TO TOTCD. 
           ADD WITHH TO TOTCW. 
           ADD DEP TO C-TOTCD. 
           ADD WITHH TO C-TOTCW.
           ADD 1 TO CDD.
           ADD 1 TO CWW.
           GO TO PRINTIT.
       CHECKIT. 
           IF WITHH GREATER THAN BALANCE THEN 
           MOVE "NOT ENOUGH FUNDS IN ACCOUNT" TO MESS-LINE-ITEM. 
           GO TO MESS-PRINT.
       SAVING.                                                                  
           ADD DEP TO TOTSD. 
           ADD WITHH TO TOTSW. 
           ADD DEP TO C-TOTSD.            
           ADD WITHH TO C-TOTSW. 
           ADD 1 TO SDD. 
           ADD 1 TO SWW. GO TO              
           STARTHERE.                                                             
      * CHECK IF THE BALANCE IS LESS THAN 500                                   
       CHECK-AGAIN. 
           IF BALANCE LESS THAN 500 
               MOVE 10.00 TO FIN-CHG              
               SUBTRACT FIN-CHG FROM BALANCE.                                       
       OUTTHERE.                                                                
           MOVE ANUM TO NUMA.
           MOVE ZEROS TO FIRST-RECORD.
           GO TO PROCESS.        
       SOMEWHERE.                                                               
           GO TO PROCESS.                                                       
       BREAK-THELOOP.                                                           
           GO TO PROCESS.
           MOVE C-TOTSD TO TOTSDO. 
           MOVE C-TOTSW TO TOTSWO. 
           MOVE C-TOTCD TO TOTCDO. 
           MOVE C-TOTCW TO TOTSWO. 
           MOVE TOTAL1 TO OUT-PUT. 
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           MOVE ANUM TO NUMA. 
           MOVE 1 TO FIRST-RECORD. 
           GO TO PRINTIT.                             
       PRINTIT.                                                                 
           MOVE CUSNAME TO CNAME. 
           MOVE ANUM TO ACTNUM. 
           MOVE DEP TO DEPS.        
           MOVE WITHH TO WITHR. 
           MOVE OUTLINE TO OUT-PUT. 
           WRITE OUT-PUT          
           AFTER ADVANCING 1 LINES. 
           GO TO STARTHERE.                            
       GOTHERE.                                                                 
           DIVIDE TOTSD BY SDD GIVING A-SD. 
           DIVIDE TOTCD BY CDD GIVING A-CD.
           DIVIDE TOTSW BY SWW GIVING A-SW. 
           DIVIDE TOTCW BY CWW GIVING A-CW. 
           MOVE A-SD TO SD-A. 
           MOVE A-CD TO CD-A. 
           MOVE A-SW TO SW-A. 
           MOVE A-CW TO SW-A.
           MOVE AVG1 TO OUT-PUT. 
           WRITE OUT-PUT AFTER ADVANCING 1 LINES.
           MOVE 1 TO THESWITCH. GO TO ENDIT.
       MESS-PRINT.                                                              
           MOVE MESS-LINE TO OUT-PUT.
           WRITE OUT-PUT AFTER ADVANCING             
           1 LINES.
           GO TO STARTHERE.
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
