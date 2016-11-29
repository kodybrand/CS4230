       class-id BookApp.UserInterface.

       working-storage section.
       
       method-id GetItemNumber.
       local-storage section.
       01  SWITCHES.
           05  VALID-ENTRY-SWITCH      PIC X   VALUE "N".
               88 VALID-ENTRY                  VALUE "Y".
       01 ERROR-MESSAGE                PIC X(50).
       
       linkage section.
       01  LS-ITEM-NUMBER              PIC X(5).
       
       procedure division RETURNING LS-ITEM-NUMBER.
       
           MOVE "N" TO VALID-ENTRY-SWITCH
           MOVE SPACES TO LS-ITEM-NUMBER
           PERFORM  UNTIL VALID-ENTRY
               DISPLAY "Item Number..... " AT LINE 5 COLUMN 1
               DISPLAY "ENTER 99999 TO END" AT LINE 24 COLUMN 1
               ACCEPT LS-ITEM-NUMBER AT LINE 5 COLUMN 22 
                      WITH PROMPT
               IF LS-ITEM-NUMBER = SPACE
                   MOVE "An Item Number is Required" TO ERROR-MESSAGE
                   INVOKE self::DisplayErrorMessage(ERROR-MESSAGE)
               ELSE
                   SET VALID-ENTRY TO TRUE
               END-IF
          END-PERFORM
       
       end method.
       
       method-id GetNextItem.
    
       local-storage section.
       
       01 SWITCHES.
          05  NEXT-ITEM-SWITCH         PIC X   VALUE " ".
              88  NEXT-ITEM                    VALUE "Y".
              
          05  ERROR-MESSAGE            PIC X(50).
       
       linkage section.
       01  LS-END-OF-INQUIRIES-SWITCH  PIC X.
       
       PROCEDURE DIVISION USING LS-END-OF-INQUIRIES-SWITCH.
       
           MOVE "N" TO LS-END-OF-INQUIRIES-SWITCH
           MOVE " " TO NEXT-ITEM-SWITCH
           PERFORM UNTIL NEXT-ITEM-SWITCH = "Y" OR "N"
               DISPLAY "Do you want to display another item?"
                        LINE 24 COLUMN 1
               ACCEPT NEXT-ITEM-SWITCH AT LINE 24 COLUMN 38
                   WITH FOREGROUND-COLOR 15 UPPER
               EVALUATE NEXT-ITEM-SWITCH
                  WHEN "Y"
                         MOVE "N" TO LS-END-OF-INQUIRIES-SWITCH
                  WHEN "N"
                         MOVE "Y" TO LS-END-OF-INQUIRIES-SWITCH
                  WHEN OTHER
                         MOVE "You must Enter Y or N " TO
                                   ERROR-MESSAGE
                         INVOKE 
                             self::DisplayErrorMessage(ERROR-MESSAGE)
               END-EVALUATE
           END-PERFORM
           
       end method.
       
       method-id DisplayHeading.
       
       procedure division.
           DISPLAY "INVENTORY INQUIRY" AT LINE 1 COLUMN 1
               WITH BLANK SCREEN
               FOREGROUND-COLOR 0
               BACKGROUND-COLOR 7
           DISPLAY "Type an item number Then Press Enter"
               AT LINE 3 COLUMN 1
       end method.
       
       method-id DisplayDescInfo.
       
       local-storage section.
       01  SCREEN-DISPLAY-FIELDS.
           05  UNIT-COST               PIC ZZZ.ZZ.
           05  UNIT-PRICE              PIC ZZZ.ZZ.
           
       linkage section.
       01  LS-DESCRIPTIVE-DATA.
           05  LS-ITEM-DESC            PIC X(40).
           05  LS-UNIT-COST            PIC 9(3)V99.
           05  LS-UNIT-PRICE           PIC 9(3)V99.
           
       procedure division using LS-DESCRIPTIVE-DATA.
           MOVE LS-UNIT-COST TO UNIT-COST
           MOVE LS-UNIT-PRICE TO UNIT-PRICE
           DISPLAY "DESCRIPTION:" LINE 7 COLUMN 1
           DISPLAY LS-ITEM-DESC   LINE 7 COLUMN 22
           DISPLAY "Unit Cost:"   LINE 8 COLUMN 1
           DISPLAY UNIT-COST      LINE 8 COLUMN 22
           DISPLAY "Unit Price:"  LINE 9 COLUMN 1
           DISPLAY UNIT-PRICE    LINE 9 COLUMN 22
       end method.
       
       method-id DisplayInvInfo.
       
       local-storage section.
       01  SCREEN-DISPLAY-FIELDS.
           05  REORDER-POINT           PIC ZZ,ZZZ-.
           05  ON-HAND                 PIC ZZ,ZZZ-.
           05  ON-ORDER                PIC ZZ,ZZZ-.
       
       01  INV-DATA.
           05  LS-REORDER-POINT        PIC S9(5).
           05  LS-ON-HAND              PIC S9(5).
           05  LS-ON-ORDER             PIC S9(5).
           
       linkage section.
       01  LS-INVENTORY-DATA           PIC X(15).
           
           
       procedure division using LS-INVENTORY-DATA.
           MOVE LS-INVENTORY-DATA TO INV-DATA
           MOVE LS-REORDER-POINT TO REORDER-POINT
           MOVE LS-ON-HAND TO ON-HAND
           MOVE LS-ON-ORDER TO ON-ORDER
           DISPLAY "Reorder Point:" LINE 10 COLUMN 1
           DISPLAY REORDER-POINT    LINE 10 COLUMN 22
           DISPLAY "On Hand:"       LINE 11 COLUMN 1
           DISPLAY ON-HAND          LINE 11 COLUMN 22
           DISPLAY "On Order:"      LINE 12 COLUMN 1
           DISPLAY ON-ORDER         LINE 12 COLUMN 22
       
       end method.
       
       method-id DisplayErrorMessage.
       
       linkage section.
       01 LS-ERROR-MESSAGE         PIC X(50).
       
       procedure division using LS-ERROR-MESSAGE.
       
           DISPLAY LS-ERROR-MESSAGE AT LINE 23 COLUMN 1
               WITH FOREGROUND-COLOR 15 BEEP.
       end method.
       
       method-id DisplayBlankLine.
       
       procedure division.
       
           DISPLAY SPACES AT LINE 23 COLUMN 1
              
       end method.
       
       
       end class.
