       class-id BookApp.BookManager.

       working-storage section.
       01  BookUIObj type UserInterface.
       01  BookFMObj type FileManager.
       01  BookInvObj type BookInventory.
       
       01 Switches.
          05  END-OF-INQUIRIES-SWITCH           PIC X  VALUE "N".
              88  End-of-Inquiries                     VALUE "Y".
              
       01 ERROR-MESSAGE                        PIC X(50).
       
       01  BOOK-DATA.
           05  BK-ITEM-NUMBER                      PIC X(5).
           05  BK-DESCRIPTIVE-DATA.
               10  BK-ITEM-DESC                    PIC X(40).
               10  BK-UNIT-COST                    PIC 9(3)V99.
               10  BK-UNIT-PRICE                   PIC 9(3)V99.
           05  BK-INVENTORY-DATA.
               10  BK-REORDER-POINT                PIC S9(5).
               10  BK-ON-HAND                      PIC S9(5).
               10  BK-ON-ORDER                     PIC S9(5).

       method-id ProcessInquiries.
       local-storage section.
       procedure division.
           set BookUIObj to new UserInterface
           set BookFMObj to new FileManager
           
           INVOKE BookFmObj::OpenFile
           INVOKE BookUIObj::DisplayHeading
           
           PERFORM UNTIL End-Of-Inquiries
               INVOKE BookUIObj::GetItemNumber
                       RETURNING BK-ITEM-NUMBER
               IF BK-ITEM-NUMBER = "99999"
                   MOVE "Y" TO End-of-Inquiries-Switch
               ELSE
                  INVOKE BookFMObj::CreateBook(BK-ITEM-NUMBER) 
                         RETURNING BookInvObj
                  IF BOOKInvObj = NULL
                     MOVE "Inventory Record not Found"
                           to ERROR-MESSAGE
                     INVOKE 
                     BookUIObj::DisplayErrorMessage(ERROR-MESSAGE)
                  ELSE
                     INVOKE BookUIObj::DisplayBlankLine
                     
                     INVOKE 
                         BookInvObj::GetDescInfo(BK-DESCRIPTIVE-DATA)
                         
                     INVOKE 
                         BookUIObj::DisplayDescInfo(BK-DESCRIPTIVE-DATA)
                         
                     INVOKE 
                         BookInvObj::GetInvInfo(BK-INVENTORY-DATA)
                         
                     INVOKE 
                         BookUIObj::DisplayInvInfo(BK-INVENTORY-DATA) 
                         
                     INVOKE self::UpdatePrice
                     
                     INVOKE
                         BookUIObj::GetNextItem(END-OF-INQUIRIES-SWITCH)
                     IF NOT END-OF-INQUIRIES
                         INVOKE BookUIObj::DisplayHeading
                     END-IF
                  END-IF
               END-IF
           END-PERFORM.
           INVOKE BookFMObj::CloseFile
       end method.
       
       method-id UpdatePrice.
       local-storage section.
       01  ACCEPT-PRICE           PIC X.
       linkage section.
       
       procedure division.
     
           INVOKE 
           BookUIObj::UPDPrice(BK-UNIT-PRICE, ACCEPT-PRICE)                      
          
           IF ACCEPT-PRICE = "Y" OR "y"
                       INVOKE 
                   BookFMObj::UPDPrice(BK-ITEM-NUMBER, BK-UNIT-PRICE)
                   DISPLAY "UPDATED RECORD!" AT LINE 23 COLUMN 1
           END-IF.
           
       end method.
       
       end class.
