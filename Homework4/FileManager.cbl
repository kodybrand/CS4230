       class-id BookApp.FileManager.
       
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT INVMAST-FILE ASSIGN TO UT-SYS-INVMAST
                   ORGANIZATION IS INDEXED
                   ACCESS IS RANDOM
                   RECORD KEY IS IM-ITEM-NO.
                   
       DATA DIVISION.
       FILE SECTION.
       FD  INVMAST-FILE
           RECORD CONTAINS 70.
       01  INVMAST-REC.
           05  IM-ITEM-NO              PIC X(5).
           05  IM-DESCRIPTIVE-DATA     PIC X(50).
           05  IM-INVENTORY-DATA       PIC X(15).

       working-storage section.
       01  UT-SYS-INVMAST              PIC X(50)
                       VALUE "C:\COBOL\INVMASTI.DAT".
                       
       method-id OpenFile.
       local-storage section.
       procedure division.
           OPEN INPUT INVMAST-FILE
       end method.
       
       method-id. CloseFile.
       local-storage section.
       procedure division.
           CLOSE INVMAST-FILE
       end method.
       
       method-id. CreateBook.
       local-storage section.
       linkage section.
       01  LS-ITEM-NO                  PIC X(5).
       01  LS-BookInvObj type BookInventory.
       
       procedure division using LS-ITEM-NO
                           RETURNING LS-BookInvObj.
       
           MOVE LS-ITEM-NO TO IM-ITEM-NO
           READ INVMAST-FILE
              INVALID KEY
                 SET LS-BookInvObj to NULL
              NOT INVALID KEY
                 SET LS-BookInvObj to New BookInventory
                 INVOKE LS-BookInvObj::SetBookInfo(INVMAST-REC)
           END-READ
       end method.

       end class.
