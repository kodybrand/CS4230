       class-id BookApp.BookInventory.
       
       DATA DIVISION.

       working-storage section.
       01 BOOK-DATA.
          05  BK-ITEM-NO               PIC X(5).
          05  BK-DESCRIPTIVE-DATA.
              10  BK-ITEM-DESC         PIC X(40).
              10  BK-UNIT-COST         PIC 9(3)V99.
              10  BK-UNIT-PRICE        PIC 9(3)V99.
          05  BK-INVENTORY-DATA.
              10  BK-REORDER-POINT     PIC S9(5).
              10  BK-ON-HAND           PIC S9(5).
              10  BK-ON-ORDER          PIC S9(5).
    
       method-id SetBookInfo.
       local-storage section.
       linkage section.
       01  LS-BOOK-DATA.
           05  LS-ITEM-NO              PIC X(5).
           05  LS-DESCRIPTIVE-DATA     PIC X(50).
           05  LS-INVENTORY-DATA       PIC X(15).
           
       procedure division using LS-BOOK-DATA.
           MOVE LS-BOOK-DATA TO BOOK-DATA
       
       end method.
       
       method-id GetDescInfo.
       local-storage section.
       01 ANS          pic x.
       linkage section.
       01  LS-DESCRIPTIVE-DATA.
           05  LS-ITEM-DESC            PIC X(40).
           05  LS-UNIT-COST            PIC 9(3)V99.
           05  LS-UNIT-PRICE           PIC 9(3)V99.
           
       procedure division USING LS-DESCRIPTIVE-DATA.
          
          MOVE BK-DESCRIPTIVE-DATA TO LS-DESCRIPTIVE-DATA.
       
       end method.
       
       method-id GetInvInfo.
       local-storage section.
       linkage section.
       01  LS-INVENTORY-DATA.
           05  LS-REORDER-POINT        PIC S9(5).
           05  LS-ON-HAND              PIC S9(5).
           05  LS-ON-ORDER             PIC S9(5).
           
       procedure division USING LS-INVENTORY-DATA.
           
           MOVE BK-INVENTORY-DATA TO LS-INVENTORY-DATA.
       
       end method.
       
       end class.
