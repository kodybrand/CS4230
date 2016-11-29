       IDENTIFICATION DIVISION.
       program-id. Program1 as "BookApp.BookInquiry".
       AUTHOR. Landgraf
      ******************************************************************
      * This program is a demo of how object oriented COBOL works.  The
      *  program creates a book manager object and allows the user
      *  to examine records stored in the INVMASTI.DAT file.
      *
      * Classes: 
      *      BookInventory
      *      BookManager
      *      FileManager
      *      UserInterface
      * Input file is INVMASTI.DAT
      *   Item Number 5 characters
      *   Description 40 characters
      *   Unit Cost   999.99
      *   Unit Price  999.99
      *   Reorder Point  S9(5)
      *   On Hand        S9(5)
      *   On Order       S9(5)   
      ******************************************************************

       data division.
       working-storage section.
        01  BookMngObj type BookManager.

       procedure division.
       
            set BookMngObj to new BookManager
            invoke BookMngObj::ProcessInquiries
            stop run.
           
       end program.
