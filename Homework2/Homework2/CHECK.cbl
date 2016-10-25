       PROGRAM-ID. CHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EMP-ID.
           05  EMP-4   PIC 9.
           05  EMP-3   PIC 9.
           05  EMP-2   PIC 9.
           05  EMP-1   PIC 9.
       77  E-1         PIC 99.
       77  E-2         PIC 99.
       77  WS-TEMP     PIC 99.
       LINKAGE SECTION.
       01  LINK-EMPID  PIC 9999.
       01  LINK-CHKDIG PIC 9.
       
       PROCEDURE DIVISION USING LINK-EMPID, LINK-CHKDIG.
       000-MAIN.
           MOVE LINK-EMPID TO EMP-ID.
           COMPUTE E-1 = 2 * EMP-2.
           IF E-1 > 10
               COMPUTE E-1 = E-1 - 9
           END-IF.
           COMPUTE E-2 = 2 * EMP-4.
           IF E-2 > 10
               COMPUTE E-2 = E-2 - 9
           END-IF
           COMPUTE WS-TEMP = EMP-1 + E-1 + EMP-3 + E-2.
           IF WS-TEMP > 10 THEN
               COMPUTE WS-TEMP = WS-TEMP - 10
           ELSE
               MOVE WS-TEMP TO LINK-CHKDIG
           END-IF.
           MOVE WS-TEMP TO LINK-CHKDIG
           GOBACK.
       END PROGRAM CHECK.