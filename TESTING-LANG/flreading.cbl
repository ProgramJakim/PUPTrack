      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. TOSHIBA.
       OBJECT-COMPUTER. TOSHIBA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT SALES-READ ASSIGN TO "salesread.dat"
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD   SALES-READ.
       01   SALES-READ-REC.
            05   NAME1-SALES PIC A(10).
            05   FILLER   PIC X(5).
            05   NUM-SALES PIC 9(6).
            05   FILLER   PIC X(5).
            05   SOLD-M PIC Z(10).
            05   FILLER   PIC X(5).
            05   UNITS-M PIC Z(10).
            05   TOTAL-SALES-M PIC Z(10).9(2).

       WORKING-STORAGE SECTION.
       01   CHOICE PIC 9(2).
       01   Y-N PIC A(3).

       01   SOLD PIC 9(10).
       01   UNITS PIC 9(10).
       01   TOTAL-SALES PIC 9(10)V9(2).

       01  EOF-SW PIC X VALUE "N".
       01  RECORD-COUNTER PIC 9(4) VALUE 0.
       01  RECORDS-PER-PAGE PIC 9(4) VALUE 5.
       01  PAGE-NUMBER PIC 9(4) VALUE 1.
       01  USER-CHOICE PIC 9.
       01  START-REC PIC 9(4).
       01  END-REC PIC 9(4).

       PROCEDURE DIVISION.
           PERFORM READ-MENU UNTIL CHOICE = 3.
           STOP RUN.

           READ-MENU.
            DISPLAY "Welcome to Hoshea's Cobol Menu! (Using READ Stmts)"
            DISPLAY "--- Determine Your Total Sales ---"
            DISPLAY "[1] ADD Record"
            DISPLAY "[2] DISPLAY Record"
            DISPLAY "[3] EXIT"
            DISPLAY "Enter your choice: " NO ADVANCING
            ACCEPT CHOICE.

            EVALUATE TRUE
                 WHEN CHOICE = 1
                      PERFORM ADD-RECORD
                      PERFORM READ-MENU
                 WHEN CHOICE = 2
                      PERFORM DISPLAY-RECORD
                      PERFORM READ-MENU
                 WHEN CHOICE = 3
                      STOP RUN

            END-EVALUATE.

           ADD-RECORD.
            OPEN EXTEND SALES-READ.
            DISPLAY "TOTAL SALES"
            DISPLAY "Input your name: " NO ADVANCING
            ACCEPT NAME1-SALES.

            DISPLAY "Input your number: " NO ADVANCING
            ACCEPT NUM-SALES.

            DISPLAY "Input units sold: " NO ADVANCING
            ACCEPT UNITS.

            DISPLAY "Input unit price: " NO ADVANCING
            ACCEPT SOLD.

            COMPUTE TOTAL-SALES = UNITS * SOLD

            MOVE SOLD TO SOLD-M.
            MOVE UNITS TO UNITS-M.
            MOVE TOTAL-SALES TO TOTAL-SALES-M.

            DISPLAY "---------- TOTAL SALES ----------"
            DISPLAY "Salesname: " NAME1-SALES.
            DISPLAY "Salesnumber: " NUM-SALES.
            DISPLAY "Unit/s sold: " UNITS-M.
            DISPLAY "Unit/s price: " SOLD-M.
            DISPLAY "Total sales is: " TOTAL-SALES-M.

            WRITE SALES-READ-REC.
            CLOSE SALES-READ.

            DISPLAY "Do you want another input/s?"
            DISPLAY "(Y/Yes or N/No): " NO ADVANCING
            ACCEPT Y-N.
                 IF Y-N = 'YES' OR 'Y' OR 'y' OR 'yes'
                      GO TO ADD-RECORD
                 ELSE IF Y-N = 'NO' OR 'N' OR 'n' OR 'no'
                      EXIT PROGRAM

            EXIT PROGRAM.

           DISPLAY-RECORD.
           DISPLAY "RECORDS OF TOTAL SALES"
           DISPLAY "Enter page number: " NO ADVANCING
           ACCEPT PAGE-NUMBER
           COMPUTE START-REC = (PAGE-NUMBER - 1) * RECORDS-PER-PAGE + 1
           COMPUTE END-REC = PAGE-NUMBER * RECORDS-PER-PAGE

           OPEN INPUT SALES-READ
           MOVE "N" TO EOF-SW
           MOVE 0 TO RECORD-COUNTER

           PERFORM UNTIL EOF-SW = "Y" OR RECORD-COUNTER >= END-REC
               READ SALES-READ
               AT END
                   MOVE "Y" TO EOF-SW
               NOT AT END
                   ADD 1 TO RECORD-COUNTER

                   IF RECORD-COUNTER >= START-REC
                      DISPLAY "---------- TOTAL SALES ----------"
                      DISPLAY "Salesname: " NAME1-SALES
                      DISPLAY "Salesnumber: " NUM-SALES
                      DISPLAY "Unit/s sold: " UNITS-M
                      DISPLAY "Unit/s price: " SOLD-M
                      DISPLAY "Total sales is: " TOTAL-SALES-M
                      DISPLAY "---------------------------------"
                   END-IF
               END-READ
           END-PERFORM

           CLOSE SALES-READ
           DISPLAY "End of page " PAGE-NUMBER.


           IF RECORD-COUNTER >= END-REC
           DISPLAY "Do you want to go to the next page of the records?"
           DISPLAY "YES or NO?"
           ACCEPT Y-N
               IF Y-N = 'YES' OR 'Y' OR 'y' OR 'yes'
                      ADD 1 TO PAGE-NUMBER
                      GO TO DISPLAY-RECORD
                 ELSE
                   DISPLAY "End of records."
                END-IF
           END-IF.

           EXIT PROGRAM.
       END PROGRAM YOUR-PROGRAM-NAME.
