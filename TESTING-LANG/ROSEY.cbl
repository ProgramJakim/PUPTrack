      ******************************************************************
      * Author: TOLENTINO, MA. ROSE
      * Date: SEPTEBER 30, 2024
      * Purpose: ACTIVITY IN COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. SequenceMainMenu.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HELLOWORLD-FILE
           ASSIGN TO 'C:\Users\User\Documents\comprog3files\try.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

           SELECT MATH-OPERAND-FILE ASSIGN TO "salesread.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRINTNAME-FILE
           ASSIGN TO 'C:\Users\User\Documents\comprog3files\try.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

           SELECT CIRCUM-CIRCLE
           ASSIGN TO 'C:\Users\User\Documents\comprog3files\try.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD HELLOWORLD-FILE.
       01 FIRST-RECORD.
               02 HELLO-WORLD PIC A(12).

       FD MATH-OPERAND-FILE.
       01 MATH-OPERAND-RECORD.
           02 INPUT-NUM.
               03 IN-TEXT1 PIC A(14).
               03 IN-NUM1 PIC Z(5).
               03 IN-FILL1 PIC X(3).
               03 IN-TEXT2 PIC A(15).
               03 IN-NUM2 PIC Z(5).
               03 FILLER-SPACE1 PIC X(15).
           02 OPERAND.
                   03 OP-TEXT1 PIC A(5).
                   03 OP-SUM PIC Z(5).
                   03 FILLER-SLASH1 PIC X(3).
                   03 OP-TEXT2 PIC A(12).
                   03 OP-DIF PIC Z(5).
                   03 FILLER-SLASH2 PIC X(3).
                   03 OP-TEXT3 PIC A(10).
                   03 OP-QUO PIC Z(5).
                   03 FILLER-SLASH3 PIC X(3).
                   03 OP-TEXT4 PIC A(9).
                   03 OP-PROD PIC Z(5).

       FD PRINTNAME-FILE.
       01 NAMEPRINT.
           02 FD-TEXT PIC A(6).
           02 FD-NAME1 PIC A(8).
           02 NM-FILLER-SPACE1 PIC X(3).
           02 FD-NAME2 PIC A(8).
           02 NM-FILLER-SPACE2 PIC X(3).
           02 FD-NAME3 PIC A(8).
           02 NM-FILLER-SPACE3 PIC X(3).
           02 FD-NAME4 PIC A(8).
           02 NM-FILLER-SPACE4 PIC X(3).
           02 FD-NAME5 PIC A(8).
           02 NM-FILLER-SPACE5 PIC X(3).

       FD CIRCUM-CIRCLE.
       01 CIRC-ILE.
           02 RA-TEXT PIC X(8).
           02 RA-RAD PIC Z(5).99.
           02 RA-FILLERSPACE1 PIC X(5).
           02 RA-CIRCUM PIC X(15).
           02 RA-FILLERSPACE2 PIC X(5).
           02 RA-CIR PIC Z(4).99.

       WORKING-STORAGE SECTION.
       01 BUFFER PIC X.
       01 NUM1 PIC 9(4).
       01 NUMM1 PIC Z(9).99.
       01 NUM2 PIC 9(4).
       01 NUMM2 PIC Z(9).99.
       01 ANS PIC Z(4).99.
       01 NAME PIC A(20).
       01 CHOICE PIC 9(4).
       01 RADIUS PIC S9(4)V9(2) VALUE 0.
       01 CIRCUMFERENCE PIC Z(5).99.
       01 PI PIC S9V9(5) VALUE 3.14159.
       01 PRELIM PIC 9(4).
       01 MIDTERM PIC 9(4).
       01 FINALS PIC 9(4).
       01 GWA PIC 9(4)V99.
       01 FGRADE PIC Z(4).99.
       01 SALESNUM PIC Z(4).99.
       01 UNITS PIC 9(4)V99.
       01 UNITS2 PIC Z(4).99.
       01 PRICE PIC 9(4)V99.
       01 PRICE2 PIC Z(4).99.
       01 TOTALSALES PIC Z(4).99.
       01 NUM3 PIC 9(4).
       01 SQUARE PIC Z(4).99.
       01 CUBE PIC Z(4).99.
       01 ENUMBER PIC Z(4).
       01 HOUR PIC 9(4).
       01 HOUR2 PIC Z(4).99.
       01 RATE PIC 9(4)V99.
       01 RATE2 PIC Z(4).99.
       01 GROSSPAY PIC Z(4).99.
       01 FAHRENHEIT PIC Z(4).99.
       01 AREAA PIC S9(4)V99.
       01 PERIMETER PIC S9(4)V99.
       01 WS-EOF PIC X.
       01 WS-FILE-STATUS PIC XX.
       01  WS-FILE-ERROR       PIC X(50).
       PROCEDURE DIVISION.

       INITIALIZE-PROGRAM.
           OPEN I-O PARKING-FILE
           IF WS-FILE-STATUS NOT = "00"
               IF WS-FILE-STATUS = "35"
                   OPEN OUTPUT PARKING-FILE
                   IF WS-FILE-STATUS NOT = "00"
                       STRING "Create File Error: " WS-FILE-STATUS
                           INTO WS-FILE-ERROR
                       DISPLAY WS-FILE-ERROR
                       STOP RUN
                   END-IF
                   CLOSE MATH-OPERAND-FILE
                   OPEN I-O MATH-OPERAND-FILE
               ELSE
                   STRING "Open Error: " WS-FILE-STATUS
                       INTO WS-FILE-ERROR
                   DISPLAY WS-FILE-ERROR
                   STOP RUN
               END-IF
           END-IF
           .
       
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROGRAM
           PERFORM UNTIL CHOICE = 12
               CALL "SYSTEM" USING "CLS"
               DISPLAY " "
               DISPLAY "|--------------------------------------------|"
               DISPLAY "|*********** SEQUENTIAL MAIN MENU ***********|"
               DISPLAY "|--------------------------------------------|"
               DISPLAY "| 1  | HELLO WORLD                           |"
               DISPLAY "| 2  | MATH OPERATIONS                       |"
               DISPLAY "| 3  | PRINT NAME FIVE TIMES                 |"
               DISPLAY "| 4  | CIRCUMFERENCE AND AREA OF A CIRCLE    |"
               DISPLAY "| 5  | FINAL GRADES                          |"
               DISPLAY "| 6  | DISPLAY THE TOTAL SALES               |"
               DISPLAY "| 7  | SWAP THE VARIABLES                    |"
               DISPLAY "| 8  | SQUARE AND CUBE OF A NUMBER           |"
               DISPLAY "| 9  | GROSSPAY                              |"
               DISPLAY "| 10 | CELSIUS TO FAHRENHEIT                 |"
               DISPLAY "| 11 | AREA AND PERIMETER OF A CIRCLE        |"
               DISPLAY "| 12 | EXIT                                  |"
               DISPLAY "|--------------------------------------------|"
               DISPLAY " "
               DISPLAY "Enter your choice: " WITH NO ADVANCING
               ACCEPT CHOICE

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM CLEARSCREEN
                       OPEN EXTEND HELLOWORLD-FILE

                       DISPLAY "HELLO WORLD"
                       MOVE "HELLO WORLD" TO HELLO-WORLD
                       WRITE FIRST-RECORD
                       CLOSE HELLOWORLD-FILE

                       ACCEPT BUFFER

                   WHEN 2
                       PERFORM CLEARSCREEN
                       OPEN EXTEND MATH-OPERAND-FILE
                       DISPLAY "MATH OPERATIONS SELECTED"
                       DISPLAY " "

                       MOVE SPACES TO FILLER-SPACE1
                       MOVE " | " TO FILLER-SLASH1
                       MOVE " | " TO FILLER-SLASH2
                       MOVE " | " TO FILLER-SLASH3
                       MOVE "FIRST NUMBER: " TO IN-TEXT1
                       MOVE " | " TO IN-FILL1
                       MOVE "SECOND NUMBER: " TO IN-TEXT2
                       MOVE "SUM: " TO OP-TEXT1
                       MOVE "DIFFERENCE: " TO OP-TEXT2
                       MOVE "QUOTIENT: " TO OP-TEXT3
                       MOVE "PRODUCT: " TO OP-TEXT4

                       DISPLAY "Enter first number: " WITH NO ADVANCING
                       ACCEPT NUM1
                       MOVE NUM1 TO IN-NUM1

                       DISPLAY "Enter second number: " WITH NO ADVANCING
                       ACCEPT NUM2
                       MOVE NUM2 TO IN-NUM2

                       MOVE 0 TO ANS
                       DISPLAY " "
                       COMPUTE ANS = NUM1 + NUM2
                       DISPLAY "SUM: " ANS
                       MOVE ANS TO OP-SUM

                       MOVE 0 TO ANS
                       COMPUTE ANS = NUM1 - NUM2
                       DISPLAY "DIFFERENCE: " ANS
                       MOVE ANS TO OP-DIF

                       MOVE 0 TO ANS
                       COMPUTE ANS = NUM1 / NUM2
                       DISPLAY "QUOTIENT: " ANS
                       MOVE ANS TO OP-QUO

                       MOVE 0 TO ANS
                       COMPUTE ANS = NUM1 * NUM2
                       DISPLAY "PRODUCT: " ANS
                       MOVE ANS TO OP-PROD

                       ACCEPT BUFFER

                       WRITE MATH-OPERAND-RECORD
                       END-WRITE
                       CLOSE MATH-OPERAND-FILE


                   WHEN 3
                       PERFORM CLEARSCREEN
                       OPEN EXTEND PRINTNAME-FILE
                       DISPLAY "PRINTING NAME FIVE TIMES"
                       DISPLAY " "

                       MOVE "NAME: " TO FD-TEXT
                       MOVE SPACES TO NM-FILLER-SPACE1
                       MOVE SPACES TO NM-FILLER-SPACE2
                       MOVE SPACES TO NM-FILLER-SPACE3
                       MOVE SPACES TO NM-FILLER-SPACE4
                       MOVE SPACES TO NM-FILLER-SPACE5

                       DISPLAY "MA. ROSE"
                       DISPLAY "MA. ROSE"
                       DISPLAY "MA. ROSE"
                       DISPLAY "MA. ROSE"
                       DISPLAY "MA. ROSE"

                       MOVE "MA. ROSE" TO FD-NAME1, FD-NAME2, FD-NAME3,
                       FD-NAME4, FD-NAME5

                       ACCEPT BUFFER

                       WRITE NAMEPRINT
                       END-WRITE
                       CLOSE PRINTNAME-FILE

                   WHEN 4
                       PERFORM CLEARSCREEN
                       OPEN EXTEND CIRCUM-CIRCLE
                       DISPLAY "CIRCUMFERENCE"
                       DISPLAY " "

                       MOVE "RADIUS: " TO RA-TEXT
                       MOVE SPACES TO RA-FILLERSPACE1
                       MOVE "CIRCUMFERENCE: " TO RA-CIRCUM
                       MOVE SPACES TO RA-FILLERSPACE2



                       DISPLAY "Enter the radius of the circle: "
                       WITH NO ADVANCING
                       ACCEPT RADIUS
                       MOVE RADIUS TO RA-RAD

                       COMPUTE CIRCUMFERENCE = 2 * 3.14 * RADIUS
                       DISPLAY "The circumference of the circle is: "
                       CIRCUMFERENCE
                       MOVE CIRCUMFERENCE TO RA-CIR
                       ACCEPT BUFFER

                       WRITE CIRC-ILE
                       END-WRITE
                       CLOSE CIRCUM-CIRCLE

                   WHEN 5
                       PERFORM CLEARSCREEN
                       DISPLAY "FINAL GRADES SELECTED"
                       DISPLAY " "


                       DISPLAY "Enter Prelim Grade:  " WITH NO ADVANCING
                       ACCEPT PRELIM

                       DISPLAY "Enter Midterm Grade: " WITH NO ADVANCING
                       ACCEPT MIDTERM

                       DISPLAY "Enter Finals Grade:  " WITH NO ADVANCING
                       ACCEPT FINALS

                       COMPUTE GWA = PRELIM + MIDTERM + FINALS
                       COMPUTE FGRADE = GWA / 3

                       DISPLAY " "
                       DISPLAY "YOUR FINAL GRADE: " FGRADE
                       ACCEPT BUFFER

                   WHEN 6
                       PERFORM CLEARSCREEN
                       DISPLAY "DISPLAYING TOTAL SALES"
                       DISPLAY " "

                       DISPLAY "Enter your name: " WITH NO ADVANCING
                       ACCEPT NAME

                       DISPLAY "Enter your salesnumber: "
                       WITH NO ADVANCING
                       ACCEPT SALESNUM

                       DISPLAY "Enter unit sold:  " WITH NO ADVANCING
                       ACCEPT UNITS
                       MOVE UNITS TO UNITS2

                       DISPLAY "Enter unit price: " WITH NO ADVANCING
                       ACCEPT PRICE
                       MOVE PRICE TO PRICE2

                       COMPUTE TOTALSALES = UNITS * PRICE
                       DISPLAY " "

                       DISPLAY "NAME: " NAME
                       DISPLAY "SALESNUMBER: " SALESNUM
                       DISPLAY "UNIT SOLD: " UNITS2
                       DISPLAY "UNIT PRICE: " PRICE2
                       DISPLAY "YOUR TOTAL SALES: " TOTALSALES
                       ACCEPT BUFFER

                   WHEN 7
                       PERFORM CLEARSCREEN
                       DISPLAY "SWAPPING VARIABLES"
                       DISPLAY " "

                       DISPLAY "Enter first number: " WITH NO ADVANCING
                       ACCEPT NUM1
                       MOVE NUM1 TO NUM3
                       MOVE NUM3 TO NUMM1

                       DISPLAY "Enter second number: " WITH NO ADVANCING
                       ACCEPT NUM2
                       MOVE NUM2 TO NUMM2

                       DISPLAY " "
                       DISPLAY "Before swapping:"
                       DISPLAY "NUM1 = " NUMM1
                       DISPLAY "NUM2 = " NUMM2
                       DISPLAY " "

                       COMPUTE NUM1 = (NUM1 - NUM1) + NUM2
                       COMPUTE NUM2 = (NUM2 - NUM2) + NUM3
                       MOVE NUM1 TO NUMM1
                       MOVE NUM2 TO NUMM2

                       DISPLAY "After swapping:"
                       DISPLAY "NUM1 = " NUMM1
                       DISPLAY "NUM2 = " NUMM2
                       ACCEPT BUFFER


                   WHEN 8
                       PERFORM CLEARSCREEN
                       DISPLAY "SQUARE AND CUBE OF A NUMBER"
                       DISPLAY " "

                       DISPLAY "Enter a number: " WITH NO ADVANCING
                       ACCEPT NUM1

                       COMPUTE SQUARE = NUM1 * NUM1
                       COMPUTE CUBE = NUM1 * NUM1 * NUM1

                       DISPLAY " "
                       DISPLAY "SQUARE: " SQUARE
                       DISPLAY "CUBE: " CUBE
                       ACCEPT BUFFER

                   WHEN 9
                       PERFORM CLEARSCREEN
                       DISPLAY "GROSSPAY SELECTED"
                       DISPLAY " "

                       DISPLAY "Enter employee name: " WITH NO ADVANCING
                       ACCEPT NAME

                       DISPLAY "Enter employee number: "
                       WITH NO ADVANCING
                       ACCEPT ENUMBER

                       DISPLAY "Enter number of hours worked: "
                       WITH NO ADVANCING
                       ACCEPT HOUR
                       MOVE HOUR TO HOUR2

                       DISPLAY "Enter rate per hour: " WITH NO ADVANCING
                       ACCEPT RATE
                       MOVE RATE TO RATE2

                       COMPUTE GROSSPAY = HOUR * RATE

                       DISPLAY " "
                       DISPLAY "NAME: " NAME
                       DISPLAY "EMPLOYEE NUMBER: " ENUMBER
                       DISPLAY "HOUR[S] WORKED: " HOUR2
                       DISPLAY "RATE PER HOUR: " RATE2
                       DISPLAY "YOUR SALARY: " GROSSPAY
                       ACCEPT BUFFER

                   WHEN 10
                       PERFORM CLEARSCREEN
                       DISPLAY "CELSIUS TO FAHRENHEIT CONVERSION"
                       DISPLAY " "

                       DISPLAY "Enter temperature in Celsius: "
                       WITH NO ADVANCING
                       ACCEPT NUM1

                       COMPUTE FAHRENHEIT = (NUM1 * 1.8 ) + 32

                       DISPLAY " "
                       DISPLAY "Temperature in Fahrenheit: " FAHRENHEIT
                       ACCEPT BUFFER

                   WHEN 11
                       PERFORM CLEARSCREEN
                       DISPLAY "AREA AND PERIMETER OF A CIRCLE"
                       DISPLAY " "

                       DISPLAY"Enter a length of a rectangle: "
                       WITH NO ADVANCING
                       ACCEPT NUM1

                       DISPLAY "Enter a width of a rectangle: "
                       WITH NO ADVANCING
                       ACCEPT NUM2

                       COMPUTE AREAA = NUM1 * NUM2
                       MOVE AREAA TO NUMM1
                       COMPUTE PERIMETER = (NUM1 + NUM2) * 2
                       MOVE PERIMETER TO NUMM2

                   DISPLAY " "
                   DISPLAY "The AREA of a rectangle is: " NUMM1
                   DISPLAY "The PERIMETER of a rectangle is: " NUMM2
                   ACCEPT BUFFER

                   WHEN 12
                       PERFORM CLEARSCREEN
                       DISPLAY "THANK YOU FOR USING THE PROGRAM!"
                       ACCEPT BUFFER

                   WHEN OTHER
                       PERFORM CLEARSCREEN
                       DISPLAY "INVALID INPUT."
                       ACCEPT BUFFER

               END-EVALUATE
           END-PERFORM.

           STOP RUN.

       CLEARSCREEN.
           CALL "SYSTEM" USING "CLS".
       END PROGRAM SequenceMainMenu.
