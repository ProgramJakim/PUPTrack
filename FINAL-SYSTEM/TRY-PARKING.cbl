       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARKING-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARKING-FILE ASSIGN TO "PARKING.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PARKING-FILE.
       01  PARKING-RECORD.
           05 STUDENT-NUMBER    PIC X(11).
           05 STUDENT-NAME      PIC X(30).
           05 MOTORCYCLE-MODEL  PIC X(20).
           05 LICENSE-PLATE     PIC X(6).
           05 MOTORCYCLE-COLOR  PIC X(10).
           05 TIME-OF-ENTRY     PIC X(25).
           05 TIME-OF-EXIT      PIC X(25).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS        PIC XX.
       01  WS-OPTIONS.
           05 WS-OPTION         PIC 9 VALUE 0.
       01  WS-INPUT.
           05 WS-STUDENT-NUMBER PIC X(11).
           05 WS-STUDENT-NAME   PIC X(30).
           05 WS-MODEL          PIC X(20).
           05 WS-PLATE          PIC X(6).
           05 WS-COLOR          PIC X(10).
       01  WS-TIMESTAMP.
           05 WS-DATE-TIME      PIC X(25).
       01  WS-STATUS-FLAGS.
           05 VALID-INPUT       PIC X VALUE 'N'.
           05 RECORD-FOUND      PIC X VALUE 'N'.
           05 END-OF-FILE       PIC X VALUE 'N'.
       01  WS-CURRENT-DATE.
           05  WS-YEAR           PIC 9(4).
           05  WS-MONTH          PIC 99.
           05  WS-DAY            PIC 99.
           05  WS-HOURS          PIC 99.
           05  WS-MINUTES        PIC 99.
           05  WS-SECONDS        PIC 99.
           05  WS-HUNDREDTHS     PIC 99.
           05  WS-GMT-SIGN       PIC X.
           05  WS-GMT-HOURS      PIC 99.
           05  WS-GMT-MINUTES    PIC 99.
       01  WS-EOF              PIC X VALUE 'N'.
       01  WS-FILE-ERROR       PIC X(50).

      * MAIN FUNCTION
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-PROGRAM
           PERFORM DISPLAY-MENU UNTIL WS-OPTION = 6
           PERFORM TERMINATE-PROGRAM
           STOP RUN.

      * DEBUGGING FUNCTION & AUTO GENERATES .DAT FILE
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
                   CLOSE PARKING-FILE
                   OPEN I-O PARKING-FILE
               ELSE
                   STRING "Open Error: " WS-FILE-STATUS
                       INTO WS-FILE-ERROR
                   DISPLAY WS-FILE-ERROR
                   STOP RUN
               END-IF
           END-IF
           .

      * MAIN MENU
       DISPLAY-MENU.
           DISPLAY "--- RTU STUDENT PARKING SYSTEM ---".
           DISPLAY "1. New Parking".
           DISPLAY "2. Edit Parking".
           DISPLAY "3. Display Parking Info".
           DISPLAY "4. Exit Parking".
           DISPLAY "5. Display All Data".
           DISPLAY "6. Exit Program".
           DISPLAY "Enter your option: " WITH NO ADVANCING.
           ACCEPT WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM NEW-PARKING
               WHEN 2
                   PERFORM EDIT-PARKING
               WHEN 3
                   PERFORM DISPLAY-PARKING-INFO
               WHEN 4
                   PERFORM EXIT-PARKING
               WHEN 5
                   PERFORM DISPLAY-ALL-DATA
               WHEN 6
                   DISPLAY "Exiting Program..."
               WHEN OTHER
                   DISPLAY "Invalid Option! Try Again."
           END-EVALUATE.

      * FUNCTION DEFINITIONS (CRUD)
      
      * CREATE FUNCTION
       NEW-PARKING.
           CLOSE PARKING-FILE
           OPEN EXTEND PARKING-FILE
           IF WS-FILE-STATUS NOT = "00"
               STRING "File Open Error: " WS-FILE-STATUS
                   INTO WS-FILE-ERROR
               DISPLAY WS-FILE-ERROR
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Enter Student Number: " WITH NO ADVANCING
           ACCEPT WS-STUDENT-NUMBER
           PERFORM CHECK-STUDENT-NUMBER
           IF VALID-INPUT = 'N'
               DISPLAY "Invalid Student Number!"
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Enter Student Name: " WITH NO ADVANCING.
           ACCEPT WS-STUDENT-NAME.
           DISPLAY "Enter Motorcycle Model: " WITH NO ADVANCING.
           ACCEPT WS-MODEL.
           DISPLAY "Enter License Plate: " WITH NO ADVANCING
           ACCEPT WS-PLATE
           PERFORM CHECK-LICENSE-PLATE
           IF VALID-INPUT = 'N'
               DISPLAY "Invalid License Plate!"
               EXIT PARAGRAPH
           END-IF.
           DISPLAY "Enter Motorcycle Color: " WITH NO ADVANCING.
           ACCEPT WS-COLOR.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           STRING WS-YEAR DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-MONTH DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-DAY DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-HOURS DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-MINUTES DELIMITED BY SIZE
                  INTO WS-DATE-TIME
           END-STRING.

           MOVE WS-STUDENT-NUMBER TO STUDENT-NUMBER.
           MOVE WS-STUDENT-NAME TO STUDENT-NAME.
           MOVE WS-MODEL TO MOTORCYCLE-MODEL.
           MOVE WS-PLATE TO LICENSE-PLATE.
           MOVE WS-COLOR TO MOTORCYCLE-COLOR.
           MOVE WS-DATE-TIME TO TIME-OF-ENTRY.
           MOVE SPACES TO TIME-OF-EXIT.

           WRITE PARKING-RECORD
           IF WS-FILE-STATUS NOT = "00"
               STRING "Write Error: " WS-FILE-STATUS
                   INTO WS-FILE-ERROR
               DISPLAY WS-FILE-ERROR
           ELSE
               DISPLAY "Parking Record Created Successfully."
           END-IF

           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           .

      * EDIT FUNCTION
       EDIT-PARKING.
           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           IF WS-FILE-STATUS NOT = "00"
               STRING "File Open Error: " WS-FILE-STATUS
                   INTO WS-FILE-ERROR
               DISPLAY WS-FILE-ERROR
               EXIT PARAGRAPH
           END-IF

           PERFORM FIND-RECORD
           IF RECORD-FOUND = 'N'
               DISPLAY "Record not found for the given Student Number."
           ELSE
               DISPLAY "Editing record..."
               DISPLAY "Enter New Student Name: " WITH NO ADVANCING
               ACCEPT WS-STUDENT-NAME
               DISPLAY "Enter New Motorcycle Model: " WITH NO ADVANCING
               ACCEPT WS-MODEL
               DISPLAY "Enter New License Plate: " WITH NO ADVANCING
               ACCEPT WS-PLATE
               PERFORM CHECK-LICENSE-PLATE
               IF VALID-INPUT = 'N'
                   DISPLAY "Invalid License Plate!"
                   EXIT PARAGRAPH
               END-IF
               DISPLAY "Enter New Motorcycle Color: " WITH NO ADVANCING
               ACCEPT WS-COLOR
               MOVE WS-STUDENT-NAME TO STUDENT-NAME
               MOVE WS-MODEL TO MOTORCYCLE-MODEL
               MOVE WS-PLATE TO LICENSE-PLATE
               MOVE WS-COLOR TO MOTORCYCLE-COLOR
               REWRITE PARKING-RECORD
               IF WS-FILE-STATUS NOT = "00"
                   STRING "Rewrite Error: " WS-FILE-STATUS
                       INTO WS-FILE-ERROR
                   DISPLAY WS-FILE-ERROR
               ELSE
                   DISPLAY "Record updated successfully."
               END-IF
           END-IF
           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           .

      * READ FUNCTION
       DISPLAY-PARKING-INFO.
           CLOSE PARKING-FILE
           OPEN INPUT PARKING-FILE
           PERFORM FIND-RECORD
           IF RECORD-FOUND = 'N'
               DISPLAY "Record not found for the given Student Number."
           ELSE
               DISPLAY "--- Parking Information ---"
               DISPLAY "Student Number: " STUDENT-NUMBER
               DISPLAY "Student Name: " STUDENT-NAME
               DISPLAY "Motorcycle Model: " MOTORCYCLE-MODEL
               DISPLAY "License Plate: " LICENSE-PLATE
               DISPLAY "Motorcycle Color: " MOTORCYCLE-COLOR
               DISPLAY "Time of Entry: " TIME-OF-ENTRY
               DISPLAY "Time of Exit: " TIME-OF-EXIT
           END-IF.
           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           .

      * CREATE FUNCTION
       EXIT-PARKING.
           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           PERFORM FIND-RECORD
           IF RECORD-FOUND = 'N'
               DISPLAY "Record not found for the given Student Number."
           ELSE
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
               STRING WS-YEAR DELIMITED BY SIZE
                      "-" DELIMITED BY SIZE
                      WS-MONTH DELIMITED BY SIZE
                      "-" DELIMITED BY SIZE
                      WS-DAY DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      WS-HOURS DELIMITED BY SIZE
                      ":" DELIMITED BY SIZE
                      WS-MINUTES DELIMITED BY SIZE
                      INTO WS-DATE-TIME
               END-STRING
               MOVE WS-DATE-TIME TO TIME-OF-EXIT
               REWRITE PARKING-RECORD
               DISPLAY "Exit time recorded successfully."
           END-IF.
           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           .

      * READ FUNCTION
       DISPLAY-ALL-DATA.
           CLOSE PARKING-FILE
           OPEN INPUT PARKING-FILE
           MOVE 'N' TO END-OF-FILE
           DISPLAY "--- Parking Records ---"
           PERFORM UNTIL END-OF-FILE = 'Y'
               READ PARKING-FILE
                   AT END
                       MOVE 'Y' TO END-OF-FILE
                   NOT AT END
                       DISPLAY STUDENT-NUMBER " | "
                               STUDENT-NAME " | "
                               MOTORCYCLE-MODEL " | "
                               LICENSE-PLATE " | "
                               MOTORCYCLE-COLOR " | "
                               TIME-OF-ENTRY " | "
                               TIME-OF-EXIT
               END-READ
           END-PERFORM
           CLOSE PARKING-FILE
           OPEN I-O PARKING-FILE
           .

      * READ FUNCTION
       FIND-RECORD.
           MOVE 'N' TO RECORD-FOUND
           MOVE 'N' TO END-OF-FILE
           DISPLAY "Enter Student Number: " WITH NO ADVANCING
           ACCEPT WS-STUDENT-NUMBER
           
           PERFORM UNTIL END-OF-FILE = 'Y' OR RECORD-FOUND = 'Y'
               READ PARKING-FILE NEXT RECORD
                   AT END
                       MOVE 'Y' TO END-OF-FILE
                   NOT AT END
                       IF STUDENT-NUMBER = WS-STUDENT-NUMBER
                           MOVE 'Y' TO RECORD-FOUND
                       END-IF
               END-READ
           END-PERFORM
           .

      * READ FUNCTION
       CHECK-STUDENT-NUMBER.
           IF FUNCTION LENGTH(WS-STUDENT-NUMBER) = 11
               MOVE "Y" TO VALID-INPUT
           ELSE
               MOVE "N" TO VALID-INPUT.

      * READ FUNCTION
       CHECK-LICENSE-PLATE.
           IF FUNCTION LENGTH(WS-PLATE) = 6
               MOVE "Y" TO VALID-INPUT
           ELSE
               MOVE "N" TO VALID-INPUT.


       TERMINATE-PROGRAM.
           CLOSE PARKING-FILE
           DISPLAY 
           "Thank you for using the PUP-Taguig Student Parking System!"
           .
