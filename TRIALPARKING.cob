       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARKINGSYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE
        ASSIGN TO "C:\KIMCOBOL\PARKING.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.
           SELECT TEMP-FILE
         ASSIGN TO "C:\KIMCOBOL\PARKING.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STU-NUMBER     PIC X(15).
           05 STU-NAME       PIC X(30).
           05 STU-MODEL      PIC X(20).
           05 STU-PLATE      PIC X(10).
           05 STU-COLOR      PIC X(10).
           05 STU-ENTRY      PIC X(8).
           05 STU-EXIT       PIC X(8).

       FD TEMP-FILE.
       01 TEMP-RECORD PIC X(103).

       WORKING-STORAGE SECTION.
       01 OPTION PIC 9 VALUE 0.
       01 STUDENT-NUMBER PIC X(15).
       01 STUDENT-NAME PIC X(30).
       01 CAR-MODEL PIC X(20).
       01 PLATE-NUMBER PIC X(10).
       01 CAR-COLOR PIC X(10).
       01 ENTRY-TIME PIC X(8).
       01 EXIT-TIME PIC X(8).
       01 EOF PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL OPTION = 6
               DISPLAY '--- PUP-T STUDENT PARKING SYSTEM ---'
               DISPLAY '1 - New Parking Entry'
               DISPLAY '2 - Edit Parking Record'
               DISPLAY '3 - Display Record'
               DISPLAY '4 - Exit Parking'
               DISPLAY '5 - Display All Records'
               DISPLAY '6 - Exit Program'
               DISPLAY 'Select an option: '
               ACCEPT OPTION

               EVALUATE OPTION
                   WHEN 1
                       PERFORM NEW-PARKING-ENTRY
                   WHEN 2
                       PERFORM EDIT-PARKING-RECORD
                   WHEN 3
                       PERFORM DISPLAY-RECORD
                   WHEN 4
                       PERFORM EXIT-PARKING
                   WHEN 5
                       PERFORM DISPLAY-ALL-RECORDS
                   WHEN 6
                       PERFORM EXIT-PROGRAM
                   WHEN OTHER
                       DISPLAY 'Invalid option. Please try again.'
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       NEW-PARKING-ENTRY.
           DISPLAY 'Enter Student Number (max 15 characters): '
           ACCEPT STUDENT-NUMBER
           DISPLAY 'Enter Name: '
           ACCEPT STUDENT-NAME
           DISPLAY 'Enter Car Model: '
           ACCEPT CAR-MODEL
           DISPLAY 'Enter Plate Number: '
           ACCEPT PLATE-NUMBER
           DISPLAY 'Enter Car Color: '
           ACCEPT CAR-COLOR
           DISPLAY 'Enter Entry Time (HHMM): '
           ACCEPT ENTRY-TIME
           DISPLAY 'Enter Exit Time (HHMM): '
           ACCEPT EXIT-TIME
           OPEN OUTPUT STUDENT-FILE
           MOVE STUDENT-NUMBER TO STU-NUMBER
           MOVE STUDENT-NAME TO STU-NAME
           MOVE CAR-MODEL TO STU-MODEL
           MOVE PLATE-NUMBER TO STU-PLATE
           MOVE CAR-COLOR TO STU-COLOR
           MOVE ENTRY-TIME TO STU-ENTRY
           MOVE EXIT-TIME TO STU-EXIT
           WRITE STUDENT-RECORD
           CLOSE STUDENT-FILE
           DISPLAY 'Parking entry recorded successfully.'.

       EDIT-PARKING-RECORD.
           OPEN INPUT STUDENT-FILE
           OPEN OUTPUT TEMP-FILE
           DISPLAY 'Enter Student Number to edit: '
           ACCEPT STUDENT-NUMBER
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ STUDENT-FILE AT END MOVE 'Y' TO EOF
               NOT AT END
                   IF STU-NUMBER = STUDENT-NUMBER
                       DISPLAY 'Record Found. Enter new details.'
                       DISPLAY 'Enter Name: '
                       ACCEPT STUDENT-NAME
                       DISPLAY 'Enter Car Model: '
                       ACCEPT CAR-MODEL
                       DISPLAY 'Enter Plate Number: '
                       ACCEPT PLATE-NUMBER
                       DISPLAY 'Enter Car Color: '
                       ACCEPT CAR-COLOR
                       DISPLAY 'Enter Entry Time (HHMM): '
                       ACCEPT ENTRY-TIME
                       DISPLAY 'Enter Exit Time (HHMM): '
                       ACCEPT EXIT-TIME
                       MOVE STUDENT-NAME TO STU-NAME
                       MOVE CAR-MODEL TO STU-MODEL
                       MOVE PLATE-NUMBER TO STU-PLATE
                       MOVE CAR-COLOR TO STU-COLOR
                       MOVE ENTRY-TIME TO STU-ENTRY
                       MOVE EXIT-TIME TO STU-EXIT
                   END-IF
                   WRITE TEMP-RECORD FROM STUDENT-RECORD
           END-PERFORM
           CLOSE STUDENT-FILE
           CLOSE TEMP-FILE

           OPEN OUTPUT STUDENT-FILE
           OPEN INPUT TEMP-FILE
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ TEMP-FILE AT END MOVE 'Y' TO EOF
               NOT AT END
                   WRITE STUDENT-RECORD FROM TEMP-RECORD
           END-PERFORM
           CLOSE TEMP-FILE
           CLOSE STUDENT-FILE
           DISPLAY 'Record updated successfully.'.

       DISPLAY-RECORD.
           OPEN INPUT STUDENT-FILE
           DISPLAY 'Enter Student Number to display: '
           ACCEPT STUDENT-NUMBER
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ STUDENT-FILE AT END MOVE 'Y' TO EOF
               NOT AT END
                   IF STU-NUMBER = STUDENT-NUMBER
                       DISPLAY 'Student Number: ' STU-NUMBER
                       DISPLAY 'Name: ' STU-NAME
                       DISPLAY 'Model: ' STU-MODEL
                       DISPLAY 'Plate: ' STU-PLATE
                       DISPLAY 'Color: ' STU-COLOR
                       DISPLAY 'Entry: ' STU-ENTRY
                       DISPLAY 'Exit: ' STU-EXIT
                       MOVE 'Y' TO EOF
                   END-IF
           END-PERFORM
           CLOSE STUDENT-FILE.

       EXIT-PARKING.
           PERFORM EDIT-PARKING-RECORD
           DISPLAY 'Exit parking recorded successfully.'.

       DISPLAY-ALL-RECORDS.
           OPEN INPUT STUDENT-FILE
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ STUDENT-FILE AT END MOVE 'Y' TO EOF
               NOT AT END
                   DISPLAY 'Student Number: ' STU-NUMBER
                   DISPLAY 'Name: ' STU-NAME
                   DISPLAY 'Model: ' STU-MODEL
                   DISPLAY 'Plate: ' STU-PLATE
                   DISPLAY 'Color: ' STU-COLOR
                   DISPLAY 'Entry: ' STU-ENTRY
                   DISPLAY 'Exit: ' STU-EXIT
                   DISPLAY '-----------------------------'
           END-PERFORM
           CLOSE STUDENT-FILE.

       EXIT-PROGRAM.
           DISPLAY 'Exiting Program...'
           STOP RUN.
