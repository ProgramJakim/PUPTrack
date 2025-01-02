       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-PARKING-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARKING-FILE ASSIGN TO "C:\PARKING.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PARKING-FILE.
       01  DATABASE-RECORD.
           05  STUDENT-NUMBER     PIC X(20).
           05  STUDENT-NAME       PIC X(30).
           05  MOTORCYCLE-MODEL   PIC X(20).
           05  LICENSE-PLATE      PIC X(20).
           05  MOTORCYCLE-COLOR   PIC X(10).
           05  TIME-OF-ENTRY      PIC X(10).
           05  TIME-OF-EXIT       PIC X(10).

       WORKING-STORAGE SECTION.
       01  USER-INPUT.
           05  OPTION             PIC 9.
           05  STUDENT-NO         PIC X(20).
           05  STUDENT-SEARCH     PIC X(15).
           05  TEMP-NAME          PIC X(30).
           05  TEMP-MODEL         PIC X(20).
           05  TEMP-PLATE         PIC X(20).
           05  TEMP-COLOR         PIC X(10).
           05  TEMP-ENTRY         PIC X(10).
           05  TEMP-EXIT          PIC X(10).

       01  DATABASE.
           05  DB-INDEX           PIC 9(3) VALUE 0.
           05  MAX-RECORDS        PIC 9(3) VALUE 100.
           05  DATA-TABLE OCCURS 100 TIMES.
               10  DB-STUDENT-NUMBER  PIC X(20).
               10  DB-STUDENT-NAME    PIC X(30).
               10  DB-MODEL           PIC X(20).
               10  DB-PLATE           PIC X(20).
               10  DB-COLOR           PIC X(10).
               10  DB-ENTRY-TIME      PIC X(10).
               10  DB-EXIT-TIME       PIC X(10).

       01  FLAGS.
           05  RECORD-FOUND        PIC X VALUE 'N'.
           05  DATA-FULL           PIC X VALUE 'N'.
           05  EOF                 PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DATA
           PERFORM DISPLAY-MENU UNTIL OPTION = 6
           PERFORM SAVE-TO-FILE
           DISPLAY "THANK YOU FOR USING THE SYSTEM!".
           STOP RUN.

       INITIALIZE-DATA.
           DISPLAY "Loading existing records...".
           OPEN INPUT PARKING-FILE.
           PERFORM UNTIL EOF = 'Y'
               READ PARKING-FILE INTO DATABASE-RECORD
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       ADD 1 TO DB-INDEX
                      MOVE STUDENT-NUMBER TO DB-STUDENT-NUMBER(DB-INDEX)
                       MOVE STUDENT-NAME TO DB-STUDENT-NAME(DB-INDEX)
                       MOVE MOTORCYCLE-MODEL TO DB-MODEL(DB-INDEX)
                       MOVE LICENSE-PLATE TO DB-PLATE(DB-INDEX)
                       MOVE MOTORCYCLE-COLOR TO DB-COLOR(DB-INDEX)
                       MOVE TIME-OF-ENTRY TO DB-ENTRY-TIME(DB-INDEX)
                       MOVE TIME-OF-EXIT TO DB-EXIT-TIME(DB-INDEX)
               END-READ
           END-PERFORM.
           CLOSE PARKING-FILE.
           DISPLAY "Data loaded successfully!".

       DISPLAY-MENU.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "--- PUP-T STUDENT PARKING SYSTEM ---".
           DISPLAY "1 - New Parking Entry".
           DISPLAY "2 - Edit Parking Record".
           DISPLAY "3 - Display Record".
           DISPLAY "4 - Exit Parking".
           DISPLAY "5 - Display All Records".
           DISPLAY "6 - Exit Program".
           DISPLAY "Select an option: ".
           ACCEPT OPTION.
           EVALUATE OPTION
               WHEN 1
               PERFORM NEW-PARKING
               ACCEPT OMITTED
               WHEN 2
               PERFORM EDIT-RECORD
               ACCEPT OMITTED
               WHEN 3
               PERFORM DISPLAY-RECORD
               ACCEPT OMITTED
               WHEN 4
               PERFORM EXIT-PARKING
               ACCEPT OMITTED
               WHEN 5
               PERFORM DISPLAY-ALL-RECORDS
               ACCEPT OMITTED
               WHEN 6
               DISPLAY "Exiting program..."
               ACCEPT OMITTED
               WHEN OTHER
               DISPLAY "Invalid option. Please try again."
               ACCEPT OMITTED
           END-EVALUATE.

       NEW-PARKING.
           IF DB-INDEX = MAX-RECORDS
               DISPLAY "Database is full."
           ELSE
               ADD 1 TO DB-INDEX
               DISPLAY "Enter Student Number: " ACCEPT STUDENT-NO
               MOVE STUDENT-NO TO DB-STUDENT-NUMBER(DB-INDEX)
               DISPLAY "Enter Student Name: " ACCEPT TEMP-NAME
               MOVE TEMP-NAME TO DB-STUDENT-NAME(DB-INDEX)
               DISPLAY "Enter Motorcycle Model: " ACCEPT TEMP-MODEL
               MOVE TEMP-MODEL TO DB-MODEL(DB-INDEX)
               DISPLAY "Enter License Plate: " ACCEPT TEMP-PLATE
               MOVE TEMP-PLATE TO DB-PLATE(DB-INDEX)
               DISPLAY "Enter Motorcycle Color: " ACCEPT TEMP-COLOR
               MOVE TEMP-COLOR TO DB-COLOR(DB-INDEX)
               DISPLAY "Enter Time of Entry: " ACCEPT TEMP-ENTRY
               MOVE TEMP-ENTRY TO DB-ENTRY-TIME(DB-INDEX)
               DISPLAY "Record added successfully!"
           END-IF.

       EDIT-RECORD.
           DISPLAY "Enter Student Number to Edit: " ACCEPT
           STUDENT-SEARCH
           PERFORM SEARCH-RECORD
           IF RECORD-FOUND = 'Y'
               DISPLAY "Enter New Name: " ACCEPT TEMP-NAME
               MOVE TEMP-NAME TO DB-STUDENT-NAME(DB-INDEX)
               DISPLAY "Enter New Model: " ACCEPT TEMP-MODEL
               MOVE TEMP-MODEL TO DB-MODEL(DB-INDEX)
               DISPLAY "Enter New Plate: " ACCEPT TEMP-PLATE
               MOVE TEMP-PLATE TO DB-PLATE(DB-INDEX)
               DISPLAY "Enter New Color: " ACCEPT TEMP-COLOR
               MOVE TEMP-COLOR TO DB-COLOR(DB-INDEX)
               DISPLAY "Record updated successfully!"
           ELSE
               DISPLAY "Record not found."
           END-IF.

       DISPLAY-RECORD.
           DISPLAY "Enter Student Number to Display: "
           ACCEPT STUDENT-SEARCH
           PERFORM SEARCH-RECORD
           IF RECORD-FOUND = 'Y'
               DISPLAY "Student Number: " DB-STUDENT-NUMBER(DB-INDEX)
               DISPLAY "Student Name: " DB-STUDENT-NAME(DB-INDEX)
               DISPLAY "Model: " DB-MODEL(DB-INDEX)
               DISPLAY "Plate: " DB-PLATE(DB-INDEX)
               DISPLAY "Color: " DB-COLOR(DB-INDEX)
               DISPLAY "Entry: " DB-ENTRY-TIME(DB-INDEX)
               DISPLAY "Exit: " DB-EXIT-TIME(DB-INDEX)
           ELSE
               DISPLAY "Record not found."
           END-IF.

       DISPLAY-ALL-RECORDS.
           PERFORM VARYING DB-INDEX FROM 1 BY 1 UNTIL DB-INDEX >
           MAX-RECORDS
               IF DB-STUDENT-NUMBER(DB-INDEX) NOT = SPACES
                  DISPLAY "Student Number: " DB-STUDENT-NUMBER(DB-INDEX)
                   DISPLAY "Name: " DB-STUDENT-NAME(DB-INDEX)
                   DISPLAY "Model: " DB-MODEL(DB-INDEX)
                   DISPLAY "Plate: " DB-PLATE(DB-INDEX)
                   DISPLAY "Color: " DB-COLOR(DB-INDEX)
                   DISPLAY "Entry: " DB-ENTRY-TIME(DB-INDEX)
                   DISPLAY "Exit: " DB-EXIT-TIME(DB-INDEX)
               END-IF
           END-PERFORM.

       EXIT-PARKING.
           DISPLAY "Enter Student Number to Exit: "
           ACCEPT STUDENT-SEARCH
           PERFORM SEARCH-RECORD
           IF RECORD-FOUND = 'Y'
               DISPLAY "Enter Exit Time: " ACCEPT TEMP-EXIT
               MOVE TEMP-EXIT TO DB-EXIT-TIME(DB-INDEX)
               DISPLAY "Exit recorded successfully!"
           ELSE
               DISPLAY "Record not found."
           END-IF.

       SEARCH-RECORD.
           MOVE 'N' TO RECORD-FOUND
           PERFORM VARYING DB-INDEX FROM 1 BY 1 UNTIL DB-INDEX >
           MAX-RECORDS
               IF STUDENT-SEARCH = DB-STUDENT-NUMBER(DB-INDEX)
                   MOVE 'Y' TO RECORD-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SAVE-TO-FILE.
           DISPLAY "Saving records to file..."
           OPEN OUTPUT PARKING-FILE
           PERFORM VARYING DB-INDEX FROM 1 BY 1 UNTIL DB-INDEX >
           MAX-RECORDS
               IF DB-STUDENT-NUMBER(DB-INDEX) NOT = SPACES
                   MOVE DB-STUDENT-NUMBER(DB-INDEX) TO STUDENT-NUMBER
                   MOVE DB-STUDENT-NAME(DB-INDEX) TO STUDENT-NAME
                   MOVE DB-MODEL(DB-INDEX) TO MOTORCYCLE-MODEL
                   MOVE DB-PLATE(DB-INDEX) TO LICENSE-PLATE
                   MOVE DB-COLOR(DB-INDEX) TO MOTORCYCLE-COLOR
                   MOVE DB-ENTRY-TIME(DB-INDEX) TO TIME-OF-ENTRY
                   MOVE DB-EXIT-TIME(DB-INDEX) TO TIME-OF-EXIT
                   WRITE DATABASE-RECORD
                   DISPLAY "Record written: " STUDENT-NUMBER
               END-IF
           END-PERFORM.
           CLOSE PARKING-FILE.
           DISPLAY "Records saved successfully!".
