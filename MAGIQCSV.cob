      ******************************************************************
      * PROGRAM NAME: MAGIQCSV                                         *
      * AUTHOR: JERIC JAY MALLARI                                      *
      *                                                                *
      * MAINTENANCE LOG                                                *
      * DATE     AUTHOR                MAINTENANCE REQUIREMENTS        *
      * -------- --------------------- ------------------------------- *
      * 18/01/24 JERIC JAY MALLARI     READ CSV AND DISPLAY WITHOUT    *
      *                                COMMA                           *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAGIQCSV.
       DATE-WRITTEN. 18/01/24
       DATE-COMPILED. 18/01/24
      ******************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

       SELECT CSVIN ASSIGN TO "INPUT.CSV"
           ORGANIZATION IS LINE SEQUENTIAL.

      ******************************************************************
       DATA DIVISION. 
       FILE SECTION. 

       FD  CSVIN.
       01  CSVIN-REC                           PIC X(80).

       WORKING-STORAGE SECTION. 

       01  WS-CSVIN-EOF-FLAG                   PIC X VALUE 'N'.
           88  WS-CSVIN-EOF                          VALUE 'Y'.
           88  WS-CSVIN-NOT-EOF                      VALUE 'N'.

       01  WS-INPUT-REC                        PIC X(80).

       01  WS-COUNTERS.
           05  WS-COUNTER1                     PIC 9(2) VALUE ZEROES.
           05  WS-COUNTER2                     PIC 9(2) VALUE ZEROES.

       01  WS-LINE.
           05  WS-LETTER OCCURS 80 TIMES       PIC X VALUE SPACE.

      ******************************************************************
       PROCEDURE DIVISION.

         0000-START.

           PERFORM 1000-INITIALIZATION.
           PERFORM 2000-MAIN UNTIL WS-CSVIN-EOF.
           PERFORM 9000-END.
         
         1000-INITIALIZATION.

           OPEN INPUT CSVIN.
           
           INITIALIZE WS-INPUT-REC
                      WS-COUNTERS
                      WS-LINE.

           PERFORM 1100-READ-CSVIN.

         1100-READ-CSVIN.

           READ CSVIN INTO WS-INPUT-REC
              AT END
                 MOVE 'Y' TO WS-CSVIN-EOF-FLAG
           END-READ.

         2000-MAIN.

           PERFORM VARYING WS-COUNTER1 FROM 0 BY 1 
              UNTIL WS-COUNTER1 = FUNCTION LENGTH(WS-INPUT-REC)

              IF WS-INPUT-REC(WS-COUNTER1:1) = ","
                 CONTINUE
              ELSE
                MOVE WS-INPUT-REC(WS-COUNTER1:1) 
                       TO WS-LETTER(WS-COUNTER2)
                 ADD 1 TO WS-COUNTER2
              END-IF

           END-PERFORM.

           DISPLAY WS-LINE.
           
           PERFORM 1100-READ-CSVIN.

           INITIALIZE WS-COUNTERS
                      WS-LINE.

         9000-END.
           
           CLOSE CSVIN.
           STOP RUN.

