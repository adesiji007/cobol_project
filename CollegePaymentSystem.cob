      ******************************************************************
      * Author: adesiji solomon
      * Date:30/07/2015
      * Purpose:HP JOB
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CollegePaymentSystem.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
       01 WS-STUDENT-DETAIL.
           02 WS-StudentName OCCURS 4 TIMES INDEXED BY S.
               03 WS-FIRSTNAME PIC X(15).
               03 WS-MIDDLEINITIAL PIC X(1).
               03 WS-Surname PIC X(15).

           02 WS-STUDENT-ID PIC 9(5) OCCURS 4 TIMES.

           02 WS-STUDENT-DOB  OCCURS 4 TIMES  .
               03 WS-DayOfBirth PIC 99.
               03 WS-FILLER PIC X VALUE '-'.
               03 WS-MonthOfBirth PIC 99.
               03 WS-FILLER PIC X VALUE '-'.
               03 WS-YearOfBirth PIC 9(4).
               03 WS-FILLER PIC X VALUE ' '.

           02 WS-STUDENT-ADDRESS  OCCURS 4 TIMES .
               03 WS-HOUSE-NUMBER PIC 9(3).
               03 WS-COUNTRY PIC X(15).
               03 WS-PINCODE PIC 9(6) VALUE 123456.


           02 WS-CourseDetails.
               03 WS-COURSE-CODE1 PIC X(8).
               03 WS-COURSE-CODE2 PIC X(8).
               03 WS-COURSE-CODE3 PIC X(8).
               03 WS-COURSE-CODE4 PIC X(8).

               03 WS-FILLER PIC X VALUE ' '.

               03 WS-COURSE-NAME1 PIC A(22).
               03 WS-COURSE-NAME2 PIC A(22).
               03 WS-COURSE-NAME3 PIC A(15).
               03 WS-COURSE-NAME4 PIC A(18).

               03 WS-FILLER PIC X VALUE ' '.


               03 WS-COURSE-FEES  PIC 9(4) OCCURS 4 TIMES.
               03 WS-COURSE-FEES1 PIC 9(4).
               03 WS-COURSE-FEES2 PIC 9(4).
               03 WS-COURSE-FEES3 PIC 9(4).
               03 WS-COURSE-FEES4 PIC 9(4).

           02 WS-TELEPHONE-NUMBER PIC 9(11) OCCURS 4 TIMES.
           02 WS-NUMBER PIC 9(2) OCCURS 4 TIMES.

           02 WS-HOUSEHOLD-SALARY OCCURS 2 TIMES INDEXED BY I.
               03 WS-HOUSEHOLD-INCOME PIC 9(4).
               03 WS-DISCOUNT PIC 9(2) VALUES 1.

       01 WS-STUDENT-DOB2 PIC X(10).

       01 WS-COURSE-DETAIL PIC X(8).

       01 WS-COURSE-DISCOUNT.

           02 WS-FAMILY-LOW-INCOME PIC 9(4).
           02 WS-FAMILY-MIDDLE-INCOME PIC 9(4).
           02 WS-FAMILY-HIGHER-INCOME PIC 9(4).

       01 WS-FAMILY-INCOME PIC 9(6).

       01 USER-PROMPT PIC 9(6).

       01 NUM PIC 9(1).
       01 DEPOSIT PIC 9(4).
       01 INDEX-CHK PIC 9(1) VALUE 1.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
       MOVE 01274635 TO WS-COURSE-CODE1
       MOVE 'Games Development' TO WS-COURSE-NAME1
       MOVE '£8000' TO WS-COURSE-FEES1

       MOVE 65746352 TO WS-COURSE-CODE2
       MOVE 'Computer systems Engineering' TO WS-COURSE-NAME2
       MOVE '£6000' TO WS-COURSE-FEES2

       MOVE 96752436 TO WS-COURSE-CODE3
       MOVE 'Networking' TO WS-COURSE-NAME3
       MOVE '£5000' TO WS-COURSE-FEES3

       MOVE 98614253 TO WS-COURSE-CODE4
       MOVE 'Web Development' TO WS-COURSE-NAME4
       MOVE '£3000' TO WS-COURSE-FEES4

       MOVE 10000 TO WS-FAMILY-LOW-INCOME
       MOVE 15000 TO WS-FAMILY-MIDDLE-INCOME
       MOVE 25000 TO WS-FAMILY-HIGHER-INCOME

       DISPLAY "****************************************** "
            DISPLAY "WELCOME TO STUDENT PAYMENT SYSTEM "
            DISPLAY "****************************************** "

           PERFORM UNTIL USER-PROMPT = 5
       DISPLAY "1. ADD STUDENT" " 2. MODIFY STUDENT"" 3. ADD A PAYMENT"
       " 4. RUN FEES REPORT" " 5. EXIT"
       DISPLAY "*******************************************************"
            ACCEPT USER-PROMPT
            IF(USER-PROMPT) = 1
                PERFORM ADD-STUDENT
            END-IF
            IF(USER-PROMPT = 2)
                PERFORM MODIFY-STUDENT
            END-IF
             IF(USER-PROMPT = 3)
                PERFORM ADD-PAYMENT
            END-IF
             IF(USER-PROMPT = 4)
                PERFORM RUN-FEES-REPORT
            END-IF
          END-PERFORM.
        DISPLAY "****************************************** "

        STOP RUN.

       ADD-STUDENT.
            DISPLAY "STUDENT RECORD "
            DISPLAY "*****************************************"

            DISPLAY "PLEASE ENTER YOUR FIRST NAME"
               ACCEPT WS-FIRSTNAME (S).
               DISPLAY 'STUDENT NAME: ' WS-FIRSTNAME (S).

            DISPLAY "PLEASE ENTER YOUR MIDDLE-INITIAL NAME"
               ACCEPT WS-MIDDLEINITIAL (S).
            DISPLAY 'MIDDLEINITIAL: ' WS-MIDDLEINITIAL (S).

            DISPLAY "PLEASE ENTER YOUR SURNAME"
               ACCEPT WS-Surname (S).
            DISPLAY 'WS-Surname: ' WS-Surname (S).

            DISPLAY "*******************************************"



            DISPLAY "PLEASE ENTER YOUR STUDENT-ID"
            DISPLAY "*******************************************"

            ACCEPT WS-STUDENT-ID(S).
            DISPLAY 'WS-STUDENT-ID: ' WS-STUDENT-ID(S).

            DISPLAY "PLEASE ENTER YOUR TELEPHONE-NUMBER"
            ACCEPT WS-TELEPHONE-NUMBER(S).
            DISPLAY 'TELEPHONE-NUMBER:' WS-TELEPHONE-NUMBER(S).

            DISPLAY "*******************************************"


            DISPLAY "PLEASE ENTER YOUR STUDENT-DOB '10-09-2015'"
            DISPLAY "*******************************************"
               ACCEPT WS-STUDENT-DOB2.
               UNSTRING WS-STUDENT-DOB2 DELIMITED BY SPACE
                 INTO WS-DayOfBirth(S), WS-MonthOfBirth(S),
                 WS-YearOfBirth(S)
               END-UNSTRING
            DISPLAY 'STUDENT-DOB: ' WS-STUDENT-DOB(S).
            DISPLAY '*******************************************T'



            DISPLAY 'STUDENT COURSE DETAIL'
            DISPLAY "*******************************************"
            DISPLAY "ENTER YOUR COURSE CODE"

            ACCEPT WS-COURSE-DETAIL

            DISPLAY "Course Name and Course Fees are:"
            IF WS-COURSE-CODE1 EQUALS WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME1,' ' WS-COURSE-FEES1
               END-IF.

            IF WS-COURSE-CODE2 = WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME2,' ' , WS-COURSE-FEES2
            END-IF.
            IF WS-COURSE-CODE3 = WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME3, ' ', WS-COURSE-FEES3
               END-IF.
            IF WS-COURSE-CODE4 = WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME4,' ' WS-COURSE-FEES4
               END-IF.

            ACCEPT WS-CourseDetails.
            DISPLAY "*******************************************"



            DISPLAY 'STUDENT DISCOUNT'
            DISPLAY "*******************************************"
            DISPLAY 'ENTER YOUR FAMILY INCOME'

            ACCEPT WS-FAMILY-INCOME.
            IF WS-FAMILY-INCOME < 10000
        COMPUTE WS-FAMILY-LOW-INCOME = WS-FAMILY-LOW-INCOME + 200
               DISPLAY 'Paymount amount for Low-Income £:'
               WS-FAMILY-LOW-INCOME
            END-IF.

            IF WS-FAMILY-INCOME < 15000
        COMPUTE WS-FAMILY-MIDDLE-INCOME = WS-FAMILY-MIDDLE-INCOME + 700
               DISPLAY 'Paymount amount for Middle-Income is:'
               WS-FAMILY-MIDDLE-INCOME
            END-IF.

            IF WS-FAMILY-INCOME < 25000
               DISPLAY 'Paymount amount for Higher-Income is:'
               WS-FAMILY-HIGHER-INCOME
            END-IF.
            ACCEPT WS-COURSE-DISCOUNT.
            ADD 1 TO INDEX-CHK.
       ADD 1 TO I.



       MODIFY-STUDENT.
            DISPLAY "*****************************************"
            DISPLAY "STUDENT RECORD "
            DISPLAY "*****************************************"

            DISPLAY "PLEASE ENTER YOUR FIRST NAME"
            ACCEPT WS-FIRSTNAME (S).
             DISPLAY 'STUDENT NAME: ' WS-FIRSTNAME (S).

            DISPLAY "PLEASE ENTER YOUR MIDDLE-INITIAL NAME"
            ACCEPT WS-MIDDLEINITIAL (S).
            DISPLAY 'MIDDLEINITIAL: ' WS-MIDDLEINITIAL (S).

            DISPLAY "PLEASE ENTER YOUR SURNAME"
            ACCEPT WS-Surname (S).
            DISPLAY 'WS-Surname: ' WS-Surname (S).

            DISPLAY "*******************************************"
            DISPLAY "PLEASE ENTER YOUR STUDENT-ID"
            DISPLAY "*******************************************"

            ACCEPT WS-STUDENT-ID(I).
            DISPLAY 'WS-STUDENT-ID: ' WS-STUDENT-ID(I).

            DISPLAY "*******************************************"
            DISPLAY "PLEASE ENTER YOUR STUDENT-DOB '10-09-2015'"
            DISPLAY "*******************************************"
            ACCEPT WS-STUDENT-DOB2.
            UNSTRING WS-STUDENT-DOB2 DELIMITED BY SPACE
            INTO WS-DayOfBirth(I), WS-MonthOfBirth(I),WS-YearOfBirth(I)
            END-UNSTRING
            DISPLAY 'STUDENT-DOB: ' WS-STUDENT-DOB(I).


            DISPLAY '*******************************************T'
            DISPLAY 'STUDENT COURSE DETAIL'
            DISPLAY "*******************************************"
            DISPLAY "ENTER YOUR COURSE CODE"

            ACCEPT WS-COURSE-DETAIL

            DISPLAY "Course Name and Course Fees are:"
            IF WS-COURSE-CODE1 EQUALS WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME1,' ' WS-COURSE-FEES1
            END-IF.

            IF WS-COURSE-CODE2 = WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME2, ' ' , WS-COURSE-FEES2
            END-IF.
            IF WS-COURSE-CODE3 = WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME3, ' ', WS-COURSE-FEES3
            END-IF.
            IF WS-COURSE-CODE4 = WS-COURSE-DETAIL
                DISPLAY WS-COURSE-NAME4,' ' WS-COURSE-FEES4
            END-IF.

            ACCEPT WS-CourseDetails.

            DISPLAY "*******************************************"

        DISPLAY "Family income"
        ACCEPT WS-FAMILY-INCOME.

            IF WS-FAMILY-INCOME <= 25000
               COMPUTE WS-NUMBER(I) = WS-COURSE-FEES1 -
               (WS-COURSE-FEES1/100 *10).
            IF WS-FAMILY-INCOME<= 15000
                COMPUTE WS-NUMBER(I) = WS-COURSE-FEES1 -
                (WS-COURSE-FEES1/100 *20).
             IF WS-FAMILY-INCOME <= 10000
                COMPUTE WS-NUMBER(I)  = WS-COURSE-FEES1 -
                (WS-COURSE-FEES1/100 *50).



       ADD-PAYMENT.

          DISPLAY "Enter user"
          ACCEPT NUM
          DISPLAY "HOW MUCH DEPOSIT"
          ACCEPT DEPOSIT
         COMPUTE WS-COURSE-FEES(NUM) = WS-COURSE-FEES(NUM)-DEPOSIT.



       RUN-FEES-REPORT.
           SET I TO 1
           PERFORM UNTIL I = INDEX-CHK
               DISPLAY 'WS-StudentName' WS-StudentName(I)
               DISPLAY 'WS-STUDENT-ID' WS-STUDENT-ID(I)
               DISPLAY 'WS-STUDENT-DOB' WS-STUDENT-DOB(I)
               DISPLAY 'WS-STUDENT-ADDRESS' WS-STUDENT-ADDRESS(I)
               DISPLAY 'WS-CourseDetails' WS-CourseDetails
               DISPLAY 'TELEPHONE-NUMBER' WS-TELEPHONE-NUMBER(I)
               ADD 1 TO I
           END-PERFORM.


      * STOP RUN.
      ** add other procedures here
       END PROGRAM CollegePaymentSystem.
