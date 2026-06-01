       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT-REGISTER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCTFILE'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE
       RECORD CONTAINS 100 CHARACTERS.

       01  ACCOUNT-RECORD.
           05    ACCOUNT-ID              PIC 9(10).
           05    ACCOUNT-NAME            PIC X(30).
           05    CUSTOMER-ID.
               10    PRIMARY-ID          PIC 9(10).
               10    SECONDARY-ID        PIC 9(10).
           05    ACCOUNT-CREATE-DATE.
               10    ACD-YEAR            PIC 9(4).
               10    ACD-MON             PIC 9(2).
               10    ACD-DAY             PIC 9(2).
           05    CURR-BALANCE            PIC 9(12)V99.
           05    FILLER                  PIC X(18).

       WORKING-STORAGE SECTION.
       01  WS-OPTION                   PIC 9 VALUE 0.
       01  WS-ACCOUNT-ID               PIC 9(10).
       01  WS-ACCOUNT-NAME             PIC X(30).
       01  WS-PRIMARY-ID               PIC 9(10).
       01  WS-SECONDARY-ID             PIC 9(10) VALUE ZEROES.
       01  WS-ACD-YEAR                 PIC 9(4).
       01  WS-ACD-MON                  PIC 9(2).
       01  WS-ACD-DAY                  PIC 9(2).
       01  WS-CURR-BALANCE             PIC 9(12)V99.
       01  WS-EOF-SWITCH               PIC X  VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CHECK-FILE.
           PERFORM UNTIL WS-OPTION = 3
               PERFORM DISPLAY-MENU
               PERFORM HANDLE-OPTION
           END-PERFORM
           
           DISPLAY "GOODBYE"
           STOP RUN.
           
       CHECK-FILE.
           OPEN INPUT ACCOUNT-FILE
           CLOSE ACCOUNT-FILE.
           
       DISPLAY-MENU.
           DISPLAY "ACCOUNT REGISTRATION"
           DISPLAY "1 - REGISTER A NEW ACCOUNT"
           DISPLAY "2 - VIEW ALL ACCOUNTS"
           DISPLAY "3 - EXIT"
           DISPLAY "ENTER COMMAND: "
           ACCEPT WS-OPTION.
           
       HANDLE-OPTION.
           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM REGISTER-ACCOUNT
               WHEN 2
                   PERFORM VIEW-ACCOUNTS
               WHEN 3
                   CONTINUE
               WHEN OTHER
                   DISPLAY "INVALID COMMAND"
           END-EVALUATE.
           
       REGISTER-ACCOUNT.
           DISPLAY "ENTER YEAR (4 DIGITS)"
           ACCEPT WS-ACD-YEAR
           DISPLAY "ENTER MONTH (2 DIGITS)"
           ACCEPT WS-ACD-MON
           DISPLAY "ENTER DAY (2 DIGITS)"
           ACCEPT WS-ACD-DAY
           DISPLAY "ENTER ACCOUNT NUMBER (10 DIGITS)"
           ACCEPT WS-ACCOUNT-ID
           DISPLAY "ENTER ACCOUNT NAME"
           ACCEPT WS-ACCOUNT-NAME
           DISPLAY "ENTER INITIAL BALANCE"
           ACCEPT WS-CURR-BALANCE
           
           MOVE WS-ACD-YEAR TO ACD-YEAR
           MOVE WS-ACD-MON TO ACD-MON
           MOVE WS-ACD-DAY TO ACD-DAY
           MOVE WS-ACCOUNT-ID TO ACCOUNT-ID
           MOVE WS-ACCOUNT-NAME TO ACCOUNT-NAME
           MOVE WS-CURR-BALANCE TO CURR-BALANCE
           
           OPEN EXTEND ACCOUNT-FILE
           WRITE ACCOUNT-RECORD
           CLOSE ACCOUNT-FILE.
           
       VIEW-ACCOUNTS.
           DISPLAY "REGISTERED ACCOUNTS"
           OPEN INPUT ACCOUNT-FILE
           
           PERFORM UNTIL WS-EOF-SWITCH = 'Y'
               READ ACCOUNT-FILE
                   AT END
                        MOVE 'Y' TO WS-EOF-SWITCH
                   NOT AT END
                       DISPLAY "ACCOUNT NUMBER: ", ACCOUNT-ID
                       DISPLAY "ACCOUNT NAME:", ACCOUNT-NAME
                       DISPLAY "BALANCE: ", CURR-BALANCE
                       DISPLAY "-----------------------------"
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNT-FILE
           
           MOVE 'N' TO WS-EOF-SWITCH.
