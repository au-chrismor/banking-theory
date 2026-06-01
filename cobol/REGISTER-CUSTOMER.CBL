       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-REGISTER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTFILE'
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
       RECORD CONTAINS 1000 CHARACTERS.

       01  CUSTOMER-RECORD.
           05    CUSTOMER-ID             PIC 9(10).
           05    CUSTOMER-NAME.
               10    LAST-NAME           PIC X(30).
               10    FIRST-NAME          PIC X(30).
               10    MIDDLE-NAME         PIC X(30).
           05    BUSINESS-NAME           PIC X(30).
           05    CUSTOMER-ADDRESS.
               10    LINE-1              PIC X(30).
               10    LINE-2              PIC X(30).
               10    LINE-3              PIC X(30).
               10    LINE-4              PIC X(30).
               10    CITY                PIC X(30).
               10    STATE               PIC X(5).
               10    POST-CODE           PIC X(10).
               10    COUNTRY             PIC X(30).
           05    CUSTOMER-PHONE.
               10    HOME-PHONE          PIC X(16).
               10    BUSINESS-PHONE      PIC X(16).
               10    MOBILE-PHONE        PIC X(16).
               10    HOME-FAX            PIC X(16).
               10    BUSINESS-FAX        PIC X(16).
               10    TELEX               PIC X(16).
           05    CUSTOMER-EMAIL.
               10    EMAIL-1             PIC X(256).
               10    EMAIL-2             PIC X(256).
           05    CUSTOMER-DOB.
               10    DOB-YEAR            PIC 9(4).
               10    DOB-MON             PIC 9(2).
               10    DOB-DAY             PIC 9(2).
           05    ACCOUNT-DATE.
               10    ACD-YEAR            PIC 9(4).
               10    ACD-MON             PIC 9(2).
               10    ACD-DAY             PIC 9(2).
           05    FILLER                  PIC X(51).

       WORKING-STORAGE SECTION.
       01  WS-OPTION                   PIC 9 VALUE 0.
       01  WS-CUSTOMER-ID              PIC 9(10).
       01  WS-LAST-NAME                PIC X(30).
       01  WS-FIRST-NAME               PIC X(30).
       01  WS-MIDDLE-NAME              PIC X(30).
       01  WS-LINE-1                   PIC X(30).
       01  WS-LINE-2                   PIC X(30).
       01  WS-LINE-3                   PIC X(30).
       01  WS-LINE-4                   PIC X(30).
       01  WS-CITY                     PIC X(30).
       01  WS-STATE                    PIC X(5).
       01  WS-POST-CODE                PIC X(10).
       01  WS-COUNTRY                  PIC X(30) VALUE 'AUSTRALIA'.
       01  WS-HOME-PHONE               PIC X(16) VALUE 'NONE'.
       01  WS-BUSINESS-PHONE           PIC X(16) VALUE 'NONE'.
       01  WS-MOBILE-PHONE             PIC X(16) VALUE 'NONE'.
       01  WS-HOME-FAX                 PIC X(16) VALUE 'NONE'.
       01  WS-BUSINESS-FAX             PIC X(16) VALUE 'NONE'.
       01  WS-TELEX                    PIC X(16) VALUE 'NONE'.
       01  WS-EMAIL-1                  PIC X(256) VALUE 'NONE'.
       01  WS-EMAIL-2                  PIC X(256) VALUE 'NONE'.
       01  WS-DOB-YEAR                 PIC 9(4).
       01  WS-DOB-MON                  PIC 9(2).
       01  WS-DOB-DAY                  PIC 9(2).
       01  WS-ACD-YEAR                 PIC 9(4).
       01  WS-ACD-MON                  PIC 9(2).
       01  WS-ACD-DAY                  PIC 9(2).
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
           OPEN INPUT CUSTOMER-FILE
           CLOSE CUSTOMER-FILE.
           
       DISPLAY-MENU.
           DISPLAY "CUSTOMER REGISTRATION"
           DISPLAY "1 - REGISTER A NEW CUSTOMER"
           DISPLAY "2 - VIEW ALL CUSTOMERS"
           DISPLAY "3 - EXIT"
           DISPLAY "ENTER COMMAND: "
           ACCEPT WS-OPTION.
           
       HANDLE-OPTION.
           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM REGISTER-CUSTOMER
               WHEN 2
                   PERFORM VIEW-CUSTOMERS
               WHEN 3
                   CONTINUE
               WHEN OTHER
                   DISPLAY "INVALID COMMAND"
           END-EVALUATE.
           
       REGISTER-CUSTOMER.
           DISPLAY "ENTER YEAR (4 DIGITS)"
           ACCEPT WS-ACD-YEAR
           DISPLAY "ENTER MONTH (2 DIGITS)"
           ACCEPT WS-ACD-MON
           DISPLAY "ENTER DAY (2 DIGITS)"
           ACCEPT WS-ACD-DAY
           DISPLAY "ENTER CUSTOMER NUMBER (10 DIGITS)"
           ACCEPT WS-CUSTOMER-ID
           DISPLAY "CUSTOMER NAME"
           DISPLAY "-------------"
           DISPLAY "ENTER LAST NAME"
           ACCEPT WS-LAST-NAME
           DISPLAY "ENTER FIRST NAME"
           ACCEPT WS-FIRST-NAME
           DISPLAY "ENTER MIDDLE NAME(S)"
           ACCEPT WS-MIDDLE-NAME
           DISPLAY "DATE OF BIRTH"
           DISPLAY "-------------"
           DISPLAY "ENTER BIRTH YEAR (YYYY)"
           ACCEPT WS-DOB-YEAR
           DISPLAY "ENTER BIRTH MONTH (MM)"
           ACCEPT WS-DOB-MON
           DISPLAY "ENTER BIRTH DAY (DD)"
           ACCEPT WS-DOB-DAY
           DISPLAY "ADDRESS"
           DISPLAY "-------"
           DISPLAY "LINE 1"
           ACCEPT WS-LINE-1
           DISPLAY "LINE 2"
           ACCEPT WS-LINE-2
           DISPLAY "LINE 3"
           ACCEPT WS-LINE-3
           DISPLAY "LINE 4"
           ACCEPT WS-LINE-4
           DISPLAY "CITY"
           ACCEPT WS-CITY
           DISPLAY "STATE"
           ACCEPT WS-STATE
           DISPLAY "POST CODE"
           ACCEPT WS-POST-CODE
           DISPLAY "COUNTRY"
           ACCEPT WS-COUNTRY
           DISPLAY "CONTACT DETAILS"
           DISPLAY "---------------"
           DISPLAY "HOME PHONE"
           ACCEPT WS-HOME-PHONE
           DISPLAY "BUSINESS PHONE"
           ACCEPT WS-BUSINESS-PHONE
           DISPLAY "MOBILE PHONE"
           ACCEPT WS-MOBILE-PHONE
           DISPLAY "BUSINESS FAX"
           ACCEPT WS-BUSINESS-FAX
           DISPLAY "HOME FAX"
           ACCEPT WS-HOME-FAX
           DISPLAY "TELEX"
           ACCEPT WS-TELEX
           DISPLAY "PRIMARY EMAIL"
           ACCEPT WS-EMAIL-1
           DISPLAY "SECONDARY EMAIL"
           ACCEPT WS-EMAIL-2
           
           MOVE WS-ACD-YEAR TO ACD-YEAR
           MOVE WS-ACD-MON TO ACD-MON
           MOVE WS-ACD-DAY TO ACD-DAY
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID
           MOVE WS-LAST-NAME TO LAST-NAME
           MOVE WS-FIRST-NAME TO FIRST-NAME
           MOVE WS-MIDDLE-NAME TO MIDDLE-NAME
           MOVE WS-DOB-YEAR TO DOB-YEAR
           MOVE WS-DOB-MON TO DOB-MON
           MOVE WS-DOB-DAY TO DOB-DAY
           MOVE WS-LINE-1 TO LINE-1
           MOVE WS-LINE-2 TO LINE-2
           MOVE WS-LINE-3 TO LINE-3
           MOVE WS-LINE-4 TO LINE-4
           MOVE WS-CITY TO CITY
           MOVE WS-STATE TO STATE
           MOVE WS-COUNTRY TO COUNTRY
           MOVE WS-POST-CODE TO POST-CODE
           MOVE WS-HOME-PHONE TO HOME-PHONE
           MOVE WS-BUSINESS-PHONE TO BUSINESS-PHONE
           MOVE WS-MOBILE-PHONE TO MOBILE-PHONE
           MOVE WS-BUSINESS-FAX TO BUSINESS-FAX
           MOVE WS-HOME-FAX TO HOME-FAX
           MOVE WS-TELEX TO TELEX
           MOVE WS-EMAIL-1 TO EMAIL-1
           MOVE WS-EMAIL-2 TO EMAIL-2

           OPEN EXTEND CUSTOMER-FILE
           WRITE CUSTOMER-RECORD
           CLOSE CUSTOMER-FILE.
           
       VIEW-CUSTOMERS.
           DISPLAY "REGISTERED CUSTOMERS"
           OPEN INPUT CUSTOMER-FILE
           
           PERFORM UNTIL WS-EOF-SWITCH = 'Y'
               READ CUSTOMER-FILE
                   AT END
                        MOVE 'Y' TO WS-EOF-SWITCH
                   NOT AT END
                       DISPLAY "CUSTOMER: ", CUSTOMER-ID
                       DISPLAY "CUSTOMER NAME:", LAST-NAME
                       DISPLAY "EMAIL: ", EMAIL-1
                       DISPLAY "--------------------------"
               END-READ
           END-PERFORM
           
           CLOSE CUSTOMER-FILE
           
           MOVE 'N' TO WS-EOF-SWITCH.
