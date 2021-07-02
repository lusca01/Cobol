
       01  CAB0.
           05  FILLER                 PIC X(057) VALUE
           "HOSPITAL MAINFRAME                          *** RELACAO D".
           05  FILLER                 PIC X(015) VALUE
           "E CONVENIOS ***".

       01  CAB1.
           05  FILLER                 PIC X(057) VALUE
           "  CODIGO               NOME                  PLANO    ANS".

       01  CAB2.
           05  FILLER                 PIC X(057) VALUE
           "   ----   ------------------------------     -----   ----".
           05  FILLER                 PIC X(002) VALUE
           "--".

       01  CONVTX.
           05  FILLER                 PIC X(003) VALUE "   ".
           05  CODIGOTX  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(003) VALUE "   ".
           05  NOMETX  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(007) VALUE "       ".
           05  PLANO  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(004) VALUE "    ".
           05  ANS  VALUE ZEROS  PIC 9(006).

       01  TOTAL1.
           05  FILLER                 PIC X(052) VALUE
           "                           *** TOTAL DE REGISTROS : ".
           05  TOTREG  VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE " ***".
