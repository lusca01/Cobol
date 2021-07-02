
       01  CAB0.
           05  FILLER                 PIC X(057) VALUE
           "HOSPITAL MAINFRAME                          *** RELACAO D".
           05  FILLER                 PIC X(011) VALUE
           "E CID'S ***".

       01  CAB1.
           05  FILLER                 PIC X(057) VALUE
           "CODIGO CID            DENOMINACAO               CLASSIFIC".
           05  FILLER                 PIC X(029) VALUE
           "ACAO    CONTAGIOSA   RARIDADE".

       01  CAB2.
           05  FILLER                 PIC X(057) VALUE
           "   ----      ------------------------------           -  ".
           05  FILLER                 PIC X(025) VALUE
           "            -           -".

       01  CIDTX.
           05  FILLER                 PIC X(003) VALUE
           "   ".
           05  CODIGOTX  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(006) VALUE
           "      ".
           05  DENOMINACAOTX  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(011) VALUE
           "           ".
           05  CLASSIFICACAOTX  VALUE ZEROS  PIC 9(001).
           05  FILLER                 PIC X(014) VALUE
           "              ".
           05  CONTAGIOSATX  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(011) VALUE
           "           ".
           05  RARIDADETX  VALUE SPACES PIC X(001).

       01  TOTAL1.
           05  FILLER                 PIC X(052) VALUE
           "                           *** TOTAL DE REGISTROS : ".
           05  VAR-0006  VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE
           " ***".
