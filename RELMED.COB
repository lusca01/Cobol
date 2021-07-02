
       01  CAB0.
           05  FILLER                 PIC X(057) VALUE
           "HOSPITAL MAINFRAME                          *** RELACAO D".
           05  FILLER                 PIC X(013) VALUE "E MEDICO'S***".

       01  DET1.
           05  FILLER               PIC X(005) VALUE "CRM: ".
           05  CRMTX  VALUE ZEROS   PIC 9(006).
           05  FILLER               PIC X(007) VALUE " NOME: ".
           05  NOMETX  VALUE SPACES PIC X(030).
           05  FILLER               PIC X(015) VALUE " ESPECIALIDADE ".
           05  ESPECIALIDADETX  VALUE ZEROS  PIC 9(001).
           05  FILLER               PIC X(006) VALUE " SEXO ".
           05  SEXOTX  VALUE SPACES PIC X(001).
           05  FILLER             PIC X(017) VALUE " DATA NASCIMENTO ".
           05  DIATX  VALUE ZEROS  PIC 9(002).
           05  FILLER               PIC X(001) VALUE " ".
           05  MESTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  ANOTX  VALUE ZEROS  PIC 9(004).

       01  DET2.
           05  FILLER                 PIC X(007) VALUE "EMAIL: ".
           05  EMAILTX  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(010) VALUE " TELEFONE ".
           05  DDDTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(003) VALUE " - ".
           05  NUMEROTX  VALUE ZEROS  PIC 9(009).

       01  TOTAL1.
           05  FILLER                 PIC X(052) VALUE
           "                           *** TOTAL DE REGISTROS : ".
           05  TOTREG  VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE " ***".
