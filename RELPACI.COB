
       01  CAB0.
           05  FILLER                 PIC X(057) VALUE
           "HOSPITAL MAINFRAME                          *** RELACAO D".
           05  FILLER                 PIC X(014) VALUE
           "E PACIENTES***".

       01  DETPACI1.
           05  FILLER                 PIC X(005) VALUE "CPF: ".
           05  CPFTX  VALUE ZEROS  PIC 9(011).
           05  FILLER                 PIC X(007) VALUE " NOME: ".
           05  NOMETX  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(017) VALUE
           " DATA NACIMENTO: ".
           05  DIATX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  MESTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  ANOTX  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(007) VALUE " SEXO: ".
           05  SEXOTX  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(009) VALUE " GENERO: ".
           05  GENEROTX  VALUE SPACES PIC X(001).

       01  DETPACI2.
           05  FILLER                 PIC X(010) VALUE  "TELEFONE: ".
           05  DDDTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  TNUMTX  VALUE ZEROS  PIC 9(009).
           05  FILLER                 PIC X(009) VALUE "  EMAIL: ".
           05  EMAILTX  VALUE SPACES PIC X(030).

       01  DETCEP.
           05  FILLER                 PIC X(005) VALUE "CEP: ".
           05  CEPTX  VALUE ZEROS  PIC 9(008).
           05  FILLER                 PIC X(011) VALUE " ENDERECO: ".
           05  ENDERECOTX  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(004) VALUE " N: ".
           05  NUMEROTX  VALUE SPACES PIC X(005).
           05  FILLER                 PIC X(009) VALUE " BAIRRO: ".
           05  BAIRROTX  VALUE SPACES PIC X(020).
           05  FILLER                 PIC X(005) VALUE " UF: ".
           05  UFTX  VALUE SPACES PIC X(002).

       01  DETCONV.
           05  FILLER                 PIC X(014) VALUE
           "COD CONVENIO: ".
           05  CODIGOTX  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(008) VALUE " PLANO: ".
           05  PLANOTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(006) VALUE " ANS: ".
           05  ANSTX  VALUE ZEROS  PIC 9(006).

       01  TOTAL1.
           05  FILLER                 PIC X(052) VALUE
           "                           *** TOTAL DE REGISTROS : ".
           05  TOTREG  VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE " ***".
