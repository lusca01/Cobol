
       01  CAB0.
           05  FILLER               PIC X(057) VALUE
           "HOSPITAL MAINFRAME                          *** RELACAO D".
           05  FILLER               PIC X(015) VALUE "E CONSULTAS ***".

       01  DETCONSU.
           05  FILLER               PIC X(014) VALUE "CPF PACIENTE: ".
           05  CPFPACIENTETX  VALUE ZEROS  PIC 9(011).
           05  FILLER               PIC X(019) VALUE
           " DATA DA CONSULTA: ".
           05  DIACTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  MESCTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  ANOCTX  VALUE ZEROS  PIC 9(004).

       01  DETPACI.
           05  FILLER                 PIC X(006) VALUE "NOME: ".
           05  NOMEPACIENTETX  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(007) VALUE " SEXO: ".
           05  SEXOTX  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(018) VALUE
           " DATA NASCIMENTO: ".
           05  DIATX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  MESTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE " ".
           05  ANOTX  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(009) VALUE " GENERO: ".
           05  GENEROTX  VALUE SPACES PIC X(001).

       01  DETCONV.
           05  FILLER                 PIC X(010) VALUE "CONVENIO: ".
           05  CODIGOCONVTX  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(009) VALUE "  PLANO: ".
           05  PLANOTX  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(007) VALUE "  ANS: ".
           05  ANSTX  VALUE ZEROS  PIC 9(006).

       01  DETMED.
           05  FILLER              PIC X(005) VALUE "CRM: ".
           05  CRMTX  VALUE ZEROS  PIC 9(006).
           05  FILLER              PIC X(014) VALUE " NOME MEDICO: ".
           05  NOMEMEDICOTX  VALUE SPACES PIC X(030).
           05  FILLER              PIC X(007) VALUE " SEXO: ".
           05  SEXOMEDICOTX  VALUE SPACES PIC X(001).
           05  FILLER              PIC X(016) VALUE " ESPECIALIDADE: ".
           05  ESPECIALIDADETX  VALUE ZEROS  PIC 9(002).

       01  DETCID.
           05  FILLER              PIC X(005) VALUE "CID: ".
           05  CODIGOCIDTX  VALUE ZEROS  PIC 9(004).
           05  FILLER              PIC X(017) VALUE "  DENOMINAÇÃO: ".
           05  DENOMINACAOTX  VALUE SPACES PIC X(030).
           05  FILLER              PIC X(013) VALUE " CONTAGIOSA: ".
           05  CONTAGIOSATX  VALUE SPACES PIC X(001).
           05  FILLER              PIC X(012) VALUE "  RARIDADE: ".
           05  RARIDADETX  VALUE SPACES PIC X(001).
           05  FILLER              PIC X(016) VALUE " CLASSIFICACAO: ".
           05  CLASSIFICACAOTX  VALUE ZEROS  PIC 9(001).

       01  DETDESCRICAO.
           05  FILLER                PIC X(013) VALUE "DESCRICAO 1: ".
           05  DESCRICAO1TX  VALUE SPACES PIC X(030).
           05  FILLER                PIC X(014) VALUE " DESCRICAO 2: ".
           05  DESCRICAO2TX  VALUE SPACES PIC X(030).

       01  TOTAL1.
           05  FILLER                 PIC X(052) VALUE
           "                           *** TOTAL DE REGISTROS : ".
           05  TOTREG  VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE " ***".
