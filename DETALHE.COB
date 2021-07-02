
       01  LINHA-001.
           05  FILLER                 PIC X(057) VALUE
           "HOSPITAL MAINFRAME                          *** RELACAO D".
           05  FILLER                 PIC X(015) VALUE
           "E CONSULTAS ***".

       01  LINHA-002.
           05  FILLER                 PIC X(014) VALUE
           "CPF PACIENTE: ".
           05  VAR-0001  VALUE ZEROS  PIC 9(011).
           05  FILLER                 PIC X(019) VALUE
           " DATA DA CONSULTA: ".
           05  VAR-0002  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE
           " ".
           05  VAR-0003  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE
           " ".
           05  VAR-0004  VALUE ZEROS  PIC 9(004).

       01  LINHA-003.
           05  FILLER                 PIC X(006) VALUE
           "NOME: ".
           05  VAR-0005  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(007) VALUE
           " SEXO: ".
           05  VAR-0006  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(018) VALUE
           " DATA NASCIMENTO: ".
           05  VAR-0007  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE
           " ".
           05  VAR-0008  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(001) VALUE
           " ".
           05  VAR-0009  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(009) VALUE
           " GENERO: ".
           05  VAR-0010  VALUE SPACES PIC X(001).

       01  LINHA-004.
           05  FILLER                 PIC X(010) VALUE
           "CONVENIO: ".
           05  VAR-0011  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(009) VALUE
           "  PLANO: ".
           05  VAR-0012  VALUE ZEROS  PIC 9(002).
           05  FILLER                 PIC X(007) VALUE
           "  ANS: ".
           05  VAR-0013  VALUE ZEROS  PIC 9(006).

       01  LINHA-005.
           05  FILLER                 PIC X(005) VALUE
           "CRM: ".
           05  VAR-0014  VALUE ZEROS  PIC 9(006).
           05  FILLER                 PIC X(014) VALUE
           " NOME MEDICO: ".
           05  VAR-0015  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(007) VALUE
           " SEXO: ".
           05  VAR-0016  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(016) VALUE
           " ESPECIALIDADE: ".
           05  VAR-0017  VALUE ZEROS  PIC 9(002).

       01  LINHA-006.
           05  FILLER                 PIC X(005) VALUE
           "CID: ".
           05  VAR-0018  VALUE ZEROS  PIC 9(004).
           05  FILLER                 PIC X(017) VALUE
           "  DENOMINAÇÃO: ".
           05  VAR-0019  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(013) VALUE
           " CONTAGIOSA: ".
           05  VAR-0020  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(012) VALUE
           "  RARIDADE: ".
           05  VAR-0021  VALUE SPACES PIC X(001).
           05  FILLER                 PIC X(016) VALUE
           " CLASSIFICACAO: ".
           05  VAR-0022  VALUE ZEROS  PIC 9(001).

       01  LINHA-007.
           05  FILLER                 PIC X(013) VALUE
           "DESCRICAO 1: ".
           05  VAR-0023  VALUE SPACES PIC X(030).
           05  FILLER                 PIC X(014) VALUE
           " DESCRICAO 2: ".
           05  VAR-0024  VALUE SPACES PIC X(030).

       01  LINHA-008.
           05  FILLER                 PIC X(052) VALUE
           "                           *** TOTAL DE REGISTROS : ".
           05  VAR-0025  VALUE ZEROS  PIC ZZZ9.
           05  FILLER                 PIC X(004) VALUE
           " ***".
