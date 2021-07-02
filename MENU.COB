       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
      ****************************************
      * MENU DO SISTEMA DE CONSULTAS MEDICAS *
      ****************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC 9(02) VALUE ZEROS.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT       PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(50) VALUE SPACES.
       01 W-PROGR      PIC X(09) VALUE SPACES.
      *-----------------------------------------------------------------
       01 TAB-PROGR-R.
           03 FILLER PIC X(09) VALUE "SMP001".
           03 FILLER PIC X(09) VALUE "SMP002".
           03 FILLER PIC X(09) VALUE "SMP003".
           03 FILLER PIC X(09) VALUE "SMP004".
           03 FILLER PIC X(09) VALUE "SMP005".
           03 FILLER PIC X(09) VALUE "SMP006".
           03 FILLER PIC X(09) VALUE "MEDCONS".
           03 FILLER PIC X(09) VALUE "CIDCONS".
           03 FILLER PIC X(09) VALUE "CONVCONS".
           03 FILLER PIC X(09) VALUE "PACICONS".
           03 FILLER PIC X(09) VALUE "CEPCONS".
           03 FILLER PIC X(09) VALUE "CONSCONS".
           03 FILLER PIC X(09) VALUE "TXTMED".
           03 FILLER PIC X(09) VALUE "TXTCID".
           03 FILLER PIC X(09) VALUE "TXTCONV".
           03 FILLER PIC X(09) VALUE "TXTPACI".
           03 FILLER PIC X(09) VALUE "TXTCEP".
           03 FILLER PIC X(09) VALUE "TXTCONSU".
      *
       01 TAB-PROGR   REDEFINES TAB-PROGR-R.
          03 TAB-PROG  PIC X(09) OCCURS 18 TIMES.
      *--------------------------------------------------------------------
       SCREEN SECTION.
       01  TELAMENU.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                           SISTEMA DE CO".
           05  LINE 03  COLUMN 41 
               VALUE  "NSULTAS".
           05  LINE 06  COLUMN 01 
               VALUE  " 1 - CADASTRO DE MEDICOS     7 - CONSULT".
           05  LINE 06  COLUMN 41 
               VALUE  "A DE MEDICOS   13 - TXT DE MEDICOS".
           05  LINE 07  COLUMN 01 
               VALUE  " 2 - CADASTRO DE CID'S       8 - CONSULT".
           05  LINE 07  COLUMN 41 
               VALUE  "A DE CID'S     14 - TXT DE CID'S".
           05  LINE 08  COLUMN 01 
               VALUE  " 3 - CADASTRO DE CONVENIO    9 - CONSULT".
           05  LINE 08  COLUMN 41 
               VALUE  "A DE CONVENIO  15 - TXT DE CONVENIO".
           05  LINE 09  COLUMN 01 
               VALUE  " 4 - CADASTRO DE PACIENTES  10 - CONSULT".
           05  LINE 09  COLUMN 41 
               VALUE  "A DE PACIENTES 16 - TXT DE PACIENTES".
           05  LINE 10  COLUMN 01 
               VALUE  " 5 - CADASTRO DE CEP'S      11 - CONSULT".
           05  LINE 10  COLUMN 41 
               VALUE  "A DE CEP'S     17 - TXT DE CEP'S".
           05  LINE 11  COLUMN 01 
               VALUE  " 6 - CADASTRO DE CONSULTAS  12 - CONSULT".
           05  LINE 11  COLUMN 41 
               VALUE  "A DE CONSULTAS 18 - TXT DE CONSULTAS".
           05  LINE 16  COLUMN 01 
               VALUE  "                        OPCAO :    ( 00".
           05  LINE 16  COLUMN 41 
               VALUE  "- ENCERRA )".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  T-OPCAO
               LINE 16  COLUMN 33  PIC 9(02)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY TELAMENU.
           ACCEPT T-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-FIM.
           IF W-OPCAO = 00
               GO TO ROT-FIM.
           IF W-OPCAO > 18 OR W-OPCAO = SPACES
                 MOVE "*** FUNCAO NAO DISPONIVEL *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INICIO.
           MOVE TAB-PROG(W-OPCAO) TO W-PROGR.
              
      *
       ROT-EXEC.
           CALL W-PROGR ON OVERFLOW
              MOVE "* ERRO NA CHAMADA DO PROGRAMA SELECIONADO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ROT-EXEC1.
           CANCEL W-PROGR.
       ROT-EXEC1.
           GO TO INICIO.
      *
       ROT-FIM.
           EXIT PROGRAM.
       ROT-FIM1.
           DISPLAY (01, 01) ERASE.
           STOP RUN.
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23 12) LIMPA.
       ROT-MENS-FIM.
      *