       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXTCADCEP.
       AUTHOR. LUCAS DE LIMA.
      ************************
      * RELATORIO DE CEP'S   *
      ************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS ENDERECO
                                   WITH DUPLICATES.
           SELECT CADCEPTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP           PIC 9(08).
                03 ENDERECO      PIC X(30).
                03 BAIRRO        PIC X(20).
                03 CIDADE        PIC X(20).
                03 UF            PIC X(02).
      *
       FD CADCEPTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEPTXT.DOC".
       01 REGCEPTX    PIC X(100).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
       01 CONLIN     PIC 9(03) VALUE ZEROS.
       01 CONLIN1    PIC 9(04) VALUE ZEROS.
      *
       01 CEPTX.
           03 CEP-NUMCEPTX    PIC 99999.999.
           03 FILLER          PIC X(01) VALUE " ".
           03 CEP-LOGRATX     PIC X(30).
           03 FILLER          PIC X(01) VALUE " ".
           03 BAIRROTX        PIC X(20).
           03 FILLER          PIC X(01) VALUE " ".
           03 CIDADETX        PIC X(20).
           03 FILLER          PIC X(01) VALUE " ".
           03 UFTX            PIC X(02).
       01 CAB0.
           03 FILLER      PIC X(20) VALUE "HOSPITAL MAINFRAME".
           03 FILLER      PIC X(30) VALUE "   *** RELACAO DE CEP ***".
       01 CAB1.
           03 CAB-NUMCEPTX    PIC X(09) VALUE "   CEP   ".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-LOGRATX     PIC X(30) VALUE "   LOGRADOURO".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-BAIRROTX    PIC X(20) VALUE "   BAIRRO".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-CIDADETX    PIC X(20) VALUE "   CIDADE".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-UFTX        PIC X(02) VALUE "UF".
       01 CAB2.
           03 CAB-NUMCEPTX    PIC X(09) VALUE "   ---   ".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-LOGRATX     PIC X(30) VALUE "   ----------".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-BAIRROTX    PIC X(20) VALUE "   ------".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-CIDADETX    PIC X(20) VALUE "   ------".
           03 FILLER          PIC X(01) VALUE " ".
           03 CAB-UFTX        PIC X(02) VALUE "--".
       01 TOTAL1.
           03 FILLER          PIC X(20) VALUE SPACES.
           03 FILLER          PIC X(22) VALUE "NUMERO DE REGISTROS : ".
           03 TOTREG          PIC ZZZ9.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TXTTELACEP.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** CONVERSAO DE ARQUIVO".
           05  LINE 02  COLUMN 41 
               VALUE  " DE CEP PARA TXT ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA IMPRES".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *---------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
           MOVE SPACES TO UF ENDERECO BAIRRO CIDADE.
           MOVE ZEROS  TO CEP.
           DISPLAY TXTTELACEP.
       INC-OPC.
           ACCEPT TW-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-FIM
           IF W-OPCAO = "N" OR "n"
               MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
           IF W-OPCAO NOT = "S" AND "s"
               MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CEP NAO EXISTE *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
      *
           OPEN OUTPUT CADCEPTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO CADCEPTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC.
           WRITE REGCEPTX FROM CAB0.
           MOVE SPACES TO REGCEPTX
           WRITE REGCEPTX.
           WRITE REGCEPTX FROM CAB1.
           WRITE REGCEPTX FROM CAB2.
      *
       LER-CEP01.
           READ CADCEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE SPACES TO REGCEPTX
                 WRITE REGCEPTX
                 MOVE CONLIN1 TO TOTREG
                 WRITE REGCEPTX FROM TOTAL1   
                 MOVE "*** FIM DO CADCEP ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA LEITURA CADCEP"   TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       INC-003.
           MOVE CEP TO CEP-NUMCEPTX.
           MOVE ENDERECO TO CEP-LOGRATX.
           MOVE BAIRRO TO BAIRROTX.
           MOVE CIDADE TO CIDADETX.
           MOVE UF TO UFTX.
           MOVE CEPTX TO REGCEPTX.
       INC-WR1.
           WRITE REGCEPTX
           IF ST-ERRO NOT = "00"
               MOVE "ERRO NA GRAVACAO DO ARQUIVO CADCEPTX" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
           MOVE "*** REGISTRO GRAVADOS *** " TO MENS
           ADD 1 TO CONLIN CONLIN1
           IF CONLIN < 34
               GO TO LER-CEP01
           ELSE
               MOVE ZEROS TO CONLIN
               GO TO ROT-CABEC.
      *
      * ROTINA DE FIM      *
      *
       ROT-FIM.
           DISPLAY (01, 01) ERASE
           CLOSE CADCEP CADCEPTX.
       ROT-FIMP.
           EXIT PROGRAM.
       ROT-FIMS.
           STOP RUN.
      *
      * ROTINA DE MENSAGEM *
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
               GO TO ROT-MENS2
           ELSE
               DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-TEMPO. 