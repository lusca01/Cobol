       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
      *AUTHOR. LUCAS DE LIMA.
      *****************************************************************
      *    CADASTRO DE CONVENIO                                      *
      *****************************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.     
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01  REGCONV.
           03 CODIGO              PIC 9(04).
           03 NOME                PIC X(30).
           03 PLANO               PIC 9(02).
           03 ANS                 PIC 9(06).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       77 ST-ERRO       PIC X(02) VALUE "00".
       77 IND           PIC 9(01) VALUE ZEROS.
       01 MSGPLANO      PIC X(30) VALUE SPACES.
      *
       01 TABPLANO.
          03 TBPLANO     PIC X(30) OCCURS 12 TIMES.
      *------------------------------------
       SCREEN SECTION.
       01  TELACONV.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                      CADASTRO DO CONVEN".
           05  LINE 02  COLUMN 41 
               VALUE  "IO".
           05  LINE 04  COLUMN 01 
               VALUE  "  CODIGO    :".
           05  LINE 06  COLUMN 01 
               VALUE  "  NOME      :".
           05  LINE 08  COLUMN 01 
               VALUE  "  PLANO     :".
           05  LINE 10  COLUMN 01 
               VALUE  "  CODIGO ANS:".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  CODIGOT
               LINE 04  COLUMN 15  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  NOMET
               LINE 06  COLUMN 15  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  PLANOT
               LINE 08  COLUMN 15  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  PLANOTT
               LINE 08  COLUMN 18  PIC X(30)
               USING  MSGPLANO
               HIGHLIGHT.
           05  ANST
               LINE 10  COLUMN 15  PIC 9(06)
               USING  ANS
               HIGHLIGHT.
      *
       01  TELAPLANOS.
           05  LINE 07  COLUMN 41 
               VALUE  "         1 - ENFERMARIA REGIONAL".
           05  LINE 08  COLUMN 41 
               VALUE  "         2 - ENFERMARIA NACIONAL".
           05  LINE 09  COLUMN 41 
               VALUE  "         3 - ENFERMARIA INTERNACIONAL".
           05  LINE 10  COLUMN 41 
               VALUE  "         4 - APTO PADRAO REGIONAL".
           05  LINE 11  COLUMN 41 
               VALUE  "         5 - APTO PADRAO NACIONAL".
           05  LINE 12  COLUMN 41 
               VALUE  "         6 - APTO PADRAO INTERNACIONAL".
           05  LINE 13  COLUMN 41 
               VALUE  "         7 - EMERGENCIA REGIONAL".
           05  LINE 14  COLUMN 41 
               VALUE  "         8 - EMERGENCIA NACIONAL".
           05  LINE 15  COLUMN 41 
               VALUE  "         9 - EMERGENCIA INTERNACIONAL".
           05  LINE 16  COLUMN 41 
               VALUE  "         10 - PLANO GLOBAL".
           05  LINE 17  COLUMN 41 
               VALUE  "         11 - PLANO INDIVIDUAL".
           05  LINE 18  COLUMN 41 
               VALUE  "         12 - PLANO FAMILIAR".
      *-----------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "ENFERMARIA REGIONAL"       TO TBPLANO(01)
           MOVE "ENFERMARIA NACIONAL"       TO TBPLANO(02)
           MOVE "ENFERMARIA INTERNACIONAL"  TO TBPLANO(03)
           MOVE "APTO PADRAO REGIONAL"      TO TBPLANO(04)
           MOVE "APTO PADRAO NACIONAL"      TO TBPLANO(05)
           MOVE "APTO PADRAO INTERNACIONAL" TO TBPLANO(06)
           MOVE "EMERGENCIA REGIONAL"       TO TBPLANO(07)
           MOVE "EMERGENCIA NACIONAL"       TO TBPLANO(08)
           MOVE "EMERGENCIA INTERNACIONAL"  TO TBPLANO(09)
           MOVE "PLANO GLOBAL"              TO TBPLANO(10)
           MOVE "PLANO INDIVIDUAL"          TO TBPLANO(11)
           MOVE "PLANO FAMILIAR"            TO TBPLANO(12).
      *
       L0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
                OPEN OUTPUT CADCONV
                CLOSE CADCONV
                MOVE " ARQUIVO CADCONVENIO FOI CRIADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO L0
             ELSE
                MOVE " ERRO NA ABERTURA DO ARQUIVO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
        L1.
           MOVE SPACES TO NOME MSGPLANO
           MOVE ZEROS TO PLANO CODIGO ANS
           DISPLAY TELACONV.
        L2.
           ACCEPT CODIGOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-FIM.
           IF CODIGO = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2. 
        LER.
           READ CADCONV
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                 PERFORM L5A
                 DISPLAY TELACONV
                 MOVE "** CONVENIO JA CADASTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ACE-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONVENIO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
             MOVE "CODIGO DE CONVENIO AINDA NAO CADASTRADO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM.
        L3.
           ACCEPT NOMET
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L2.
           IF NOME = SPACES
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L3.
        L5.
           DISPLAY TELAPLANOS
           ACCEPT PLANOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L3.
        L5A.
           IF PLANO = 0 OR PLANO > 12
                 MOVE "*** DIGITE APENAS DE 1 ATE 12 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L5.
           MOVE TBPLANO(PLANO) TO MSGPLANO
           DISPLAY PLANOTT.
           DISPLAY TELACONV.
        L6.
           ACCEPT ANST
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L5.
           IF ANS = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L6.
      *------------- VERFICAR SE E ALTERACAO --------
           IF W-SEL = 1
              GO TO ALT-OPC.

        INC-OPC.
            MOVE "S" TO W-OPCAO
            DISPLAY (23, 40) "DADOS OK (S/N) : ".
            ACCEPT (23, 57) W-OPCAO WITH UPDATE
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01 
               GO TO L6.
            IF W-OPCAO = "N" OR "n"
               MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO L1.
            IF W-OPCAO NOT = "S" AND "s"
               MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-OPC.
        INC-WR1.
            WRITE REGCONV
            IF ST-ERRO = "00" OR "02"
               MOVE "** DADOS GRAVADOS **" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO L1.
            IF ST-ERRO = "22"
               MOVE "** CONVENIO JA CADASTRADO, DADOS NAO GRAVADOS **" 
                      TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO L1
            ELSE
               MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE CONVENIO * "
                            TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO L1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO L3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCONV RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CONVENIO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO L6.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCONV
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CONVENIO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       ROT-FIM.
           CLOSE CADCONV.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
               GO TO ROT-MENS2
           ELSE
              MOVE SPACES TO MENS
              DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-TEMPO.   