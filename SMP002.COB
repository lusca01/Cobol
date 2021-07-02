       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP002.
      *AUTHOR. LUCAS DE LIMA.
      *****************************************************************
      *    CADASTRO DE DOENÇA         *
      *****************************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                       WITH DUPLICATES.     
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT".
       01  REGCID.
           03 CODIGO              PIC 9(04).
           03 DENOMINACAO         PIC X(30).
           03 CLASSIFICACAO       PIC 9(01).
           03 CONTAGIOSA          PIC X(01).
           03 RARIDADE            PIC X(01).
      *
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
       01 MSGCONTAGIOSA     PIC X(03) VALUE SPACES.
       01 MSGCLASSIFICACAO       PIC X(15) VALUE SPACES.
       01 MSGRARIDADE   PIC X(03) VALUE SPACES.
      *
       01 TABCLASSIFICACAO.
          03 TBCLASSIFICACAO     PIC X(15) OCCURS 4 TIMES.
      *------------------------------------
       SCREEN SECTION.
       01  TELACID.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                         CADASTRO DA DOE".
           05  LINE 03  COLUMN 41 
               VALUE  "NCA".
           05  LINE 05  COLUMN 01 
               VALUE  "     CODIGO       :".
           05  LINE 07  COLUMN 01 
               VALUE  "     DENOMINACAO  :".
           05  LINE 09  COLUMN 01 
               VALUE  "     CLASSIFICACAO:".
           05  LINE 11  COLUMN 01 
               VALUE  "     CONTAGIOSA   :".
           05  LINE 13  COLUMN 01 
               VALUE  "     RARA         :".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM:".
           05  TCODIGO
               LINE 05  COLUMN 21  PIC 9(04)
               USING  CODIGO
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 07  COLUMN 21  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TCLASSIFICACAO
               LINE 09  COLUMN 21  PIC 9(01)
               USING  CLASSIFICACAO
               HIGHLIGHT.
           05  TTCLASSIFICACAO
               LINE 09  COLUMN 25  PIC X(15)
               USING  MSGCLASSIFICACAO
               HIGHLIGHT.
           05  TCONTAGIOSA
               LINE 11  COLUMN 21  PIC X(01)
               USING  CONTAGIOSA
               HIGHLIGHT.
           05  TTCONTAGIOSA
               LINE 11  COLUMN 23  PIC X(03)
               USING  MSGCONTAGIOSA
               HIGHLIGHT.
           05  TRARIDADE
               LINE 13  COLUMN 21  PIC X(01)
               USING  RARIDADE
               HIGHLIGHT.
           05  TTRARIDADE
               LINE 13  COLUMN 23  PIC X(03)
               USING  MSGRARIDADE
               HIGHLIGHT.
      *
       01  TELACLASS.
           05  LINE 10  COLUMN 35 
               VALUE  "         1 - EMERGENCIA".
           05  LINE 12  COLUMN 35 
               VALUE  "         2 - URGENCIA".
           05  LINE 14  COLUMN 35 
               VALUE  "         3 - SEMI-URGENCIA".
           05  LINE 16  COLUMN 35 
               VALUE  "         4 - NAO-URGENCIA".
      *
      *--------------(INICIO)--------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "EMERGENCIA"    TO TBCLASSIFICACAO(01)
           MOVE "URGENCIA"      TO TBCLASSIFICACAO(02)
           MOVE "SEMI-URGENCIA" TO TBCLASSIFICACAO(03)
           MOVE "NAO-URGENCIA"  TO TBCLASSIFICACAO(04).
       L0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
                OPEN OUTPUT CADCID
                CLOSE CADCID
                MOVE " ARQUIVO CADCID FOI CRIADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO L0
             ELSE
                MOVE " ERRO NA ABERTURA DO ARQUIVO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       L1.
           MOVE SPACES TO DENOMINACAO RARIDADE MSGCLASSIFICACAO 
           MOVE SPACES TO CONTAGIOSA MSGCONTAGIOSA MSGRARIDADE
           MOVE ZEROS TO CLASSIFICACAO CODIGO
           DISPLAY TELACID.
       L2.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-FIM.
           IF CODIGO = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2. 
        LER.
           READ CADCID
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                 PERFORM L5A            
                 PERFORM L6A
                 PERFORM L7A
                 DISPLAY TELACID
                 MOVE "** DOENCA JA CADASTRADA **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ACE-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCID" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
             MOVE "DOENCA AINDA NAO CADASTRADA" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM.
        L3.
           ACCEPT TDENOMINACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L2.
           IF DENOMINACAO = SPACES
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L3.
        L5.
           DISPLAY TELACLASS
           ACCEPT TCLASSIFICACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L3.
        L5A.
           IF CLASSIFICACAO = 0 OR CLASSIFICACAO > 4 
                 MOVE "*** DIGITE APENAS DE 1 ATE 4 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L5.
           MOVE TBCLASSIFICACAO(CLASSIFICACAO) TO MSGCLASSIFICACAO
           DISPLAY TTCLASSIFICACAO.
           DISPLAY TELACID.
        L6.
           PERFORM SIMNAO
           ACCEPT TCONTAGIOSA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L5.
        L6A.
           IF CONTAGIOSA = "S" OR CONTAGIOSA = "s"
              MOVE "SIM" TO MSGCONTAGIOSA
           ELSE
              IF CONTAGIOSA = "N" OR CONTAGIOSA = "n"
                 MOVE "NAO" TO MSGCONTAGIOSA
              ELSE
                 MOVE "*DIGITE S = SIM  N = NAO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L6.
           DISPLAY TTCONTAGIOSA.
        L7.
           PERFORM SIMNAO
           ACCEPT TRARIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L6.
        L7A.
           IF RARIDADE = "S" OR RARIDADE = "s"
              MOVE "SIM" TO MSGRARIDADE
           ELSE
              IF RARIDADE = "N" OR RARIDADE = "n"
                 MOVE "NAO" TO MSGRARIDADE
              ELSE
                 MOVE "*DIGITE S = SIM  N = NAO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L7.
           DISPLAY TTRARIDADE.

      *------------- VERFICAR SE E ALTERACAO --------
           IF W-SEL = 1
              GO TO ALT-OPC.

        INC-OPC.
            MOVE "S" TO W-OPCAO
            DISPLAY (23, 40) "DADOS OK (S/N) : ".
            ACCEPT (23, 57) W-OPCAO WITH UPDATE
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01 
               GO TO L7.
            IF W-OPCAO = "N" OR "n"
               MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO L1.
            IF W-OPCAO NOT = "S" AND "s"
               MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-OPC.
        INC-WR1.
            WRITE REGCID
            IF ST-ERRO = "00" OR "02"
               MOVE "** DADOS GRAVADOS **" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO L1.
            IF ST-ERRO = "22"
               MOVE "** DOENCA JA CADASTRADA, DADOS NAO GRAVADOS **" 
                      TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO L1
            ELSE
               MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE DOENCA * "
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
                DELETE CADCID RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO MEDICO EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO L7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCID
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO MEDICO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       ROT-FIM.
           CLOSE CADCID.
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

       SIMNAO. 
           MOVE " S = SIM OU N = NAO " TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM.


