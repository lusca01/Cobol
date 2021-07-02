       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP005.
      *AUTHOR. LUCAS DE LIMA.
      *****************************************************************
      *    CADASTRO DE CEP                                            *
      *****************************************************************
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
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01  REGCEP.
           03  CEP                 PIC 9(08).
           03  ENDERECO            PIC X(30).
           03  BAIRRO              PIC X(20).
           03  CIDADE              PIC X(20).
           03  UF                  PIC X(02).  
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
       01 ESTADO        PIC X(20) VALUE SPACES.
      *
       01 TABESTADOSX.
          03 FILLER     PIC X(20) VALUE "ACACRE".
          03 FILLER     PIC X(20) VALUE "AMAMAZONAS".
          03 FILLER     PIC X(20) VALUE "PAPARA".
          03 FILLER     PIC X(20) VALUE "SPSAO PAULO".
          03 FILLER     PIC X(20) VALUE "RJRIO DE JANEIRO".
          03 FILLER     PIC X(20) VALUE "PBPARAIBA".
          03 FILLER     PIC X(20) VALUE "PEPERNAMBUCO".
          03 FILLER     PIC X(20) VALUE "BABAHIA".
          03 FILLER     PIC X(20) VALUE "MGMINAS GERAIS".
       01 TABESTADOS REDEFINES TABESTADOSX.
          03 TBESTADOS  PIC X(20) OCCURS 9 TIMES.
       01 TXTESTADO.
          03 TXTESTADO1 PIC X(02) VALUE SPACES.
          03 TXTESTADO2 PIC X(18) VALUE SPACES.
      *---------------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                CADASTRO DE CEP".
           05  LINE 05  COLUMN 01 
               VALUE  "   CODIGO  :".
           05  LINE 07  COLUMN 01 
               VALUE  "   ENDERECO:".
           05  LINE 09  COLUMN 01 
               VALUE  "   BAIRRO  :".
           05  LINE 11  COLUMN 01 
               VALUE  "   CIDADE  :".
           05  LINE 13  COLUMN 01 
               VALUE  "   ESTADO  :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM:".
           05  CEPT
               LINE 05  COLUMN 14  PIC Z(08)
               USING  CEP
               HIGHLIGHT.
           05  ENDERECOT
               LINE 07  COLUMN 14  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  BAIRROT
               LINE 09  COLUMN 14  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  CIDADET
               LINE 11  COLUMN 14  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  UFT
               LINE 13  COLUMN 14  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  ESTADOT
               LINE 13  COLUMN 17  PIC X(20)
               USING  ESTADO
               HIGHLIGHT.
      *
       01  TELAESTADO.
           05  LINE 09  COLUMN 41 
               VALUE  "                     AC - ACRE".
           05  LINE 10  COLUMN 41 
               VALUE  "                     AM - AMAZONAS".
           05  LINE 11  COLUMN 41 
               VALUE  "                     PA - PARA".
           05  LINE 12  COLUMN 41 
               VALUE  "                     SP - SAO PAULO".
           05  LINE 13  COLUMN 41 
               VALUE  "                     RJ - RIO DE JANEIRO".
           05  LINE 14  COLUMN 41 
               VALUE  "                     PB - PARAIBA".
           05  LINE 15  COLUMN 41 
               VALUE  "                     PE - PERNAMBUCO".
           05  LINE 16  COLUMN 41 
               VALUE  "                     BA - BAHIA".
           05  LINE 17  COLUMN 41 
               VALUE  "                     MG - MINAS GERAIS".
      *------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
         L0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE " ARQUIVO CADCEP FOI CRIADO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE " ERRO NA ABERTURA DO ARQUIVO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
        L1.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF ESTADO
           MOVE ZEROS TO CEP
           DISPLAY TELACEP.
        L2.
           ACCEPT CEPT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-FIM.
           IF CEP = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2. 
        LER.
           READ CADCEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                 PERFORM L7
                 DISPLAY TELACEP
                 MOVE "** CEP JA CADASTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ACE-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
             MOVE "CEP AINDA NAO CADASTRADO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM.
        L3.
           ACCEPT ENDERECOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L2.
           IF ENDERECO = SPACES
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L3. 
        L4.
           ACCEPT BAIRROT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO L3.
           IF BAIRRO = SPACES
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L4.
        L5.
           ACCEPT CIDADET
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO L4.
           IF CIDADE = SPACES
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L5.
        L6.
           DISPLAY TELAESTADO
           MOVE 1 TO IND
           ACCEPT UFT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO L5.
        L7.
           MOVE TBESTADOS(IND) TO TXTESTADO
           IF TXTESTADO1 NOT = UF
              ADD 1 TO IND
              IF IND < 10
                 GO TO L7
              ELSE
                 MOVE "UF INCORRETA" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L6
           ELSE 
              MOVE TXTESTADO2 TO ESTADO.
           DISPLAY TELACEP.


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
            WRITE REGCEP
            IF ST-ERRO = "00" OR "02"
               MOVE "** DADOS GRAVADOS **" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO L1.
            IF ST-ERRO = "22"
               MOVE "** CEP JA CADASTRADO, DADOS NAO GRAVADOS **" 
                      TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO L1
            ELSE
               MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE CEP* "
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
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CEP EXCLUIDO ***" TO MENS
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
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       ROT-FIM.
           CLOSE CADCEP.
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