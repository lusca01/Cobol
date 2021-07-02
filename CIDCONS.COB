       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCONS.
      *AUTHOR. LUCAS DE LIMA.
      **************************************************
      * CONSULTA DE CID´S *
      **************************************************
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
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-OPCAO     PIC X(01) VALUE SPACES.
       01 LIMPA       PIC X(55) VALUE SPACES.
       01 W-CONT      PIC 9(04) VALUE ZEROS.
       01 ST-ERRO     PIC X(02) VALUE "00".
       01 W-ACT       PIC 9(02) VALUE ZEROS.
       01 MENS        PIC X(50) VALUE SPACES.
       01 IND         PIC 9(05) VALUE ZEROS.
       01 MSGCONTAGIOSA     PIC X(03) VALUE SPACES.
       01 MSGCLASSIFICACAO       PIC X(15) VALUE SPACES.
       01 MSGRARIDADE   PIC X(03) VALUE SPACES.
      *
       01 TABCLASSIFICACAO.
          03 TBCLASSIFICACAO     PIC X(15) OCCURS 4 TIMES.

       01 TABCID.
          03 TBCID   PIC 9(04) OCCURS 1000 TIMES.
       01 REGN      PIC 9(04) VALUE ZEROS.  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACID.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                          CONSULTA DE CID'S".
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
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
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
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "EMERGENCIA"    TO TBCLASSIFICACAO(01)
           MOVE "URGENCIA"      TO TBCLASSIFICACAO(02)
           MOVE "SEMI-URGENCIA" TO TBCLASSIFICACAO(03)
           MOVE "NAO-URGENCIA"  TO TBCLASSIFICACAO(04).
       ABRIR-CADCID.
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCID NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCID"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CODIGO REGN.
           START CADCID KEY IS NOT LESS CODIGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
      *
       LER-MED.           
           READ CADCID NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO REGN
                 ADD -1 TO REGN
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCID"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CODIGO TO TBCID(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 GO TO LER-MED.
       L0.
           MOVE SPACES TO DENOMINACAO RARIDADE MSGCLASSIFICACAO 
           MOVE SPACES TO CONTAGIOSA MSGCONTAGIOSA MSGRARIDADE
           MOVE ZEROS TO CLASSIFICACAO CODIGO
           DISPLAY TELACID.
       L1.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO ROT-FIM.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1.
           MOVE 1 TO IND.
       L2.
           IF TBCID(IND) < CODIGO
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** CEP NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L1
              ELSE
                 GO TO L2.
       L3.
           MOVE TBCID(IND) TO CODIGO.
       L4.
           READ CADCID 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCID"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE TBCLASSIFICACAO(CLASSIFICACAO) TO MSGCLASSIFICACAO
           IF CONTAGIOSA = "S" OR CONTAGIOSA = "s"
              MOVE "SIM" TO MSGCONTAGIOSA
           ELSE
              MOVE "NAO" TO MSGCONTAGIOSA.
           IF RARIDADE = "S" OR RARIDADE = "s"
              MOVE "SIM" TO MSGRARIDADE
           ELSE
              MOVE "NAO" TO MSGRARIDADE.
      *
       ROT-MONTAR.
              DISPLAY TELACID.
       OPCAO.
           ACCEPT  TW-OPCAO  
           IF W-OPCAO = "E"
                  GO TO ROT-FIM 
           ELSE
             IF W-OPCAO = "P"
                 IF IND < REGN
                   ADD 1 TO IND
                   GO TO L3
                 ELSE
                   MOVE "*** ULTIMO REGISTRO ***" TO MENS
                           PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO OPCAO
             ELSE
                IF W-OPCAO = "A"
                    IF IND > 1
                       ADD -1 TO IND
                       GO TO L3
                    ELSE
                       MOVE "*** PRIMEIRO REGISTRO ***" TO MENS
                           PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO OPCAO
                ELSE
                     MOVE "*** OPCAO NAO DISPONIVEL ***" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO OPCAO.
      *
       ROT-FIM.
           CLOSE CADCID.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.