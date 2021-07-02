       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEPCONS.
      *AUTHOR. LUCAS DE LIMA.
      **************************************************
      * CONSULTA DE CEP´S *
      **************************************************
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
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-OPCAO     PIC X(01) VALUE SPACES.
       01 LIMPA       PIC X(55) VALUE SPACES.
       01 W-CONT      PIC 9(04) VALUE ZEROS.
       01 ST-ERRO     PIC X(02) VALUE "00".
       01 W-ACT       PIC 9(02) VALUE ZEROS.
       01 MENS        PIC X(50) VALUE SPACES.
       01 IND         PIC 9(05) VALUE ZEROS.
       01 INDX        PIC 9(01) VALUE ZEROS.
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

       01 TABCEP.
          03 TBCEP   PIC 9(08) OCCURS 1000 TIMES.
       01 REGN      PIC 9(08) VALUE ZEROS.  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01 
               VALUE  "                CONSULTA DE CEP".
           05  LINE 05  COLUMN 01 
               VALUE  "   CEP  :".
           05  LINE 07  COLUMN 01 
               VALUE  "   ENDERECO:".
           05  LINE 09  COLUMN 01 
               VALUE  "   BAIRRO  :".
           05  LINE 11  COLUMN 01 
               VALUE  "   CIDADE  :".
           05  LINE 13  COLUMN 01 
               VALUE  "   ESTADO  :".
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
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
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
       ABRIR-CADCEP.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CEP REGN.
           START CADCEP KEY IS NOT LESS CEP INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
      *
       LER-CEP.           
           READ CADCEP NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO REGN
                 ADD -1 TO REGN
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CEP TO TBCEP(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 GO TO LER-CEP.
       L0.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE UF ESTADO
           MOVE ZEROS TO CEP
           DISPLAY TELACEP.
       L1.
           ACCEPT CEPT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO ROT-FIM.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1.
           MOVE 1 TO IND.
       L2.
           IF TBCEP(IND) < CEP
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** CEP NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L1
              ELSE
                 GO TO L2.
       L3.
           MOVE TBCEP(IND) TO CEP.
       L4.
           READ CADCEP 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCEP"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              PERFORM ESTADOS THRU ESTADOS-FIM.
      *
       ROT-MONTAR.
              DISPLAY TELACEP.
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
           CLOSE CADCEP.
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

       ESTADOS.
           MOVE TBESTADOS(INDX) TO TXTESTADO
           IF TXTESTADO1 = UF
              MOVE TXTESTADO2 TO ESTADO 
           ELSE
              ADD 1 TO INDX
              IF INDX < 10
                 GO TO ESTADOS.
       ESTADOS-FIM.