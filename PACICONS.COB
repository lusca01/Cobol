       IDENTIFICATION DIVISION.
       PROGRAM-ID. PACICONS.
      *AUTHOR. LUCAS DE LIMA.
      **************************************************
      * CONSULTA DE PACIENTES *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT CADPACI ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CPF
                      ALTERNATE RECORD KEY IS NOMES WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
              SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *
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
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGPACI.
          03 CPF       PIC X(11).
          03 NOMES     PIC X(30).
          03 DATANASC. 
              05 DIA   PIC 9(02).
              05 MES   PIC 9(02).
              05 ANO   PIC 9(04).
          03 SEXO      PIC X(01).
          03 GENERO    PIC X(01).
          03 CODIGOR   PIC 9(04).
          03 CEPZ      PIC 9(08).
          03 NUMERO    PIC X(05).
          03 COMPLEMENTO PIC X(10).
          03 TELEFONE.
               05 DDD  PIC 9(02).
               05 TNUM PIC 9(09).
          03 EMAIL     PIC X(30).
      *-----------------------------------------------------------------
       FD CADCONV
              LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01  REGCONV.
           03 CODIGO              PIC 9(04).
           03 NOME                PIC X(30).           
           03 PLANO               PIC 9(02).
           03 ANS                 PIC 9(06).
      *-------------------------------------------------------
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
       01 MSGSEXO       PIC X(10) VALUE SPACES.
       01 MSGESTADO        PIC X(18) VALUE SPACES.
       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "AASSEXUAL".
          03 FILLER     PIC X(15) VALUE "EHETEROSSEXUAL".
          03 FILLER     PIC X(15) VALUE "HHOMOSSEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSEXUAL".
          03 FILLER     PIC X(15) VALUE "TTRANSEXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO  PIC X(15) OCCURS 7 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 
      * 
       01 MSGPLANO       PIC X(30) VALUE SPACES.
       01 TABPLANO.
          03 TBPLANO     PIC X(30) OCCURS 12 TIMES.
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

       01 TABPACI.
          03 TBPACI    PIC 9(11) OCCURS 1000 TIMES.
       01 REGN      PIC 9(11) VALUE ZEROS.  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPACIENTE.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                      CONSULTA DE PACIEN".
           05  LINE 02  COLUMN 41 
               VALUE  "TES".
           05  LINE 04  COLUMN 01 
               VALUE  "   CPF:             NOME:".
           05  LINE 04  COLUMN 41 
               VALUE  "                  DATANASC:".
           05  LINE 06  COLUMN 01 
               VALUE  "   SEXO:              GENERO:".
           05  LINE 06  COLUMN 41 
               VALUE  "         TELEFONE:".
           05  LINE 08  COLUMN 01 
               VALUE  "   CONVENIO:      PLANO: ".
           05  LINE 08  COLUMN 61
               VALUE "ANS:".
           05  LINE 10  COLUMN 01 
               VALUE  "   CEP:          ENDERECO:".
           05  LINE 10  COLUMN 41 
               VALUE  "                  NUMERO:".
           05  LINE 12  COLUMN 01 
               VALUE  "   COMPLEMENTO:            BAIRRO:".
           05  LINE 14  COLUMN 01 
               VALUE  "   UF: ".
           05  LINE 12  COLUMN 41 
               VALUE  "               CIDADE:".
           05  LINE 16  COLUMN 01 
               VALUE  "   EMAIL:".
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM:".
           05  CPFT
               LINE 04  COLUMN 08  PIC 9(11)
               USING  CPF
               HIGHLIGHT.
           05  NOMET
               LINE 04  COLUMN 27  PIC X(30)
               USING  NOMES
               HIGHLIGHT. 
           05  DIAT
               LINE 04  COLUMN 68  PIC 9(02)
               USING  DIA
               HIGHLIGHT.
           05  MEST
               LINE 04  COLUMN 71  PIC 9(02)
               USING  MES
               HIGHLIGHT.
           05  ANOT
               LINE 04  COLUMN 74  PIC 9(04)
               USING  ANO
               HIGHLIGHT.
           05  SEXOT
               LINE 06  COLUMN 09  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  SEXOTT
               LINE 06  COLUMN 11  PIC X(10)
               USING  MSGSEXO
               HIGHLIGHT.
           05  GENEROT
               LINE 06  COLUMN 31  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 06  COLUMN 33  PIC X(15)
               USING  TXTGENERO
               HIGHLIGHT.
           05  DDDT
               LINE 06  COLUMN 59  PIC 9(02)
               USING  DDD
               HIGHLIGHT.
           05  TELEFONET
               LINE 06  COLUMN 62  PIC 9(09)
               USING  TNUM
               HIGHLIGHT.
           05  CODIGOT
               LINE 08  COLUMN 13  PIC 9(04)
               USING  CODIGOR
               HIGHLIGHT.
           05  PLANOT
               LINE 08  COLUMN 26  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  PLANOTT
               LINE 08  COLUMN 29  PIC X(30)
               USING  MSGPLANO
               HIGHLIGHT.
           05  ANST
               LINE 08  COLUMN 66  PIC 9(06)
               USING  ANS
               HIGHLIGHT.
           05  CEPT
               LINE 10  COLUMN 09  PIC 9(08)
               USING  CEPZ
               HIGHLIGHT.
           05  ENDERECOT
               LINE 10  COLUMN 27  PIC X(30)
               USING  ENDERECO
               HIGHLIGHT.
           05  NUMEROT
               LINE 10  COLUMN 66  PIC X(05)
               USING  NUMERO
               HIGHLIGHT.
           05  COMPLEMENTOT
               LINE 12  COLUMN 16  PIC X(10)
               USING  COMPLEMENTO
               HIGHLIGHT.
           05  BAIRROT
               LINE 12  COLUMN 35  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  CIDADET
               LINE 12  COLUMN 63  PIC X(15)
               USING  CIDADE
               HIGHLIGHT.
           05  UFT
               LINE 14  COLUMN 07  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  ESTADOTT
               LINE 14  COLUMN 10  PIC X(18)
               USING  MSGESTADO
               HIGHLIGHT.
           05  EMAILT
               LINE 16  COLUMN 10  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
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
       ABRIR-CADPACI.
           OPEN INPUT CADPACI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADPACI NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADPACI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CPF REGN.
           START CADPACI KEY IS NOT LESS CPF INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
        ABRIR-CADCONV.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADCONV NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       ABRIR-CADCEP.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
      *----------------------------------------------------
       LER-PACI.           
           READ CADPACI NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO REGN
                 ADD -1 TO REGN
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADPACI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CPF TO TBPACI(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 GO TO LER-PACI.
       L0.
           MOVE ZEROS TO CPF DATANASC DIA MES ANO TELEFONE CODIGOR 
           MOVE SPACES TO NOMES SEXO MSGSEXO GENERO ENDERECO TXTGENERO
           MOVE SPACES TO MSGPLANO MSGESTADO
           MOVE ZEROS TO ANS CEPZ PLANO TNUM DDD
           MOVE SPACES TO COMPLEMENTO BAIRRO CIDADE NUMERO EMAIL UF
           DISPLAY TELAPACIENTE.
       L1.
           ACCEPT CPFT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO ROT-FIM.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1.
           MOVE 1 TO IND.
       L2.
           IF TBPACI(IND) < CPF
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** CPF NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L1
              ELSE
                 GO TO L2.
       L3.
           MOVE TBPACI(IND) TO CPF.
       L4.
           READ CADPACI 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADPACI"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               NEXT SENTENCE.
        L4A.
           MOVE TBGENERO(INDX) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO INDX
              IF INDX < 8
                 GO TO L4A
           ELSE
              MOVE TXTGENERO2 TO TXTGENERO.
              IF SEXO = "M" OR SEXO = "m"
                 MOVE "MASCULINO" TO MSGSEXO
              ELSE
                 MOVE "FEMININO" TO MSGSEXO.
              MOVE CEPZ TO CEP
              READ CADCEP
              PERFORM UFESTADO THRU UFESTADOFIM.

              MOVE CODIGOR TO CODIGO
              READ CADCONV
              MOVE TBPLANO(PLANO) TO MSGPLANO.
      *
       ROT-MONTAR.
              DISPLAY TELAPACIENTE.
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
           CLOSE CADPACI CADCEP CADCONV.
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

       UFESTADO.
           MOVE TBESTADOS(IND) TO TXTESTADO
           IF TXTESTADO1 = UF
              MOVE TXTESTADO2 TO MSGESTADO 
           ELSE
              ADD 1 TO IND
              IF IND < 10
                 GO TO UFESTADO.
       UFESTADOFIM.