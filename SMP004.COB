       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. LUCAS DE LIMA.
      ********************************************************
      * CADASTRO DE PACIENTES                                *
      ********************************************************
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
      *-----------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 W-SEL         PIC 9(01) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES.
       77 ST-ERRO       PIC X(02) VALUE "00".
       77 IND           PIC 9(01) VALUE ZEROS.
       01 MSGSEXO       PIC X(10) VALUE SPACES.
       01 MSGESTADO        PIC X(18) VALUE SPACES.
      *
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
      *-------------------------------------------------------
       SCREEN SECTION.
       01  TELAPACIENTE.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                      CADASTRO DE PACIEN".
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
      *------------------------------------------------
       01  TELAGENERO.
           05  LINE 14  COLUMN 41 
               VALUE  "         A - ASSEXUAL".
           05  LINE 15  COLUMN 41 
               VALUE  "         E - HETEROSSEXUAL".
           05  LINE 16  COLUMN 41 
               VALUE  "         H - HOMOSSEXUAL".
           05  LINE 17  COLUMN 41 
               VALUE  "         B - BISSEXUAL".
           05  LINE 18  COLUMN 41 
               VALUE  "         T - TRANSEXUAL".
           05  LINE 19  COLUMN 41 
               VALUE  "         P - PANSEXUAL".
           05  LINE 20  COLUMN 41 
               VALUE  "         N - NAO DECLARADO".
      *-----------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** ARQUIVO CADPACIENTE FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INICIO
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPACIENTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
      *
        R0.
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
      *
       R0A.
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
      *-------------------------(AGORA COMEÇA A BRINCADEIRA)----------
       L01.
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
       L0. 
           MOVE ZEROS TO CPF DATANASC DIA MES ANO TELEFONE CODIGOR
           MOVE SPACES TO MSGPLANO 
           MOVE SPACES TO NOMES SEXO MSGSEXO GENERO ENDERECO TXTGENERO
           MOVE ZEROS TO ANS CEPZ PLANO TNUM DDD
           MOVE SPACES TO COMPLEMENTO BAIRRO CIDADE NUMERO EMAIL UF
           DISPLAY TELAPACIENTE.
       L1.
           ACCEPT CPFT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CPF = ZEROS
              MOVE " ** PREENCHA O CAMPO PARA PROSSEGUIR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L1.
       
       LER-CADPACIENTE.
           READ CADPACI
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                 PERFORM L6A
                 PERFORM L7A
                 PERFORM L9A
                 PERFORM L10PLS
                 DISPLAY TELAPACIENTE
                 MOVE "** PACIENTE JA CADASTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ACE-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADPACIENTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                MOVE "*** PACIENTE NAO CADASTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
      *
       L2.
           ACCEPT NOMET
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L1.
           IF NOME = SPACES 
              MOVE " ** PREENCHA O CAMPO PARA PROSSEGUIR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2.
        L3.
           ACCEPT DIAT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L2.
           IF DIA = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L3.
           IF DIA > 31
              MOVE "O DIA INSERIDO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L3.
        L4.
           ACCEPT MEST
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L3.
           IF MES = " "
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L4.
           IF MES > 12 OR MES < 1
              MOVE "O MES INSERIDO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L4.                        
        L5.
           ACCEPT ANOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L4.
           IF ANO = " "
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L5.
           IF ANO > 2020 OR ANO < 1900 
              MOVE "O ANO INSERIDO NAO E COMPATIVEL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L5.
      *
        L6.
           MOVE " M = MASCULINO E F = FEMININO " TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           ACCEPT SEXOT.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L5.
        L6A.
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO MSGSEXO
           ELSE
              IF SEXO = "F" or SEXO = "f"
                 MOVE "FEMININO" TO MSGSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L6.
           DISPLAY SEXOTT.
      *
       L7.
           DISPLAY TELAGENERO
           MOVE 1 TO IND
           ACCEPT GENEROT.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L6.
       L7A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 8
                 GO TO L7A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L7
           ELSE
               MOVE TXTGENERO2 TO TXTGENERO
               DISPLAY TELAPACIENTE.  
      *
       L8.
           ACCEPT DDDT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L7.
           IF DDD = ZEROS 
              MOVE " ** PREENCHA O CAMPO PARA CONTINUAR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L8.
       L8A.
           ACCEPT TELEFONET
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L8.
           IF TNUM = ZEROS 
              MOVE " ** PREENCHA O CAMPO PARA CONTINUAR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L8A.
      *
       L9.
           ACCEPT CODIGOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPACIENTE
                   GO TO L8.
           IF CODIGOR = ZEROS 
                 MOVE "*** INFORME O CONVENIO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L9.
       L9A.
           MOVE CODIGOR TO CODIGO.
           READ CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "**CONVENIO DIGITADO NAO ENCONTRADO**" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L9
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE TBPLANO(PLANO) TO MSGPLANO
                DISPLAY TELAPACIENTE.
      *
       L10.
           ACCEPT CEPT 
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L9.         
           IF CEPZ = ZEROS
              MOVE "**INFORME O CEP**" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L10.
      *
       L10PLS.
           MOVE CEPZ TO CEP
           READ CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "**CEP DIGITADO NAO ENCONTRADO**" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L10
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
              PERFORM UFESTADO THRU UFESTADOFIM.
              DISPLAY TELAPACIENTE.
      *
       L10A.
           ACCEPT NUMEROT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L10.
           IF NUMERO = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L10A.
      *
       L11.
           ACCEPT COMPLEMENTOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L10A.
      *
       L12.
           ACCEPT EMAILT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L11.
           IF EMAIL = SPACES 
              MOVE " ** PREENCHA O CAMPO PARA FINALIZAR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L12.

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 
                   GO TO L12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L0.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO L0.
                IF ST-ERRO = "22"
                  MOVE "PACIENTE JA EXISTE,DADOS NAO GRAVADOS" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO L0
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PACIENTE"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO 
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
                   GO TO L0  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO L2.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L0.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPACI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PACIENTE EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L0.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01
                   GO TO L12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L0.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPACI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L0.
                MOVE "ERRO NA EXCLUSAO DO PACIENTE "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *-------------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPACI CADCONV CADCEP.
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

       UFESTADO.
           MOVE TBESTADOS(IND) TO TXTESTADO
           IF TXTESTADO1 = UF
              MOVE TXTESTADO2 TO MSGESTADO 
           ELSE
              ADD 1 TO IND
              IF IND < 10
                 GO TO UFESTADO.
       UFESTADOFIM.