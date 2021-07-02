       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP006.
      *AUTHOR. LUCAS DE LIMA.
      ********************************************************
      * CADASTRO DE CONSULTA                                 *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT CADCONSU ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS KEYPRINCIPAL
                    FILE STATUS  IS ST-ERRO.
      *
              SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMEMEDICO WITH DUPLICATES.
      *
              SELECT CADPACI ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CPF
                      ALTERNATE RECORD KEY IS NOMEPACIENTE 
                                           WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.    
      *
              SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGOCONV
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOMECONV WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES.
      *
              SELECT CADCID ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGOCID
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS DENOMINACAO
                    WITH DUPLICATES. 
      *---------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCONSU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONSU.DAT". 
       01 REGCONS.
          03 KEYPRINCIPAL. 
              05 CPFPACIENTE    PIC 9(11).
              05 DATACONS.
                 07 DIAC        PIC 9(02).
                 07 MESC        PIC 9(02).
                 07 ANOC        PIC 9(04).
          03 CODMED             PIC 9(06).
          03 CODCONV            PIC 9(04).
          03 CODCID             PIC 9(04).
          03 DESCRICAO1         PIC X(60).
          03 DESCRICAO2         PIC X(60).
      *-----------------------------PACIENTE---------------------------
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGPACI.
          03 CPF                  PIC 9(11).
          03 NOMEPACIENTE         PIC X(30).
          03 DATANASC. 
              05 DIA              PIC 9(02).
              05 MES              PIC 9(02).
              05 ANO              PIC 9(04).
          03 SEXO                 PIC X(01).
          03 GENERO               PIC X(01).
          03 CODIGOPACIENTE       PIC 9(04).
          03 CEPPACIENTE          PIC 9(08).
          03 NUMEROPACIENTE       PIC X(05).
          03 COMPLEMENTOPACIENTE  PIC X(10).
          03 TELEFONEPACIENTE.
               05 DDDPACIENTE     PIC 9(02).
               05 TNUMPACIENTE    PIC 9(09).
          03 EMAILPACIENTE        PIC X(30).
      *------------------------------CONVENIO--------------------------
       FD CADCONV
              LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT".
       01  REGCONV.
           03 CODIGOCONV          PIC 9(04).
           03 NOMECONV            PIC X(30).           
           03 PLANO               PIC 9(02).
           03 ANS                 PIC 9(06).
      *----------------------MEDICO---------------------------------
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01  REGMED.
           03 CRM                       PIC 9(06).
           03 NOMEMEDICO                PIC X(30).
           03 ESPECIALIDADE             PIC 9(02).
           03 SEXOMEDICO                PIC X(01).
           03 EMAIL                     PIC X(30).
           03 TELEFONE.
              05 DDD                    PIC 9(02).
              05 NUMERO                 PIC 9(09).
           03 DATANASCMEDICO.
              05 DIAMEDICO              PIC 9(02).
              05 MESMEDICO              PIC 9(02).
              05 ANOMEDICO              PIC 9(04).
      *------------------------CID-------------------------------
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT".
       01  REGCID.
           03 CODIGOCID           PIC 9(04).
           03 DENOMINACAO         PIC X(30).
           03 CLASSIFICACAO       PIC 9(01).
           03 CONTAGIOSA          PIC X(01).
           03 RARIDADE            PIC X(01).
      *--------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 MENS          PIC X(50) VALUE SPACES.
       01 LIMPA         PIC X(50) VALUE SPACES.
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 IND           PIC 9(01) VALUE ZEROS.
       01 MSGGENERO     PIC X(15) VALUE SPACES.
      *---MEDICO-------------------------------------------
       01 MSGESPECIALIDADE        PIC X(20) VALUE SPACES.
       01 MSGSEXOMEDICO           PIC X(10) VALUE SPACES.
       01 TABESPECIALIDADE.
          03 TBESPECIALIDADE      PIC X(20) OCCURS 9 TIMES.
      *--CID------------------------------------------------
       01 MSGCONTAGIOSA           PIC X(03) VALUE SPACES.
       01 MSGCLASSIFICACAO        PIC X(15) VALUE SPACES.
       01 MSGRARIDADE             PIC X(03) VALUE SPACES.
       01 TABCLASSIFICACAO.
          03 TBCLASSIFICACAO      PIC X(15) OCCURS 4 TIMES.
      *--CONV----------------------------------------------
       01 MSGPLANO       PIC X(30) VALUE SPACES.
       01 TABPLANO.
          03 TBPLANO     PIC X(30) OCCURS 12 TIMES.
      *--PACIENTE------------------------------------------
       01 MSGSEXO       PIC X(10) VALUE SPACES.
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
      *-------------------------------------------------------
       SCREEN SECTION.
       01  TELA06.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                       CADASTRO DE CONSU".
           05  LINE 02  COLUMN 41 
               VALUE  "LTAS".
           05  LINE 04  COLUMN 01 
               VALUE  "  PACIENTE :                     DATA DA".
           05  LINE 04  COLUMN 41 
               VALUE  " CONSULTA :  /  /".
           05  LINE 06  COLUMN 01 
               VALUE  "  NOME:".
           05  LINE 06  COLUMN 41 
               VALUE  "SEXO :   -".
           05  LINE 07  COLUMN 01 
               VALUE  "  DATA DE NASCIMENTO :   /  /         GE".
           05  LINE 07  COLUMN 41 
               VALUE  "NERO :   -".
           05  LINE 09  COLUMN 01 
               VALUE  "  CONVENIO :                     ANS :".
           05  LINE 10  COLUMN 01 
               VALUE  "  PLANO :    -".
           05  LINE 12  COLUMN 01 
               VALUE  "  MEDICO".
           05  LINE 13  COLUMN 01 
               VALUE  "  CRM :          NOME DO MEDICO :".
           05  LINE 14  COLUMN 01 
               VALUE  "  SEXO :   -              ESPECIALIDADE".
           05  LINE 14  COLUMN 41 
               VALUE  ":    -".
           05  LINE 16  COLUMN 01 
               VALUE  "  CONSULTA".
           05  LINE 17  COLUMN 01 
               VALUE  "  CID :         DENOMINACAO :".
           05  LINE 18  COLUMN 01 
               VALUE  "  CONTAGIOSA :   -          RARA :   -".
           05  LINE 20  COLUMN 01 
               VALUE  "  DESCRICAO 1 :".
           05  LINE 21  COLUMN 01 
               VALUE  "  DESCRICAO 2 :".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM:".

           05  TCPFPACIENTE
               LINE 04  COLUMN 14  PIC 9(11)
               USING  CPFPACIENTE
               HIGHLIGHT.
           05  DATACONSD
               LINE 04  COLUMN 53  PIC 9(02)
               USING  DIAC
               HIGHLIGHT.
           05  DATACONSM
               LINE 04  COLUMN 56  PIC 9(02)
               USING  MESC
               HIGHLIGHT.
           05  DATACONSA
               LINE 04  COLUMN 59  PIC 9(04)
               USING  ANOC
               HIGHLIGHT.

           05  TNOMEPACIENTE
               LINE 06  COLUMN 09  PIC X(30)
               USING  NOMEPACIENTE
               HIGHLIGHT.
           05  SEXOT
               LINE 06  COLUMN 48  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TMSGSEXO
               LINE 06  COLUMN 52  PIC X(10)
               USING  MSGSEXO
               HIGHLIGHT.
           05  DIAT
               LINE 07  COLUMN 24  PIC 9(02)
               USING  DIA
               HIGHLIGHT.
           05  MEST
               LINE 07  COLUMN 27  PIC 9(02)
               USING  MES
               HIGHLIGHT.
           05  ANOT
               LINE 07  COLUMN 30  PIC 9(04)
               USING  ANO
               HIGHLIGHT.
           05  GENEROT
               LINE 07  COLUMN 48  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TMSGGENERO
               LINE 07  COLUMN 52  PIC X(15)
               USING  MSGGENERO
               HIGHLIGHT.

           05  TCODIGOCONV
               LINE 09  COLUMN 14  PIC 9(04)
               USING  CODCONV
               HIGHLIGHT.
           05  ANST
               LINE 09  COLUMN 40  PIC 9(06)
               USING  ANS
               HIGHLIGHT.
           05  PLANOT
               LINE 10  COLUMN 11  PIC 9(02)
               USING  PLANO
               HIGHLIGHT.
           05  PLANOTT
               LINE 10  COLUMN 16  PIC X(30)
               USING  MSGPLANO
               HIGHLIGHT.

           05  CRMT
               LINE 13  COLUMN 09  PIC 9(06)
               USING  CODMED
               HIGHLIGHT.
           05  TNOMEMEDICO
               LINE 13  COLUMN 35  PIC X(30)
               USING  NOMEMEDICO
               HIGHLIGHT.
           05  TSEXOMEDICO
               LINE 14  COLUMN 10  PIC X(01)
               USING  SEXOMEDICO
               HIGHLIGHT.
           05  TTSEXOMEDICO
               LINE 14  COLUMN 14  PIC X(10)
               USING  MSGSEXOMEDICO
               HIGHLIGHT.
           05  TESPECIALIDADE
               LINE 14  COLUMN 43  PIC 9(02)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  TTESPECIALIDADE
               LINE 14  COLUMN 48  PIC X(20)
               USING  MSGESPECIALIDADE
               HIGHLIGHT.

           05  TCODIGOCID
               LINE 17  COLUMN 09  PIC 9(04)
               USING  CODCID
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 17  COLUMN 31  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TCONTAGIOSA
               LINE 18  COLUMN 16  PIC X(01)
               USING  CONTAGIOSA
               HIGHLIGHT.
           05  TTCONTAGIOSA
               LINE 18  COLUMN 20  PIC X(03)
               USING  MSGCONTAGIOSA
               HIGHLIGHT.
           05  TRARIDADE
               LINE 18  COLUMN 36  PIC X(01)
               USING  RARIDADE
               HIGHLIGHT.
           05  TTRARIDADE
               LINE 18  COLUMN 40  PIC X(03)
               USING  MSGRARIDADE
               HIGHLIGHT.

           05  TDESCRICAO1
               LINE 20  COLUMN 17  PIC X(60)
               USING  DESCRICAO1
               HIGHLIGHT.
           05  TDESCRICAO2
               LINE 21  COLUMN 17  PIC X(60)
               USING  DESCRICAO2
               HIGHLIGHT.
      *-----------------------------------------------------
       PROCEDURE DIVISION.
       ABRIR-CADCONSU.
           OPEN I-O CADCONSU.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONSU
                 CLOSE CADCONSU
                 MOVE "*** ARQUIVO CADCONSULTA FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ABRIR-CADCONSU
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONSULTA" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
      *
       ABRIR-CADPACI.
           OPEN I-O CADPACI.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADPACI NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE PACIENTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       ABRIR-CADMED.
           OPEN INPUT CADMED.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADMED NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE MEDICO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       ABRIR-CADCONV.
           OPEN INPUT CADCONV.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO DE CADCONV NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CONVENIO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       ABRIR-CADCID.
           OPEN INPUT CADCID.
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO DE CADCID NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CID" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.  
      *-------------------------CARREGANDO VETORES----------
       ESPECIALIDADES. 
           MOVE "CLINICA MEDICA" TO TBESPECIALIDADE(01)
           MOVE "UROLOGISTA"     TO TBESPECIALIDADE(02)
           MOVE "GINECOLOGISTA"  TO TBESPECIALIDADE(03)
           MOVE "PEDIATRA"       TO TBESPECIALIDADE(04)
           MOVE "CARDIOLOGISTA"  TO TBESPECIALIDADE(05)
           MOVE "DERMATOLOGISTA" TO TBESPECIALIDADE(06)
           MOVE "INFECTOLOGISTA" TO TBESPECIALIDADE(07)
           MOVE "OFTAMOLOGISTA"  TO TBESPECIALIDADE(08)
           MOVE "ORTOPEDISTA"    TO TBESPECIALIDADE(09).
       PLANOS.
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
      *--------------------------------Iniciando variáveis------------
       INICIO.
           MOVE ZEROS TO CPFPACIENTE ANOC CODMED CODCONV CODCID ANS ANO
           MOVE ZEROS TO DIA MES DATACONS ESPECIALIDADE MESC DIAC PLANO
           MOVE SPACES TO DESCRICAO1 DESCRICAO2 NOMEPACIENTE SEXO
           MOVE SPACES TO MSGSEXO GENERO MSGGENERO MSGPLANO NOMEMEDICO
           MOVE SPACES TO SEXOMEDICO MSGSEXOMEDICO MSGESPECIALIDADE
           MOVE SPACES TO DENOMINACAO CONTAGIOSA MSGCONTAGIOSA RARIDADE
           MOVE SPACES TO MSGRARIDADE.
           DISPLAY TELA06.
      *-------------------------ACEITANDO CPF E DATA CONSULTA--------
       L1.
           ACCEPT TCPFPACIENTE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CPFPACIENTE = ZEROS
              MOVE " ** PREENCHA O CAMPO PARA PROSSEGUIR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L1.
       L2.
           ACCEPT DATACONSD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L1.
           IF DIAC = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2.
           IF DIAC > 31
              MOVE "O DIA INSERIDO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2.
        L2A.
           ACCEPT DATACONSM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L2.
           IF MESC = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2A.
           IF MESC > 12 
              MOVE "O MES INSERIDO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2A.                        
        L2B.
           ACCEPT DATACONSA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L2A.
           IF ANOC = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2B.
           IF ANOC < 2020 
              MOVE "O ANO INSERIDO NAO E COMPATIVEL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2B.
      *-------------------------VERIFICANDO SE TEM UMA CONSULTA PRONTA-
       LER-CADCONSULTA.
           READ CADCONSU
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                 PERFORM LER-PACIENTE
                 PERFORM LER-CONVENIO
                 PERFORM LER-MEDICO
                 PERFORM LER-CID
                 DISPLAY TELA06
                 MOVE "** CONSULTADA JA CADASTRADA **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ACE-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONSU" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                MOVE "*** CONSULTA NAO CADASTRADA ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.         
      *-----------------------------VERIFICAR PACIENTE------------------
       LER-PACIENTE.
           MOVE CPFPACIENTE TO CPF
           READ CADPACI
           DISPLAY (20, 50) ST-ERRO 
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "**PACIENTE DIGITADO NAO ENCONTRADO**" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADPACI" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                PERFORM MENSAGEM-PACIENTE THRU FIM-MENSAGEM-PACIENTE
                DISPLAY TELA06.
      *---------------------------------------VERIFICAR CONVENIO-----------
       L3.
           ACCEPT TCODIGOCONV
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L2.
           IF CODCONV = ZEROS
              MOVE " ** PREENCHA O CAMPO PARA PROSSEGUIR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L3.
      *---------------------------LER CONVENIO-------------
       LER-CONVENIO.
           MOVE CODCONV TO CODIGOCONV
           READ CADCONV
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "**CONVENIO DIGITADO NAO ENCONTRADO**" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L3
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCONV" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                PERFORM MENSAGEM-CONVENIO THRU FIM-MENSAGEM-CONVENIO
                DISPLAY TELA06.
      *-----------------------------------VERIFICAR MEDICO--------------
       L4.
           ACCEPT CRMT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L3.
           IF CODMED = ZEROS
              MOVE " ** PREENCHA O CAMPO PARA PROSSEGUIR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L4.
      *---------------------------LER MEDICO-------------
       LER-MEDICO.
           MOVE CODMED TO CRM
           READ CADMED
           DISPLAY (20, 50) ST-ERRO 
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "**MEDICO DIGITADO NAO ENCONTRADO**" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L4
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADMED" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                PERFORM MENSAGEM-MEDICO THRU FIM-MENSAGEM-MEDICO
                DISPLAY TELA06.
      *--------------------------------VERIFICAR CID-------------------
       L5.
           ACCEPT TCODIGOCID
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L4.
           IF CODCID = ZEROS
              MOVE " ** PREENCHA O CAMPO PARA PROSSEGUIR ** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L5.
      *---------------------------LER CID-------------
       LER-CID.
           MOVE CODCID TO CODIGOCID
           READ CADCID
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "**CID DIGITADA NAO ENCONTRADO**" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L5
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADCID" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                PERFORM MENSAGEM-CID THRU FIM-MENSAGEM-CID
                DISPLAY TELA06.       
      *------------------------------------DESCRICOES----------
       L6.
           ACCEPT TDESCRICAO1
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L5.
       L7.
           ACCEPT TDESCRICAO2
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO L6.
     
      *-------------VERICAR SE E ALTERACAO-----------------
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
              GO TO INICIO.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-OPC.
       INC-WR1.
           WRITE REGCONS
           IF ST-ERRO = "00" OR "02"
              MOVE "*** DADOS GRAVADOS *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INICIO.
           IF ST-ERRO = "22"
              MOVE "CONSULTA JA EXISTE,DADOS NAO GRAVADOS" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INICIO
           ELSE
               MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PACIENTE" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
      *---------------- ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO------------
       ACE-001.
           DISPLAY (23, 12)"N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
           ACCEPT (23, 55) W-OPCAO
           IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
              MOVE SPACES TO MENS
              DISPLAY (23, 12) MENS
           IF W-OPCAO = "N"
              GO TO INICIO  
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
              GO TO INICIO.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO EXC-OPC.
       EXC-DL1.
           DELETE CADCONSU RECORD
           IF ST-ERRO = "00"
              MOVE "*** REGISTRO CONSULTA EXCLUIDO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INICIO.
           MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.
      *
       ALT-OPC.
           DISPLAY (23, 40) "ALTERAR  (S/N) : ".
           ACCEPT (23, 57) W-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO L7.
           IF W-OPCAO = "N" OR "n"
              MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INICIO.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGCONS
           IF ST-ERRO = "00" OR "02"
              MOVE "*** REGISTRO ALTERADO *** " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INICIO.
           MOVE "ERRO NA EXCLUSAO DA CONSULTA "   TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           GO TO ROT-FIM.

      *-------------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCONV CADMED CADCID CADCONSU CADPACI. 
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
      *------------------------------------------------------
      *----------------CARREGAR AS MENSAGENS-----------------
       MENSAGEM-PACIENTE.
       LP.
           IF SEXO = "M" OR SEXO = "m"
             MOVE "MASCULINO" TO MSGSEXO
           ELSE
             MOVE "FEMININO" TO MSGSEXO.
       LQ.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 = GENERO
              MOVE TXTGENERO2 TO MSGGENERO
           ELSE
              ADD 1 TO IND
              IF IND < 8
                 GO TO LQ.
       FIM-MENSAGEM-PACIENTE.
      *------------------------------------------------------
       MENSAGEM-CONVENIO.
           MOVE TBPLANO(PLANO) TO MSGPLANO.
       FIM-MENSAGEM-CONVENIO.
      *-------------------------------------------------------
       MENSAGEM-MEDICO.
       LX.
           IF SEXOMEDICO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO MSGSEXOMEDICO
           ELSE
              MOVE "FEMININO" TO MSGSEXOMEDICO.
       LZ.
           MOVE TBESPECIALIDADE(ESPECIALIDADE) TO MSGESPECIALIDADE.
       FIM-MENSAGEM-MEDICO.  
      *------------------------------------------------------
       MENSAGEM-CID.
        LT.
           IF CONTAGIOSA = "S" OR CONTAGIOSA = "s"
              MOVE "SIM" TO MSGCONTAGIOSA
           ELSE
              MOVE "NAO" TO MSGCONTAGIOSA.
        LR.
           IF RARIDADE = "S" OR RARIDADE = "s"
              MOVE "SIM" TO MSGRARIDADE
           ELSE
              MOVE "NAO" TO MSGRARIDADE.
       FIM-MENSAGEM-CID.
