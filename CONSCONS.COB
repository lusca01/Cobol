       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSUCONS.
      *AUTHOR. LUCAS DE LIMA.
      **************************************************
      * CONSULTA DE CONSULTA *
      **************************************************
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
      *-----------------------------------------------------------------
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
          03 DATACONS. 
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
       01 MSGGENERO   PIC X(15) VALUE SPACES.
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
          03 TBGENERO  PIC X(15) OCCURS 8 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES.

       01 TABCONSU.
          03 TBCONSU    PIC 9(11) OCCURS 1000 TIMES.
          03 TBCONSU1   PIC 9(02) OCCURS 1000 TIMES.
          03 TBCONSU2   PIC 9(02) OCCURS 1000 TIMES.
          03 TBCONSU3   PIC 9(04) OCCURS 1000 TIMES.
       01 REGN      PIC 9(04) VALUE ZEROS.  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA06.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                       CANSULTA DE CONSU".
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
           05  LINE 22  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 22  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 23  COLUMN 41 
               VALUE  "   ==>            <==".
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
           05  TW-OPCAO
               LINE 22  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
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
       ABRIR-CADCONSU.
           OPEN INPUT CADCONSU
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADCONSU NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADCONSU"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CPFPACIENTE REGN.
           START CADCONSU KEY IS NOT LESS CPFPACIENTE INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
        ABRIR-CADMED.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADMED NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE MEDICO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
        ABRIR-CADCONV.
           OPEN INPUT CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO CADCONV NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CONVENIO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       ABRIR-CADCID.
           OPEN INPUT CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CID NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CID " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE. 
       ABRIR-CADPACI.
           OPEN INPUT CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "**ARQUIVO DE PACIENTE NAO ENCONTRADO**" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE PACIENTE " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *----------------------------------------------------
       LER-CONSU.           
           READ CADCONSU NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO REGN
                 ADD -1 TO REGN
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONSU"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CPFPACIENTE TO TBCONSU(IND)
              MOVE DIAC TO TBCONSU1(IND)
              MOVE MESC TO TBCONSU2(IND)
              MOVE ANOC TO TBCONSU3(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 GO TO LER-CONSU.
       L0.
           MOVE ZEROS TO CPFPACIENTE ANOC CODMED CODCONV CODCID ANS ANO
           MOVE ZEROS TO DIA MES ESPECIALIDADE MESC DIAC PLANO
           MOVE SPACES TO DESCRICAO1 DESCRICAO2 NOMEPACIENTE SEXO
           MOVE SPACES TO MSGSEXO GENERO MSGGENERO MSGPLANO NOMEMEDICO
           MOVE SPACES TO SEXOMEDICO MSGSEXOMEDICO MSGESPECIALIDADE
           MOVE SPACES TO DENOMINACAO CONTAGIOSA MSGCONTAGIOSA RARIDADE
           MOVE SPACES TO MSGRARIDADE.
           DISPLAY TELA06.
       L1.
           ACCEPT TCPFPACIENTE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO ROT-FIM.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1.
       L1A.
           ACCEPT DATACONSD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO L1.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1A.
       L1B.
           ACCEPT DATACONSM
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO L1A.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1B.
       L1C.
           ACCEPT DATACONSA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO L1B.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1C.
           MOVE 1 TO IND.
       L2.
           IF TBCONSU(IND) < CPFPACIENTE
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** CPF NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L1
              ELSE
                 GO TO L2.
       L3.
           MOVE TBCONSU(IND) TO CPFPACIENTE
           MOVE TBCONSU1(IND) TO DIAC
           MOVE TBCONSU2(IND) TO MESC
           MOVE TBCONSU3(IND) TO ANOC.
       L4.
           READ CADCONSU 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADCONSU"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
               PERFORM LER-PACIENTE
               PERFORM LER-CONVENIO
               PERFORM LER-MEDICO
               PERFORM LER-CID.
      *
       ROT-MONTAR.
              DISPLAY TELA06.
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
           CLOSE CADCONSU CADMED CADCONV CADCID CADPACI.
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

       LER-PACIENTE.
           MOVE CPFPACIENTE TO CPF
           READ CADPACI
           IF SEXO = "M" OR SEXO = "m"
             MOVE "MASCULINO" TO MSGSEXO
           ELSE
             MOVE "FEMININO" TO MSGSEXO.
       LQ.
           MOVE TBGENERO(INDX) TO TXTGENERO
           IF TXTGENERO1 = GENERO
              MOVE TXTGENERO2 TO MSGGENERO
           ELSE
              ADD 1 TO INDX
              IF INDX < 9
                 GO TO LQ.
       LER-CONVENIO.
           MOVE CODCONV TO CODIGOCONV
           READ CADCONV
           MOVE TBPLANO(PLANO) TO MSGPLANO.
       LER-MEDICO.
           MOVE CODMED TO CRM
           READ CADMED
           IF SEXOMEDICO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO MSGSEXOMEDICO
           ELSE
              MOVE "FEMININO" TO MSGSEXOMEDICO.
           MOVE TBESPECIALIDADE(ESPECIALIDADE) TO MSGESPECIALIDADE.
       LER-CID.
           MOVE CODCID TO CODIGOCID
           READ CADCID
           IF CONTAGIOSA = "S" OR CONTAGIOSA = "s"
              MOVE "SIM" TO MSGCONTAGIOSA
           ELSE
              MOVE "NAO" TO MSGCONTAGIOSA.
           IF RARIDADE = "S" OR RARIDADE = "s"
              MOVE "SIM" TO MSGRARIDADE
           ELSE
              MOVE "NAO" TO MSGRARIDADE.