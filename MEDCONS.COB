       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDCONS.
      *AUTHOR. LUCAS DE LIMA.
      **************************************************
      * CONSULTA DE MÉDICOS *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CRM
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                    WITH DUPLICATES. 
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT".
       01  REGMED.
           03 CRM                 PIC 9(06).
           03 NOME                PIC X(30).
           03 ESPECIALIDADE       PIC 9(02).
           03 SEXO                PIC X(01).
           03 DATANASC.
              05 DIA              PIC 9(02).
              05 MES              PIC 9(02).
              05 ANO              PIC 9(04).
           03 EMAIL               PIC X(30).
           03 TELEFONE.
              05 DDD              PIC 9(02).
              05 NUMERO           PIC 9(09).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-OPCAO     PIC X(01) VALUE SPACES.
       01 LIMPA       PIC X(55) VALUE SPACES.
       01 W-CONT      PIC 9(04) VALUE ZEROS.
       01 ST-ERRO     PIC X(02) VALUE "00".
       01 W-ACT       PIC 9(02) VALUE ZEROS.
       01 MENS        PIC X(50) VALUE SPACES.
       01 IND         PIC 9(05) VALUE ZEROS.
       01 MSGESPECIALIDADE PIC X(20) VALUE SPACES.
       01 MSGSEXO     PIC X(10) VALUE SPACES.
       01 TABESPECIALIDADE.
          03 TBESPECIALIDADE     PIC X(20) OCCURS 9 TIMES.

       01 TABMED.
          03 TBMED    PIC 9(06) OCCURS 1000 TIMES.
       01 REGN      PIC 9(06) VALUE ZEROS.  
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAMEDICO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                        CONSULTA DO MEDI".
           05  LINE 02  COLUMN 41 
               VALUE  "CO".
           05  LINE 04  COLUMN 01 
               VALUE  "   CRM            :".
           05  LINE 06  COLUMN 01 
               VALUE  "   NOME           :".
           05  LINE 08  COLUMN 01 
               VALUE  "   ESPECIALIDADE  :".
           05  LINE 10  COLUMN 01 
               VALUE  "   SEXO           :".
           05  LINE 12  COLUMN 01 
               VALUE  "   DATA NASCIMENTO:   |  |".
           05  LINE 14  COLUMN 01 
               VALUE  "   EMAIL          :".
           05  LINE 16  COLUMN 01 
               VALUE  "   TELEFONE       :    -".
           05  LINE 21  COLUMN 01 
               VALUE  "              OPCAO :   (E=ENCERRA     P".
           05  LINE 21  COLUMN 41 
               VALUE  "=PROXIMO     A=ANTERIOR )".
           05  LINE 22  COLUMN 41 
               VALUE  "   ==>            <==".
           05  LINE 23  COLUMN 01 
               VALUE  "  MENSAGEM:".
           05  CRMT
               LINE 04  COLUMN 21  PIC 9(06)
               USING  CRM
               HIGHLIGHT.
           05  NOMET
               LINE 06  COLUMN 21  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  ESPECIALIDADET
               LINE 08  COLUMN 21  PIC 9(01)
               USING  ESPECIALIDADE
               HIGHLIGHT.
           05  ESPECIALIDADETT
               LINE 08  COLUMN 23  PIC X(20)
               USING  MSGESPECIALIDADE
               HIGHLIGHT.
           05  SEXOT
               LINE 10  COLUMN 21  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  SEXOTT
               LINE 10  COLUMN 23  PIC X(10)
               USING  MSGSEXO
               HIGHLIGHT.
           05  DIAT
               LINE 12  COLUMN 21  PIC 9(02)
               USING  DIA
               HIGHLIGHT.
           05  MEST
               LINE 12  COLUMN 24  PIC 9(02)
               USING  MES
               HIGHLIGHT.
           05  ANOT
               LINE 12  COLUMN 27  PIC 9(04)
               USING  ANO
               HIGHLIGHT.
           05  EMAILT
               LINE 14  COLUMN 21  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  DDDT
               LINE 16  COLUMN 21  PIC 9(02)
               USING  DDD
               HIGHLIGHT.
           05  NUMEROT
               LINE 16  COLUMN 26  PIC 9(09)
               USING  NUMERO
               HIGHLIGHT.
           05  TW-OPCAO
               LINE 21  COLUMN 23  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "CLINICA MEDICA" TO TBESPECIALIDADE(01)
           MOVE "UROLOGISTA"    TO TBESPECIALIDADE(02)
           MOVE "GINECOLOGISTA"  TO TBESPECIALIDADE(03)
           MOVE "PEDIATRA"       TO TBESPECIALIDADE(04)
           MOVE "CARDIOLOGISTA"  TO TBESPECIALIDADE(05)
           MOVE "DERMATOLOGISTA" TO TBESPECIALIDADE(06)
           MOVE "INFECTOLOGISTA" TO TBESPECIALIDADE(07)
           MOVE "OFTAMOLOGISTA"  TO TBESPECIALIDADE(08)
           MOVE "ORTOPEDISTA"    TO TBESPECIALIDADE(09).
       ABRIR-CADMED.
           OPEN INPUT CADMED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE CADMED NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.
           MOVE 1 TO IND
           MOVE ZEROS TO CRM REGN.
           START CADMED KEY IS NOT LESS CRM INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
      *
       LER-MED.           
           READ CADMED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE IND TO REGN
                 ADD -1 TO REGN
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE CRM TO TBMED(IND)
              ADD 1 TO IND              
              IF IND > 1000
                 MOVE "*** TABELA ESTOURADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 GO TO LER-MED.
       L0.
           MOVE SPACES TO NOME EMAIL SEXO MSGESPECIALIDADE MSGSEXO
           MOVE ZEROS TO DIA DDD MES ANO NUMERO
           MOVE ZEROS TO CRM ESPECIALIDADE DATANASC TELEFONE 
           DISPLAY TELAMEDICO.
       L1.
           ACCEPT CRMT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO ROT-FIM.
           IF W-ACT > 01
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM   
                      GO TO L1.
           MOVE 1 TO IND.
       L2.
           IF TBMED(IND) < CRM
              ADD 1 TO IND
              IF IND >1000
                 MOVE "*** MEDICO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L1
              ELSE
                 GO TO L2.
       L3.
           MOVE TBMED(IND) TO CRM.
       L4.
           READ CADMED 
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "*** REGISTRO NAO ENCONTRADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L0
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              MOVE TBESPECIALIDADE(ESPECIALIDADE) TO MSGESPECIALIDADE
              IF SEXO = "M" OR SEXO = "m"
                  MOVE "MASCULINO" TO MSGSEXO
              ELSE
                 MOVE "FEMININO" TO MSGSEXO.
      *
       ROT-MONTAR.
              DISPLAY TELAMEDICO.
       OPCAO.
           ACCEPT  TW-OPCAO  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                      GO TO ROT-FIM
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
           CLOSE CADMED.
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