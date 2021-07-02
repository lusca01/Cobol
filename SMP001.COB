       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRCEP.
      *AUTHOR. LUCAS DE LIMA.
      *****************************************************************
      *    CADASTRO DE MEDICO                                         *
      *****************************************************************
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
       01 MSGESPECIALIDADE      PIC X(20) VALUE SPACES.
       01 MSGSEXO       PIC X(10) VALUE SPACES.
      *
       01 TABESPECIALIDADE.
          03 TBESPECIALIDADE     PIC X(20) OCCURS 9 TIMES.
      *------------------------------------
       SCREEN SECTION.
       01  TELAMEDICO.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                        CADASTRO DO MEDI".
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
      *         
       01  TELAESP.
           05  LINE 11  COLUMN 41 
               VALUE  "                 1 - CLINICA MEDICA".
           05  LINE 12  COLUMN 41 
               VALUE  "                 2 - UROLOGISTA".
           05  LINE 13  COLUMN 41 
               VALUE  "                 3 - GINECOLOGISTA".
           05  LINE 14  COLUMN 41 
               VALUE  "                 4 - PEDIATRA".
           05  LINE 15  COLUMN 41 
               VALUE  "                 5 - CARDIOLOGISTA".
           05  LINE 16  COLUMN 41 
               VALUE  "                 6 - DERMATOLOGISTA".
           05  LINE 17  COLUMN 41 
               VALUE  "                 7 - INFECTOLOGISTA".
           05  LINE 18  COLUMN 41 
               VALUE  "                 8 - OFTAMOLOGISTA".
           05  LINE 19  COLUMN 41 
               VALUE  "                 9 - ORTOPEDISTA".    
      *----------------
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
       L0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"
             IF ST-ERRO = "30"
                OPEN OUTPUT CADMED
                CLOSE CADMED
                MOVE " ARQUIVO CADMED FOI CRIADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO L0
             ELSE
                MOVE " ERRO NA ABERTURA DO ARQUIVO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
        L1.
           MOVE SPACES TO NOME EMAIL SEXO MSGESPECIALIDADE MSGSEXO
           MOVE ZEROS TO DIA DDD MES ANO NUMERO
           MOVE ZEROS TO CRM ESPECIALIDADE DATANASC TELEFONE 
           DISPLAY TELAMEDICO.
        L2.
           ACCEPT CRMT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-FIM.
           IF CRM = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L2. 
        LER.
           READ CADMED
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                 PERFORM L5A
                 PERFORM L6A
                 DISPLAY TELAMEDICO
                 MOVE "** MEDICO JA CADASTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ACE-001
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CADMED" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
             MOVE "**MEDICO AINDA NAO CADASTRADO**" TO MENS
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
           DISPLAY TELAESP
           ACCEPT ESPECIALIDADET.
        L5A.
           IF ESPECIALIDADE = 0 
                 MOVE "*** DIGITE APENAS DE 1 ATE 9 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L5.
           MOVE TBESPECIALIDADE(ESPECIALIDADE) TO MSGESPECIALIDADE
           DISPLAY ESPECIALIDADETT.
           DISPLAY TELAMEDICO.
        L6.
           MOVE " M = MASCULINO E F = FEMININO " TO MENS
           PERFORM ROT-MENS THRU ROT-MENS-FIM
           ACCEPT SEXOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L5.
        L6A.
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO MSGSEXO
           ELSE
              IF SEXO = "F"
                 MOVE "FEMININO" TO MSGSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO L6.
           DISPLAY SEXOTT.
        L7.
           ACCEPT DIAT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L6.
           IF DIA = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L7.
           IF DIA > 31
              MOVE "O DIA INSERIDO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L7.
        L8.
           ACCEPT MEST
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L7.
           IF MES = " "
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L8.
           IF MES > 12 OR MES < 1
              MOVE "O MES INSERIDO NAO EXISTE" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L8.                        
        L9.
           ACCEPT ANOT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L8.
           IF MES = " "
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L9.
           IF ANO > 2020 OR ANO < 1900 
              MOVE "O ANO INSERIDO NAO E COMPATIVEL" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L9. 
        L10.
           ACCEPT EMAILT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L9.
           IF EMAIL = " "
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L10.
        L11.
           ACCEPT DDDT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L10.
           IF DDD = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L11.
        L12.  
           ACCEPT NUMEROT
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 
              GO TO L11.
           IF NUMERO = ZEROS
              MOVE "PREENCHA O CAMPO PARA CONTINUAR" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO L12. 
      *------------- VERFICAR SE E ALTERACAO --------
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
               GO TO L1.
            IF W-OPCAO NOT = "S" AND "s"
               MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO INC-OPC.
        INC-WR1.
            WRITE REGMED
            IF ST-ERRO = "00" OR "02"
               MOVE "** DADOS GRAVADOS **" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO L1.
            IF ST-ERRO = "22"
               MOVE "** MEDICO JA CADASTRADO, DADOS NAO GRAVADOS **" 
                      TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO L1
            ELSE
               MOVE "* ERRO NA GRAVACAO DO ARQUIVO DE MEDICO * "
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
                DELETE CADMED RECORD
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
                IF W-ACT = 01 GO TO L12.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGMED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO L1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO MEDICO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       ROT-FIM.
           CLOSE CADMED.
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











