       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProgramaP2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CC ASSIGN TO DISK
       ORGANIZATION INDEXED
       ACCESS MODE DYNAMIC
       RECORD KEY COD-C
       FILE STATUS ARQST.

       DATA DIVISION.
       FILE SECTION.
       FD  CC LABEL RECORD STANDARD
           DATA RECORD IS REG-CC
           VALUE OF FILE-ID IS "CC.DAT".

      *-Aqui são os dados que vão ficar salvos no .DAT
       01 REG-CONTA.
           02 COD-C PIC 9(4).
           02 NOME-C PIC X(30).
           02 COD-AGENCIA-C PIC 9(4).
           02 COD-CONTA-CORRENTE PIC 9(5).
           02 SALDO-C PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01 DATA-SIS.
           02 ANO PIC 99.
           02 MES PIC 99.
           02 DIA PIC 99.

           01 ARQST PIC X(2).
           01 ESCOLHA PIC 9.
           01 OPC PIC X(1) VALUE SPACES.
           01 SALVA PIC X(1) VALUE SPACES.
           01 ESPACO PIC X(60) VALUES SPACES.
           01 DADOS-EDITADOS.
           02 CODIGO PIC 9.999.
           02 NOME PIC X(30) VALUES SPACES.
           02 SALDO PIC 9999.99.



       SCREEN SECTION.

       01 Menu.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "= PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES =".
           02 LINE 4 COL 19 VALUE "| Escolha uma opcao:  |".
           02 LINE 6 COL 19 VALUE "1. Incluir uma conta.".
           02 LINE 8 COL 19 VALUE "2. Excluir uma conta.".
           02 LINE 10 COL 19 VALUE "3. Editar uma conta.".
           02 LINE 10 COL 19 VALUE "4. Sair do Programa.".

       01 Tela-Incluir.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "= PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES =".
           02 LINE 4 COL 19 VALUE "CODIGO DA CONTA:".
           02 LINE 6 COL 19 VALUE "NOME DO USUARIO:".
           02 LINE 8 COL 19 VALUE "AGENCIA:".
           02 LINE 10 COL 19 VALUE "CONTA (SEM HIFEN):".
           02 LINE 15 COL 25 VALUE "MENSAGEM:".

       01 Tela-Excluir.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "= PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES =".
           02 LINE 4 COL 19 VALUE "Digite o Codigo da Conta: ".
           02 LINE 6 COL 19 VALUE "NOME DO USUARIO:".
           02 LINE 8 COL 19 VALUE "AGENCIA:".
           02 LINE 10 COL 19 VALUE "CONTA:".
           02 LINE 15 COL 25 VALUE "MENSAGEM:".

       01 Tela-Editar.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "= PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES =".
           02 LINE 4 COL 19 VALUE "CODIGO DA CONTA:".
           02 LINE 6 COL 19 VALUE "NOME DO USUARIO:".
           02 LINE 8 COL 19 VALUE "AGENCIA:".
           02 LINE 10 COL 19 VALUE "CONTA (SEM HIFEN):".
           02 LINE 15 COL 25 VALUE "MENSAGEM:".

       01 Tela-Fim.
              02 Blank Screen.
              02 line 23 column 31 value
             "= Programa Finalizado =".

       PROCEDURE DIVISION.

       03-INICIO.
           PERFORM 04-ABRE-ARQ.
           PERFORM 05-MENU UNTIL OPC = "N" or 'n'.
           PERFORM 30-Fim.

       04-ABRE-ARQ.
           OPEN I-O CC.
           IF ARQST NOT = "00"
           CLOSE CC
           OPEN OUTPUT CC.

       05-MENU.
           Display Menu.
           PERFORM 05-Mostra-Data.
           Accept ESCOLHA at 0439 with prompt.
           if ESCOLHA = '1' PERFORM 06-INCLUIR
             else if ESCOLHA = '2' PERFORM 06-Excluir
               else if ESCOLHA = '3' PERFORM 06-Editar
                 else PERFORM 20-CONTINUA.

       05-Mostra-Data.
           ACCEPT DATA-SIS FROM DATE.
           DISPLAY DIA AT 0205.
           DISPLAY MES AT 0208.
           DISPLAY ANO AT 0211.

       05-ABERTURA.
      * ----------------------------- Inicialização das variáveis
           MOVE ZEROS TO COD-C.
           MOVE SPACE TO OPC SALVA.
           MOVE SPACES TO NOME.
           MOVE SPACES TO NOME-C.
           DISPLAY ESPACO AT 1535.

       06-Incluir.
           DISPLAY Tela-Incluir.
           PERFORM 05-Mostra-Data.
           PERFORM 05-ABERTURA.
           PERFORM 07-RECEBE.

       07-RECEBE.
           PERFORM 08-TESTA-COD UNTIL COD-C > 0.
           PERFORM 10-TESTA-NOME UNTIL NOME-C NOT = SPACES.
           PERFORM 11-GRAVA UNTIL SALVA = "S" OR = "N".


       08-TESTA-COD.
           ACCEPT CODIGO AT 0438 WITH PROMPT AUTO.
           MOVE CODIGO TO COD-C.
           IF COD-C = ZEROS
           THEN
            DISPLAY "CODIGO IGUAL ZERO" AT 1535
           ELSE
           READ CC NOT INVALID KEY
           DISPLAY "JA CADASTRADO" AT 1535
      * ---------------------------- é preciso zerar novamente os campos
           MOVE SPACES TO NOME-C
           MOVE ZEROS TO COD-C
           END-READ
           END-IF.

       10-TESTA-NOME.
           ACCEPT NOME AT 0636 WITH PROMPT AUTO.
           MOVE NOME TO NOME-C.
           IF NOME-C = SPACES
           DISPLAY "DIGITE O NOME DO USUARIO" AT 1535
           ELSE
           DISPLAY ESPACO AT 1535.

       06-Excluir.
           Display Tela-Excluir.
           Perform 05-Mostra-Data.
           Perform 05-ABERTURA.

       06-Editar.
           Display Tela-Editar.
           Perform 05-Mostra-Data.
           Perform 05-ABERTURA.

       11-GRAVA.
           DISPLAY "SALVAR (S/N)? [ ]" AT 1430.
           ACCEPT SALVA AT 1445 WITH PROMPT AUTO.
           IF SALVA = "S"
           WRITE REG-CONTA INVALID KEY STOP RUN.



       20-CONTINUA.
           DISPLAY "CONTINUA (S/N)? [ ]" AT 1430.
           ACCEPT OPC AT 1447 WITH PROMPT AUTO.
           IF OPC = "S" OR = "N"
           THEN
           DISPLAY ESPACO AT 1430
           DISPLAY ESPACO AT 1535
           ELSE
           DISPLAY ESPACO AT 1535
           DISPLAY "DIGITE S OU N" AT 1535
           END-IF.

       30-Fim.
           DISPLAY Tela-Fim.
           CLOSE CC.
           STOP ''.
           STOP Run.
