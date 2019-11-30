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
       RECORD KEY CodC
       FILE STATUS ARQST.
       DATA DIVISION.
       FILE SECTION.
       FD  CC LABEL RECORD STANDARD
           DATA RECORD IS REG-CC
           VALUE OF FILE-ID IS "CC.DAT".

      *-Aqui são os dados que vão ficar salvos no .DAT
       01 REG-CONTA.
           02 CodC pic 9(4).
           02 NomeC pic A(30).
           02 CodBancoC pic 9(4).
           02 CodAgenciaC pic 9(4).
           02 CodContaC pic 9(6).
           02 SaldoC pic 9(7).

       WORKING-STORAGE SECTION.

           01 ARQST pic X(2).
           01 Escolha pic 9(1).
           01 Opc pic x(1) value spaces.
           01 Salva pic x(1) value spaces.
           01 Espaco pic x(60) values spaces.

           01 Dados-Editados.
           02 Cod pic 9999.
           02 Nome pic A(30) values spaces.
           02 CodBanco pic 9999.
           02 CodAgencia pic 9999.
           02 CodConta pic 999999.
           02 Saldo pic z999.999,99.

       01 DATA-SIS.
           02 Ano pic 99.
           02 Mes pic 99.
           02 Dia pic 99.

       SCREEN SECTION.

       01 Menu.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "=                                             =".
           02 COL 30 VALUE
           " PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES "
           FOREGROUND-COLOR 3.
           02 LINE 5 COL 18 VALUE "| Escolha uma opcao:   |"
           FOREGROUND-COLOR 6.
           02 LINE 8 COL 19 VALUE "1. Incluir uma conta."
           FOREGROUND-COLOR 6.
           02 LINE 10 COL 19 VALUE "2. Excluir uma conta."
           FOREGROUND-COLOR 6.
           02 LINE 12 COL 19 VALUE "3. Editar uma conta ."
           FOREGROUND-COLOR 6.
           02 LINE 14 COL 19 VALUE "4. Sair do Programa."
           FOREGROUND-COLOR 6.

       01 Tela-Incluir.

           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "=                                             =".
           02 COL 30 VALUE
           " PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES "
           FOREGROUND-COLOR 3.
           02 LINE 4 COL 19 VALUE "Codigo da conta:".
           02 LINE 6 COL 19 VALUE "Nome do usuario:".
           02 LINE 8 COL 19 VALUE "Banco:".
           02 LINE 10 COL 19 VALUE "Agencia:".
           02 LINE 12 COL 19 VALUE "Conta (sem hifen):".
           02 LINE 14 COL 19 VALUE "Saldo:".
           02 LINE 16 COL 25 VALUE "Mensagem:".

       01 Tela-Excluir.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "=                                             =".
           02 COL 30 VALUE
           " PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES "
           FOREGROUND-COLOR 3.
           02 LINE 4 COL 29 VALUE "Codigo da conta:".

       01 Tela-Editar.
           02 Blank Screen.
           02 LINE 2 COL 5 VALUE "  /  / ".
           02 COL 29 VALUE
           "=                                             =".
           02 COL 30 VALUE
           " PROGRAMA DE MANIPULACAO DE CONTAS CORRENTES "
           FOREGROUND-COLOR 3.
           02 LINE 6 COL 10 VALUE "=Atuais dados da Conta="
           FOREGROUND-COLOR 6.
           02 LINE 8 COL 10 VALUE "Codigo da conta:".
           02 LINE 10 COL 10 VALUE "Nome do usuario:".
           02 LINE 12 COL 10 VALUE "Banco:".
           02 LINE 14 COL 10 VALUE "Agencia:".
           02 LINE 16 COL 10 VALUE "Conta (sem hifen):".
           02 LINE 18 COL 10 VALUE "Saldo:".
           02 LINE 22 COL 20 VALUE "Mensagem:".

           02 LINE 6 COL 65 VALUE "=Coloque novos dados="
           FOREGROUND-COLOR 6.
           02 LINE 8 COL 65 VALUE "Codigo da conta:".
           02 LINE 10 COL 65 VALUE "Nome do usuario:".
           02 LINE 12 COL 65 VALUE "Banco:".
           02 LINE 14 COL 65 VALUE "Agencia:".
           02 LINE 16 COL 65 VALUE "Conta (sem hifen):".
           02 LINE 18 COL 65 VALUE "Saldo:".

       01 Tela-Fim.
              02 Blank Screen.
              02 line 23 column 31 value
             "= Programa Finalizado =" Foreground-color 6.

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
           Accept ESCOLHA at 0539 with prompt.
           if ESCOLHA = '1' PERFORM 06-INCLUIR
             else if ESCOLHA = '2' PERFORM 06-Excluir
               else if ESCOLHA = '3' PERFORM 06-Mostrar
               else if ESCOLHA = '4' PERFORM 07-Editar
                 else PERFORM 20-CONTINUA.

       05-Mostra-Data.
           ACCEPT DATA-SIS FROM DATE.
           DISPLAY DIA AT 0205 FOREGROUND-COLOR 3.
           DISPLAY MES AT 0208 FOREGROUND-COLOR 3.
           DISPLAY ANO AT 0211 FOREGROUND-COLOR 3.

       05-ABERTURA.
      * ----------------------------- Inicialização das variáveis
           MOVE ZEROS TO CodC.
           MOVE SPACE TO Opc Salva.
           MOVE SPACES TO NomeC Nome.
           MOVE ZEROS TO CodC Cod.
           MOVE ZEROS TO CodBancoC CodBanco.
           MOVE ZEROS TO CodAgenciaC CodAgencia.
           MOVE ZEROS TO CodContaC CodConta.
           MOVE ZEROS TO SaldoC Saldo.
           DISPLAY ESPACO AT 1635.

       06-Incluir.
           DISPLAY Tela-Incluir.
           PERFORM 05-Mostra-Data.
           PERFORM 05-ABERTURA.
           PERFORM 07-RECEBE.

       07-RECEBE.
           MOVE ZEROS TO CodBancoC CodBanco
           MOVE ZEROS TO CodAgenciaC CodAgencia
           MOVE ZEROS TO CodContaC CodConta
           MOVE ZEROS TO SaldoC Saldo
           MOVE SPACES TO NomeC Nome
           PERFORM 08-Testa-Cod UNTIL CodC > 0.
           PERFORM 10-VNome UNTIL NomeC NOT = SPACES.
           PERFORM 10-VCodBanco UNTIL CodBancoC NOT = ZEROS.
           PERFORM 10-VCodAgencia UNTIL CodAgenciaC NOT = ZEROS.
           PERFORM 10-VCodConta UNTIL CodContaC NOT = ZEROS.
           PERFORM 10-VSaldo UNTIL SaldoC NOT = ZEROS.
           PERFORM 11-GRAVA UNTIL SALVA = "S" OR = "N".

       08-Testa-Cod.
           ACCEPT Cod AT 0438 WITH PROMPT AUTO.
           MOVE Cod TO CodC.
           IF CodC = ZEROS
           THEN
            DISPLAY "CODIGO IGUAL ZERO" AT 1635
           ELSE
           READ CC NOT INVALID KEY
           DISPLAY "JA CADASTRADO" AT 1635
           MOVE ZEROS TO CodC Cod
           MOVE ZEROS TO CodBancoC CodBanco
           MOVE ZEROS TO CodAgenciaC CodAgencia
           MOVE ZEROS TO CodContaC CodConta
           MOVE ZEROS TO SaldoC Saldo
           MOVE SPACES TO NomeC Nome
           END-READ
           END-IF.

       10-VNome.
           ACCEPT NOME AT 0636 WITH PROMPT AUTO.
           MOVE Nome TO NomeC.
           IF NomeC = SPACES
           DISPLAY "DIGITE O NOME DO USUARIO" AT 1635
           ELSE
           DISPLAY ESPACO AT 1635.

       10-VCodBanco.
           ACCEPT CodBanco AT 0836 WITH PROMPT AUTO.
           MOVE CodBanco TO CodBancoC.
           IF CodBancoC = ZEROS
           DISPLAY "DIGITE CODIGO CARAI" AT 1635
           ELSE
           DISPLAY ESPACO AT 1635.

       10-VCodAgencia.
           ACCEPT CodAgencia AT 1036 WITH PROMPT AUTO.
           MOVE CodAgencia TO CodAgenciaC.
           IF CodAgenciaC = ZEROS
           DISPLAY "DIGITE!" AT 1635
           ELSE
           DISPLAY ESPACO AT 1635.

       10-VCodConta.
           ACCEPT CodConta AT 1236 WITH PROMPT AUTO.
           MOVE CodConta TO CodContaC.
           IF CodContaC = ZEROS
           DISPLAY "JUST MONIKA" AT 1635
           ELSE
           DISPLAY ESPACO AT 1635.

       10-VSaldo.
           ACCEPT Saldo AT 1436 WITH PROMPT AUTO.
           MOVE Saldo TO SaldoC.
           IF SaldoC = ZEROS
           DISPLAY "FELIZ NAVIDAD" AT 1635
           ELSE
           DISPLAY ESPACO AT 1635.

       06-Excluir.

           Display Tela-Excluir.
           Perform 05-Mostra-Data.
             ACCEPT Cod AT 0446.
               MOVE Cod to CodC
               delete CC Record
                   INVALID Key
                       DISPLAY "Tem esse nao" AT 1830
                       Display ESPACO at 1333
                   NOT INVALID KEY
                       DISPLAY "Registro removido com sucesso." AT 1830
                       Display ESPACO at 1333
               END-DELETE.

       06-Mostrar.

           Display Tela-Editar.
           Perform 05-Mostra-Data.
           Accept Cod
           Move Cod to CodC

           READ CC
               Invalid Key Display "deu"
               Not Invalid Key
               Move NomeC to Nome
               Display Nome at 0303
               Move CodBancoC to CodBanco
               Display CodBanco at 0403
               Move CodAgenciaC to CodAgencia
               Display CodAgencia at 0503
               Move CodContaC to CodConta
               Display CodConta at 0603

               Display "Updatado"
           END-READ.
           PERFORM 11-GRAVA.

       07-Editar.
           Accept Cod.
               READ CC
               INVALID KEY
               Display "aaaaaaaaaaaaa"
               PERFORM 11-Grava

               END-READ.

                   Perform 07-RECEBE.

                   REWRITE REG-CONTA
                   INVALID KEY
                       Display "aaa"
                   NOT INVALID KEY
                       DISPLAY "Deu"
                       Perform 11-GRAVA
               END-REWRITE.

       11-GRAVA.
           DISPLAY "SALVAR (S/N)? [ ]" AT 1830.
           ACCEPT SALVA AT 1845 WITH PROMPT AUTO.
           IF SALVA = "S" or "s"
           WRITE REG-CONTA INVALID KEY
           CLOSE CC.


       20-CONTINUA.
           DISPLAY "CONTINUA (S/N)? [ ]" AT 1830.
           ACCEPT OPC AT 1847 WITH PROMPT AUTO.
           IF OPC = "S" OR = "N"
           THEN
           DISPLAY ESPACO AT 1830
           DISPLAY ESPACO AT 2235
           ELSE
           DISPLAY ESPACO AT 2235
           DISPLAY "DIGITE S OU N" AT 2235
           END-IF.

       30-Fim.
           DISPLAY Tela-Fim.

           STOP ''.
           STOP Run.
