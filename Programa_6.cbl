       PROGRAM-ID. Fatorial.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 Valores.
           03 numero-calculo pic s9(18).
           03 numero-mascara pic -z(17)9.
           03 resultado-calculo pic 9(18).
           03 resultado-mascara pic z(17)9.
           03 contador pic s9(18).

       01 Mensagens.
           03 Mensagem-01 pic x(34) value
           "=Calculo do fatorial de um numero=".
           03 Mensagem-02 pic x(37) value
           "Informe um numero inteiro positivo:".
           03 Mensagem-04 pic x(22) value "O valor do fatorial e:".


       01 Data-Hoje.
           03 ano pic 9(2).
           03 mes pic 9(2).
           03 dia pic 9(2).

       SCREEN SECTION.
       01 Tela-Inicial.
           03 line 02 column 02 using dia.
           03 line 02 column 04 value "/".
           03 line 02 column 05 using mes.
           03 line 02 column 07 value "/".
           03 line 02 column 08 using ano.

           03 line 05 column 10 using Mensagem-01.
           03 line 12 column 03 using Mensagem-02.
           03 line 15 column 03 using Mensagem-04.

       01 Blank-Screen.
           03 blank screen.

       PROCEDURE DIVISION.

       01-Inicio.
           initialize Valores.
           accept Data-Hoje from date.

       02-Entrada.
           display Blank-Screen.
           display Tela-Inicial.
           initialize numero-mascara.
           accept numero-mascara at 1238.
           move numero-mascara to numero-calculo.
           if (numero-calculo <0) then perform 02-Entrada
           end-if.
           if numero-calculo = 0 then move 1 to resultado-calculo
              perform 04-Resultado end-if.
           move 1 to resultado-calculo.
           perform 03-Calculo varying contador from numero-calculo
           by -1 until contador = 1.
           perform 04-Resultado.

       03-Calculo.
           compute resultado-calculo = resultado-calculo*contador.

       04-Resultado.
           move resultado-calculo to resultado-mascara.
           display resultado-mascara at 1525.
           display
           "Fim de programa. Pressione qualquer tecla para finalizar"
           at 2010.
           perform 05-Fim.

       05-Fim.
           stop''.
           stop run.
