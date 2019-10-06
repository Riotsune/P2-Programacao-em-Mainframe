      *-----------------------------------------------------------------------*
       IDENTIFICATION DIVISION.
              PROGRAM-ID. Equacao_2_Grau_Desestruturada.
      *-----------------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *=======================================================================*
           CONFIGURATION SECTION.
             SPECIAL-NAMES.
      *=======================================================================*
           INPUT-OUTPUT SECTION.
             FILE-CONTROL.
      *=======================================================================*
      *-----------------------------------------------------------------------*
       DATA DIVISION.
      *=======================================================================*
           FILE SECTION.
      *=======================================================================*
           WORKING-STORAGE SECTION.

           77 SimNao pic x(1).

       01 Coeficientes.
           03 A-Calculo pic s9(3).
           03 A-Mascara pic -zzz.
           03 B-Calculo pic s9(3).
           03 B-Mascara pic -zzz.
           03 C-Calculo pic s9(3).
           03 C-Mascara pic -zzz.

       01 Resultados.
           03 Delta-Calculo pic s9(4)v99.
           03 Delta-Mascara pic -zzz9.99.
           03 Raiz-1-Calculo pic s9(3)v99.
           03 Raiz-2-Calculo pic s9(3)v99.
           03 Raiz-1-Mascara pic -zz9.99.
           03 Raiz-2-Mascara pic -zz9.99.


       01 Mensagens.
           03 Mensagem-01 pic x(80) value
           "Calculo das raizes de uma equacao".
           03 Mensagem-02 pic x(50) value
           "Informe os valores dos coeficientes".
           03 Mensagem-03 pic x(7) value "X^2 +( ".
           03 Mensagem-04 pic x(6) value ")X +(".
           03 Mensagem-04-01 pic x(20) value "O valor de delta e:".
           03 Mensagem-05 pic x(30) value "O valor da raiz 1 e:".
           03 Mensagem-05-1 pic x(30) value "O valor da raiz 2 e: ".
           03 Mensagem-06 pic x(30) value
           "As raizes sao iguais e valem:".
           03 Mensagem-07 pic x(30) value
           "Nao existem raizes reais".
           03 Mensagem-08 pic x(50) value
           "Equacao-2-Grau-Desestruturado".
           03 Mensagem-09 pic x(32) value
           "Insira um numero maior que zero".
      *====================================================================*
           LINKAGE SECTION.
      *====================================================================*
           SCREEN SECTION.
           01 Blank-Screen.
               03 blank screen.

           01 Tela-Inicial.
               03 line 04 column 03 using Mensagem-01.
               03 line 06 column 03 using Mensagem-02.

               03 line 10 column 25 using Mensagem-03.
               03 line 10 column 40 using Mensagem-04.
               03 line 10 column 55 value ")= 0".

               03 line 15 column 10 using Mensagem-04-01.
      *--------------------------------------------------------------------*
       PROCEDURE DIVISION.
        01-Receber_Variaveis.
           initialize Coeficientes.
           initialize Resultados.
           display Blank-Screen.

           display Tela-Inicial.
           accept A-Mascara at 1020.
           accept B-Mascara at 1035.
           accept C-Mascara at 1045.

           move A-Mascara to A-Calculo.
           if A-Calculo = 0
             perform 07-A0.
           move B-Mascara to B-Calculo.
           move C-Mascara to C-Calculo.

       02-Calcular_Delta.
           display Blank-Screen.
           display Tela-Inicial.
           compute Delta-Calculo=
           (B-Calculo **2 - 4*A-Calculo*C-Calculo).
           move Delta-Calculo to Delta-Mascara.
           display Delta-Mascara at 1530.

       03-Validar.
           if Delta-Calculo < 0 PERFORM 04-MenorZero.
           if Delta-Calculo = 0 PERFORM 05-IgualZero.
           if Delta-Calculo > 0 PERFORM 06-MaiorZero.

       04-MenorZero.
               display Mensagem-07 at 1710.
               perform 08-Continue.

       05-IgualZero.
                   display Mensagem-06 at 1710.
                   compute Raiz-1-Calculo =
                   (-1*B-Calculo+ Delta-Calculo**0.5)/(2*A-Calculo).
                   move Raiz-1-Calculo to Raiz-1-Mascara.
                   display Raiz-1-Mascara at 1740.
                   perform 08-Continue.

       06-MaiorZero.
           display Mensagem-05 at 1710.
                   display Mensagem-05-1 at 1810.
                   compute Raiz-1-Calculo =
                   (-1*B-Calculo + Delta-Calculo**0.5)/(2*A-Calculo).
                   compute Raiz-2-Calculo =
                   (-1*B-Calculo - Delta-Calculo**0.5)/(2*A-Calculo).
                   move Raiz-1-Calculo to Raiz-1-Mascara.
                   move Raiz-2-Calculo to Raiz-2-Mascara.
                   display Raiz-1-Mascara at 1740.
                   display Raiz-2-Mascara at 1840.
           perform 08-Continue.

       07-A0.
         display Blank-Screen.
         display Mensagem-09.
         perform 01-Receber_Variaveis.



         08-Continue.
           display "Continua? (S/N)" at 2332.
           accept SimNao with prompt at 1938.
           if SimNao = 'S' or 's'
             display Blank-Screen
             perform 01-Receber_Variaveis
           else if SimNao = 'N' or 'n'
             display Blank-Screen
             display  "Fim Do Programa" at 2331 perform
           09-Finalizar
           else display "Opcao Invalida, Insira novamente" at 2431.
           perform 08-Continue.
         09-Finalizar.
           stop ' '.
           stop run.
