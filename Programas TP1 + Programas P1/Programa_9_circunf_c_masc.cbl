       IDENTIFICATION DIVISION.
       CONFIGURATION SECTION.
               Special-names.
               Decimal-point is comma.
       PROGRAM-ID. EX9.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 Mensagens.
           03 Mensagem-01 pic x(50) value "Area de uma circunferencia".
           03 Mensagem-02 pic x(50) value "Insira o raio da circ.:".
           03 Mensagem-03 pic x(50) value "A area da circunferencia e:".
           03 Mensagem-04 pic x(50) value "Pressione qualquer tecla.".

       01 Circunferencia.
           03 raio-mascara pic -z,zz9.99.
           03 raio-calculo pic s9(5)v99.
           03 resultado-mascara pic -z,zz9.99.
           03 resultado-calculo pic 9(5)v99.

       SCREEN SECTION.
       01 Tela-Inicial.
           03 line 05 column 20 pic x(50) using Mensagem-01.
           03 line 10 column 10 pic x(50) using Mensagem-02.
           03 line 15 column 10 pic x(50) using Mensagem-03.

       PROCEDURE DIVISION.
       01-Inicializacao.
           initialize raio-mascara.
           display Tela-Inicial.

       02-Entrada-Dados.
           initialize raio-mascara.
           accept raio-mascara at 1060.
           move raio-mascara to raio-calculo.
           if raio-calculo < 0 then display "Valor invalido" at 2020
           perform 02-Entrada-Dados
           end-if.

       03-Calculo.
           compute resultado-calculo=3.1416*(raio-calculo*raio-calculo).

       04-Resultado.
           move resultado-calculo to resultado-mascara.
           display resultado-mascara at 1560.

       05-Fim.
           display Mensagem-04 at 2020.
           stop " ".
