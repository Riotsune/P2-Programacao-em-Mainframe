*>        Erros encontrados no programa:
*>       1 - As variaveis ws-salario e ws-sal-atual foram em uma mesma operacao de calculo; por possuirem formatação diferente, 
*> são confundidas durante a entrada dos dados e na execucao do calculo, quebrando o resultado final.
      
*>      2 - O uso do Go To nao é recomendável. O ideal seria utilizar o comando perform ou uma estrutura de repetição.

*>      3-As váriaveis da "Areas-de-Trabalho" não são inicializadas no comeco, então quando o programa é repetido os últimos 
*> valores digitados são mantidos e mostrados na tela.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Dados.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 Areas-de-Trabalho.
               02 ws-nome pic x(50) value spaces.
               02 ws-idade pic 9(02) value zeros.
                   88 idade value 15 thru 29.
               02 ws-sexo pic x value space.
                   88 fm value "F" "M" "f" "m".
               02 ws-salario pic 9(5)v99 value zeros.
               02 ws-salario-m pic zz,zz9.99 value zeros.
               02 ws-sal-atual pic zz,zz9.99 value zeros.
               02 ws-sal-atual-c pic 9(5)v99 value zeros.
               02 ws-cont pic x value space.

           01 Mensagens-de-Critica.
               02 mensa1 pic x(30) value "Nome invalido, redigite.".
               02 mensa2 pic x(60) value
               "Idade invalida favor inserir um numero entre 15 e 29.".
               02 mensa3 pic x(40) value
               "Sexo invalido, favor inserir F ou M".
               02 mensa4 pic x(30) value "Salario invalido, redigite.".
               02 mensa5 pic x(30) value spaces.
               02 mensa6 pic x(30) value "Fim do programa".
               02 mensa7 pic x(40) value
               "Opcao invalida, favor redigitar".

           01 Data-do-Sistema.
               02 ano pic 9(02) value zeros.
               02 mes pic 9(02) value zeros.
               02 dia pic 9(02) value zeros.

       SCREEN SECTION.
           01 Tela.
               02 Blank Screen.
               02 line 02 column 28 value "=Programa n7=".
               02 line 08 column 21 value "Nome - ".
               02 line 10 column 21 value "Idade - ".
               02 line 12 column 21 value "Sexo - ".
               02 line 14 column 21 value "Salario Atual - ".
               02 line 19 column 21 value "Continua? (S/N) < > ".

       PROCEDURE DIVISION.

       03-Rot-Inicio.
           display Tela.
           accept Data-do-Sistema from date.
           display dia at 0215.
           display "/" at 0217.
           display mes at 0218.
           display "/" at 0220.
           display ano at 0221.
           initialize Areas-de-Trabalho.
           PERFORM 04-Rot-Nome THRU 09-Rot-Continua.


       04-Rot-Nome.
           accept ws-nome at 0828 with prompt.
           display mensa5 at 2331.
           if ws-nome = spaces
               display mensa1 at 2331
               perform 04-Rot-Nome
           end-if.

       05-Rot-Idade.
           accept ws-idade at 1029 with prompt.
           display mensa5 at 2331.
           if idade
           else display mensa2 at 2331
               perform 05-Rot-Idade
           end-if.

       06-Rot-Sexo.
           accept ws-sexo at 1228 with prompt.
           display mensa5 at 2331.
           if fm or FM
           else display mensa3 at 2331
               perform 06-Rot-Sexo
           end-if.

       07-Rot-Salario.
           accept ws-salario-m at 1437 with prompt.
           move ws-salario-m to ws-salario.
           display mensa5 at 2331.
           if ws-salario > 04999.00 or  < 50001.00
           else display mensa4 at 2331
               perform 07-Rot-Salario
           end-if.

       08-Rot-Calculo.
           compute ws-sal-atual-c = ws-salario * 25/100 + ws-salario.
           move ws-sal-atual-c to ws-sal-atual.
           display ws-sal-atual at 1639.

       09-Rot-Continua.
           accept ws-cont with prompt at 1938.
           display mensa5 at 2332.
           if ws-cont = 'S' or 's' perform 03-Rot-Inicio
           else if ws-cont = 'N' or 'n' display mensa6 at 2331 perform
           10-Fim
           else display mensa7 at 2331
           perform 09-Rot-Continua.

       10-Fim.
           stop ' '.
           stop run.
