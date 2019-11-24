       IDENTIFICATION DIVISION.
       PROGRAM-ID. Programa_5_Calculadora.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DADOS.
      * Declaração de variáveis: Nota 1 e Nota 2, soma, subtracao, multiplicacao e divisão
           02 W-N1 PIC 9(03) VALUE ZEROS.
           02 W-N2 PIC 9(03) VALUE ZEROS.
           02 W-Soma PIC 9(03) VALUE ZEROS.
           02 W-Subtracao PIC 9(03) VALUE ZEROS.
           02 W-Multiplicacao PIC 9(08) VALUE ZEROS.
           02 W-Divisao PIC 9(03)V99 VALUE ZEROS.
           02 W-Resto PIC 9(03)V99 VALUE ZEROS.
           01 MascaraInt PIC ZZ9.
           01 MascaraMult PIC Z(8)9.
           01 MascaraFloat PIC ZZ9,9.
      *o zzz. é a separação de um campo
      * EXIBE MENSAGEM NA TELA, intereção com o usuário
       01 MENSAGEMS-DE-TELA.
           02 MENSA1 PIC X(50) VALUE "Digite o primeiro valor: ".
           02 MENSA2 PIC X(50) VALUE "Digite o segundo valor: ".
           02 MENSA3 PIC X(50) VALUE "Soma: ".
           02 MENSA4 PIC X(50) VALUE "Subtracao: ".
           02 MENSA5 PIC X(50) VALUE "Multiplicacao: ".
           02 MENSA6 PIC X(50) VALUE "Divisao: ".
           02 MENSA7 PIC X(50) VALUE '--------FIM DO PROGRAMA------- -'.

       01 DATA-DO-SISTEMA.
           02 ANO PIC 9(02) VALUE ZEROS.
           02 MES PIC 9(02) VALUE ZEROS.
           02 DIA PIC 9(02) VALUE ZEROS.

       SCREEN SECTION.
       01 TELA01.
           02 LINE 02 COLUMN 06 PIC 9(02)/ USING DIA.
           02 LINE 02 COLUMN 09 PIC 9(02)/ USING MES.
           02 LINE 02 COLUMN 12 PIC 9(02) USING ANO.
           02 LINE 02 COLUMN 24 VALUE
           "--- CALCULADORA ---".

       PROCEDURE DIVISION.
       Inicio.
           Display "Data: " at 0201.
           ACCEPT DATA-DO-SISTEMA FROM DATE.
           DISPLAY TELA01 AT 0101.
           MOVE ZEROS TO DADOS.
       Entrada.
      *RECEBENDO VALORES
      *Recebe o primeiro valor
           display MENSA1 AT 0401.
           accept MascaraInt AT 0426.
           move MascaraInt to W-N1.
      *Recebe o segundo valor
           display MENSA2 AT 0601.
           move zeros to MascaraInt.
           accept MascaraInt AT 0626.
           move MascaraInt to W-N2.

       Calcula.
      *Calculo da soma: // usa o add
           add W-N1 W-N2 giving W-Soma.
      *Calculo da subrtacao: // usa subtract e o from para sub n1 do n2
           subtract W-N2 from W-N1 giving W-Subtracao.
      *Calculo da multiplicacao: // usa o multiply e o by para mult o n1 do n2
           multiply W-N1 by W-N2 giving W-Multiplicacao.
      *Calculo da divisao: // usa o divide e o by para div o n1 do n2
           divide W-N2 into W-N1 giving W-Divisao remainder W-Resto.

       Exibe.
      * Exibir a Soma das variáveis.
           display MENSA3 AT 0901.
           move W-Soma to MascaraInt.
           display MascaraInt at 0921.
      * Exibir a Subtracao das variáveis (usa-se if para colocar o número negativo).
           display MENSA4 AT 1001.
           move W-Subtracao to MascaraInt.
           if W-N1 >= W-N2 THEN
           display MascaraInt at 1021
           if W-N1 < W-N2
           display "-" at 1020
           display MascaraInt at 1021
           END-IF.
      * Exibir a Multiplicacao das variáveis.
           display MENSA5 AT 1101.
           move W-Multiplicacao to MascaraMult.
           display MascaraMult at 1121.
      * Exibir a Divisão das variáveis.
           display MENSA6 AT 1201.
           COMPUTE MascaraFloat = W-Divisao * 10.
      *    Ao repassar para a mascara de float, o resultado estava indo uma casa a mais
      *    ao lado, foi necessário multiplicar por 10 para andar uma casa para dar o valor real.
           display MascaraFloat at 1221.
       Finaliza.
           DISPLAY MENSA7 AT 1416.
           Stop " ".
           Stop Run.
