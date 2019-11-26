       PROGRAM-ID. Meses.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

           01 MESES-ANO.
               02 FILLER PIC X(9) VALUE "Janeiro".
               02 FILLER PIC X(9) VALUE "Fevereiro".
               02 FILLER PIC X(9) VALUE "Março".
               02 FILLER PIC X(9) VALUE "Abril".
               02 FILLER PIC X(9) VALUE "Maio".
               02 FILLER PIC X(9) VALUE "Junho".
               02 FILLER PIC X(9) VALUE "Julho".
               02 FILLER PIC X(9) VALUE "Agosto".
               02 FILLER PIC X(9) VALUE "Setembro".
               02 FILLER PIC X(9) VALUE "Outubro".
               02 FILLER PIC X(9) VALUE "Novembro".
               02 FILLER PIC X(9) VALUE "Dezembro".

           01 TABELA-MESES REDEFINES MESES-ANO.
               02 MES-T PIC X(9) OCCURS 12 TIMES.

           01 Data-Qualquer.
               02 ws-dia pic 9(02) value zeros.
                   88 verifica-dia value 01 thru 31.
               02 ws-mes pic 9(02) value zeros.
                   88 verifica-mes value 01 thru 12.
               02 ws-ano pic 9(04) value zeros.
                   88 verifica-ano value 0001 thru 9999.
               02 ws-opc pic x value space.

           01 Mensagens-de-Critica.
               02 mensa0 pic x(60) value spaces.
               02 mensa1 pic x(60) value
               "Dia invalido, digite um numero entre 01 e 31".
               02 mensa2 pic x(60) value
               "Mes invalido, digite um numero entre 01 e 12.".
               02 mensa3 pic x(60) value
               "Ano invalido, digite um numero entre 0001 e 9999.".
               02 mensa4 pic x(45) value
               "Opcao invalida, favor redigitar".

       SCREEN SECTION.

           01 Tela.
             02 Blank Screen.
             02 line 02 column 05 value "= Programa | Meses do Ano | =".
             02 line 08 column 02 value
             "Digite uma data para ser convertida:    /    /      .".
             02 line 19 column 02 value "Continua? (S/N) < > ".

           01 Tela-Fim.
              02 Blank Screen.
              02 line 23 column 31 value
             " = Fim do Programa = ".

       PROCEDURE DIVISION.

       03-Rot-Inicio.
           display Tela.
           initialize Data-Qualquer.
           PERFORM 05-Rot-Verifica-Dia THRU 09-Rot-Continua.

       05-Rot-Verifica-Dia.
           accept ws-dia at 0839 with prompt auto.
           display mensa0 at 2331.
           if verifica-dia
           else display mensa1 at 2331
               perform 05-Rot-Verifica-Dia.

       06-Rot-Verifica-Mes.
           accept ws-mes at 0844 with prompt auto.
           display mensa0 at 2331.

           if verifica-mes
            else display mensa2 at 2331
               perform 06-Rot-Verifica-Mes.
               if ws-mes = '04' and ws-dia = '31'
               or ws-mes = '06' and ws-dia = '31'
               or ws-mes = '08' and ws-dia = '31'
               or ws-mes = '09' and ws-dia = '31'
               or ws-mes = '11' and ws-dia = '31'

                 display "Este mes nao possui 31 dias. Redigite."
                 at 2331
                 perform 05-Rot-Verifica-Dia
                 perform 06-Rot-Verifica-Mes.

           if ws-mes = '02' and ws-dia > '28'
               display "Este mes nao possui mais de 28 dias. Redigite."
               at 2331
               perform 05-Rot-Verifica-Dia
               perform 06-Rot-Verifica-Mes.

       07-Rot-Verifica-Ano.
           accept ws-ano at 0849 with prompt auto.
           display mensa0 at 2331.
           if verifica-ano
           else display mensa3 at 2331
               perform 07-Rot-Verifica-Ano.

       08-MOSTRA.
           DISPLAY "Data por extenso:    de           de     ."
           AT 1302.
           Display ws-dia AT 1320.
           Display MES-T(ws-mes) AT 1326.
           Display ws-ano AT 1339.

       09-Rot-Continua.
           accept ws-opc with prompt at 1919.
           display mensa0 at 2332.
           if ws-opc = 'S' or 's' perform 03-Rot-Inicio
           else if ws-opc = 'N' or 'n' perform 10-Fim
           else display mensa4 at 2331
           perform 09-Rot-Continua.
       10-Fim.
           Display Tela-Fim.
           Stop ''.
           stop run.
