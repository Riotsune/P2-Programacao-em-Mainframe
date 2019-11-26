       PROGRAM-ID. SiglaDosEstados.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

           01 ESTADOS.

               02 FILLER PIC A(30) VALUE "Acre".
               02 FILLER PIC A(30) VALUE "Alagoas".
               02 FILLER PIC A(30) VALUE "Amapa".
               02 FILLER PIC A(30) VALUE "Amazonas".
               02 FILLER PIC A(30) VALUE "Bahia".
               02 FILLER PIC A(30) VALUE "Ceara".
               02 FILLER PIC A(30) VALUE "Espirito Santo".
               02 FILLER PIC A(30) VALUE "Goias".
               02 FILLER PIC A(30) VALUE "Maranhao".
               02 FILLER PIC A(30) VALUE "Mato Grosso".
               02 FILLER PIC A(30) VALUE "Mato Grosso do Sul".
               02 FILLER PIC A(30) VALUE "Minas Gerais".
               02 FILLER PIC A(30) VALUE "Para".
               02 FILLER PIC A(30) VALUE "Paraiba".
               02 FILLER PIC A(30) VALUE "Parana".
               02 FILLER PIC A(30) VALUE "Pernambuco".
               02 FILLER PIC A(30) VALUE "Piaui".
               02 FILLER PIC A(30) VALUE "Rio de Janeiro".
               02 FILLER PIC A(30) VALUE "Rio Grande do Norte".
               02 FILLER PIC A(30) VALUE "Rio Grande do Sul".
               02 FILLER PIC A(30) VALUE "Rondonia".
               02 FILLER PIC A(30) VALUE "Roraima".
               02 FILLER PIC A(30) VALUE "Santa Catarina".
               02 FILLER PIC A(30) VALUE "Sao Paulo".
               02 FILLER PIC A(30) VALUE "Sergipe".
               02 FILLER PIC A(30) VALUE "Tocantins".
               02 FILLER PIC A(30) VALUE "Distrito Federal".

           01 TABELA-ESTADO REDEFINES ESTADOS.
               02 ESTADO-T PIC A(30) OCCURS 26 TIMES.

           01 Sigla-Qualquer.
               02 ws-estado pic A(02) value space.
                  88 verifica-sigla value "AC" "AL" "AP" "AM" "BA"
                  "CE" "ES" "GO" "MA" "MT" "MS" "MG" "PA" "PB" "PR" "PE"
                   "PI" "RJ" "RN" "RS" "RO" "RR" "SC" "SP" "SE" "TO"
                   "DF" "ac" "al" "ap" "am" "ba"
                  "ce" "es" "go" "ma" "mt" "ms" "mg" "pa" "pb" "pr" "pe"
                   "pi" "rj" "rn" "rs" "ro" "rr" "sc" "sp" "se" "to"
                   "df".
               02 ws-opc pic A value space.
               02 ws-aux pic 9(02) value zeros.

           01 Mensagens-de-Critica.

               02 mensa0 pic x(60) value spaces.

               02 mensa2 pic x(45) value
               "Estado inexistente".

               02 mensa3 pic x(45) value
               "Opcao invalida, favor redigitar".

       SCREEN SECTION.

           01 Tela.

             02 Blank Screen.
             02 line 02 column 05 value
             "= Programa | Siglas do Estado | =".
             02 line 08 column 02 value
             "Digite a sigla do estado que deseja exibir:".
             02 line 19 column 02 value "Continua? (S/N) < > ".

           01 Tela-Fim.

              02 Blank Screen.
              02 line 23 column 31 value
             "=Fim do Programa, pressione qualquer tecla para fechar=".

       PROCEDURE DIVISION.

       03-Rot-Inicio.

           display Tela.
           initialize Sigla-Qualquer.
           PERFORM 05-Rot-Verifica-Sigla THRU 07-Rot-Continua.

       05-Rot-Verifica-Sigla.

           accept ws-estado at 0846.
           display mensa0 at 2331.
           if verifica-sigla
           else display mensa2 at 2331
               perform 05-Rot-Verifica-Sigla
           end-if.

       06-MOSTRA.

           DISPLAY "Estado:"
           AT 1302.
           if ws-estado = 'AC' or 'ac' then
              move 1 to ws-aux
           else if ws-estado = "AL" or "al" then
              move 2 to ws-aux
           else if ws-estado = "AP" or "ap" then
               move 3 to ws-aux
           else if ws-estado = "AM" or "am" then
               move 4 to ws-aux
           else if ws-estado = "BA"  or "ba" then
               move 5 to ws-aux
           else if ws-estado = 'CE'  or "ce" then
               move 6 to ws-aux
           else if ws-estado = 'ES' or "es" then
               move 7 to ws-aux
           else if ws-estado = 'GO' or "go" then
               move 8 to ws-aux
           else if ws-estado = 'MA' or "ma" then
               move 9 to ws-aux
           else if ws-estado = 'MT' or "mt" then
               move 10 to ws-aux
           else if ws-estado = 'MS' or "ms" then
               move 11 to ws-aux
           else if ws-estado = 'MG' or "mg" then
               move 12 to ws-aux
           else if ws-estado = 'PA' or "pa" then
               move 13 to ws-aux
           else if ws-estado = 'PB' or "pb" then
               move 14 to ws-aux
           else if ws-estado = 'PR' or "pr" then
               move 15 to ws-aux
           else if ws-estado = 'PE' or "pe" then
               move 16 to ws-aux
           else if ws-estado = 'PI' or "pi" then
               move 17 to ws-aux
           else if ws-estado = 'RJ' or "rj" then
               move 18 to ws-aux
           else if ws-estado = 'RN' or "rn" then
               move 19 to ws-aux
           else if ws-estado = 'RS' or "rs" then
               move 20 to ws-aux
           else if ws-estado = 'RO' or "ro" then
               move 21 to ws-aux
           else if ws-estado = 'RR' or "rr" then
               move 22 to ws-aux
           else if ws-estado = 'SC' or "sc" then
               move 23 to ws-aux
           else if ws-estado = 'SP' or "sp" then
               move 24 to ws-aux
           else if ws-estado = 'SE' or "se" then
               move 25 to ws-aux
           else if ws-estado = 'TO' or "to" then
               move 26 to ws-aux.

           Display ESTADO-T(ws-aux) AT 1310.

       07-Rot-Continua.

           accept ws-opc with prompt at 1919.
           display mensa0 at 2332.
           if ws-opc = 'S' or 's' perform 03-Rot-Inicio
           else if ws-opc = 'N' or 'n' perform 08-Fim
           else display mensa3 at 2331
           perform 07-Rot-Continua.

       08-Fim.

           Display Tela-Fim.
           Stop ''.
           stop run.
