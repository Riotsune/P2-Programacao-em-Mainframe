
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Agencia.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 Cadastro.
               02 Nome PIC A(30) VALUE SPACES.
                   88 Nomelimite VALUE "A" THRU "z".
               02 Idade PIC 9(2) VALUE ZEROS.
                   88 limiteidade value 18 thru 50.
               02 SEXO PIC x VALUE SPACE.
                   88 limitefm VALUE "F" "M" "f" "m".
               02 SALARIO PIC 9(8)v99 VALUE ZEROS.
               02 SALARIOM PIC zz,zz9.99 value zeros.
               02 CODIGO PIC 9(1) VALUE ZEROS.
                   88 limitecodigo value 1 THRU 6.
               02 CONTINUA PIC X VALUE SPACE.
               02 LIMPA PIC X(50) VALUE SPACE.



       SCREEN SECTION.
           01 Tela.
               02 BLANK SCREEN.
               02 line 02 column 28 value "---Carta Agencia---".
               02 line 08 column 21 value "Nome: ".
               02 line 10 column 21 value "Sexo: ".
               02 line 12 column 21 value "Pretensao Salarial: ".
               02 line 14 column 21 value "Idade: ".
               02 line 19 column 21 value "Codigo de Profissao (1~6): ".

           01 TelaResultado.
               09 BLANK SCREEN.
               09 line 02 column 21 value "Nome: ".
               09 line 04 column 21 value "Profissao: ".
               09 line 08 column 21 value "Continua? (S/N) < > ".

       PROCEDURE DIVISION.

       03-INICIO.
               DISPLAY Tela.
               INITIALIZE Cadastro.
               PERFORM 04-ENTRADANOME THRU 10-CONTINUA.

       04-ENTRADANOME.
               ACCEPT Nome AT 0827.
               IF Nomelimite
                   ELSE
                   DISPLAY "Favor digitar um nome valido." at 2121
                   PERFORM 04-ENTRADANOME
               END-IF.
               DISPLAY LIMPA AT 2121.

       05-ENTRASEXO.
               ACCEPT SEXO AT 1027.
               IF limitefm
                   ELSE DISPLAY "Favor digitar 'M' ou 'F'." at 2121
                       PERFORM 05-ENTRASEXO
               END-IF.
               DISPLAY LIMPA AT 2121.

       06-ENTRASALARIO.
               ACCEPT SALARIOM AT 1241.
               MOVE SALARIOM TO SALARIO.
               IF SALARIO > 999999.99 or = 0
                   DISPLAY "Favor digitar um valor valido." at 2121
                   PERFORM 06-ENTRASALARIO
               END-IF.
               DISPLAY LIMPA AT 2121.

       07-ENTRAIDADE.
               ACCEPT IDADE AT 1428.
               IF limiteidade
                   ELSE
               DISPLAY "Favor digitar um numero entre 18 e 50." at 2121
               PERFORM 07-ENTRAIDADE
               END-IF.
               DISPLAY LIMPA AT 2121.

       08-ENTRACODIGO.
               ACCEPT CODIGO AT 1948.
               IF limitecodigo
               ELSE DISPLAY "Favor digitar um codigo valido." at 2121
                   PERFORM 08-ENTRACODIGO
               END-IF.
               DISPLAY LIMPA AT 2121.

       09-SAIDADADOS.
               DISPLAY TelaResultado.
               DISPLAY Nome AT 0227.
               if CODIGO = 1 THEN
                   DISPLAY "Cartografo" at 0432
                   END-IF.
               if CODIGO = 2 THEN
                   DISPLAY "Assistente Social" at 0432
                   END-IF.
               if CODIGO = 3 THEN
                   DISPLAY "Psicologo" at 0432
                   END-IF.
               if CODIGO = 4 THEN
                   DISPLAY "Atendente" at 0432
                   END-IF.
               if CODIGO = 5 THEN
                   DISPLAY "Secretaria Bilingue" at 0432
                   END-IF.
               if CODIGO = 6 THEN
                   DISPLAY "Geologo" at 0432
                   END-IF.

       10-CONTINUA.
           ACCEPT CONTINUA AT 0838.
           IF CONTINUA = 'S' or 's' perform 03-INICIO
           else if CONTINUA = 'N' or 'n' display
               "Fim do Programa" at 2321 perform 11-Fim
           else display "Opcao invalida, favor redigitar." at 1021
           perform 10-CONTINUA.

       11-FIM.
           stop ''.
           stop run.
