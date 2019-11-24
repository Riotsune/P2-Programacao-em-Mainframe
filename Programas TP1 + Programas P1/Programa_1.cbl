       IDENTIFICATION DIVISION.
       PROGRAM-ID. Programa1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           Special-names.
           Decimal-point is comma.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 ALUNO.
           02 NOME          PIC X(30).
           02 NOTA1-INT     PIC 9(02).
           02 NOTA1-DEC     PIC 9(02).
           02 NOTA2-INT     PIC 9(02).
           02 NOTA2-DEC     PIC 9(02).
           02 MEDIA         PIC 9(02).
           02 MED-IMPAR     PIC V9(02).
           02 CALC-DEC      PIC 9(03).
           02 MASK-DEC      PIC 9(02).
       01 DATA-SISTEMA.
           02 ANO           PIC 9(02) VALUE ZEROS.
           02 MES           PIC 9(02) VALUE ZEROS.
           02 DIA           PIC 9(02) VALUE ZEROS.
       01 MSG-SAIDA.
           02 MSG-APROVA    PIC X(30) VALUE "ALUNO APROVADO !".
           02 MSG-REPROVA   PIC X(30) VALUE "REPROVADO !".
           02 MSG-EXAME     PIC X(30) VALUE "ALUNO FICOU DE EXAME !".
           02 MSG-IFEXAME   PIC X(11) VALUE "EXAME ...: ".
           02 MSG-CONTINUE  PIC X(30) VALUE "CONTINUAR? (s/n) ".
           02 MSG-VALERROR  PIC X(30) VALUE "VALOR INVALIDO! MAX 10,00".
       01 SYS-CONTROL.
           02 CONTINUAR     PIC X(01) VALUE SPACES.
           02 EX-LINE       PIC 9(04) VALUE 2010.
           02 TURN          PIC 9(01) VALUE ZEROS.
       SCREEN SECTION.
       01 TELA.
           02 BLANK-SCREEN.
           02 LINE 02 COLUMN 05      PIC 9(02) USING DIA.
           02 LINE 02 COLUMN 08      PIC 9(02) USING MES.
           02 LINE 02 COLUMN 11      PIC 9(02) USING ANO.
           02 LINE 02 COLUMN 28      VALUE "CALCULO MEDIA".
           02 LINE 08 COLUMN 21      VALUE "NOME ...: ".
           02 LINE 12 COLUMN 21      VALUE "NOTA1 ...: 00,00".
           02 LINE 14 COLUMN 21      VALUE "NOTA2 ...: 00,00".
           02 LINE 16 COLUMN 21      VALUE "MEDIA ...: 00,00".

       PROCEDURE DIVISION.
       INICIO.
           ACCEPT DATA-SISTEMA             FROM DATE.
           PERFORM PROCESSO                UNTIL CONTINUAR = 'n'.
           PERFORM SAIDA.
           EXIT.

       PROCESSO.
           DISPLAY TELA.
           MOVE ZEROS                      TO TURN.
           MOVE 2010                       TO EX-LINE.
           MOVE 0 TO MEDIA, CALC-DEC.
           PERFORM ENTRA-DADOS             UNTIL TURN = 2.
           DISPLAY MSG-CONTINUE            AT EX-LINE.
           ADD 10                          TO EX-LINE.
           ACCEPT CONTINUAR                AT EX-LINE WITH PROMPT AUTO.
           EXIT.

       ENTRA-DADOS.
           ACCEPT NOME                     AT 0832 WITH PROMPT AUTO.
           PERFORM ENTRA-NOTA1.
           PERFORM ENTRA-NOTA2.
           PERFORM CALCULO-MEDIA.
           EXIT.

       ENTRA-NOTA1.
           ACCEPT NOTA1-INT                AT 1232 WITH PROMPT AUTO.
           ACCEPT NOTA1-DEC                AT 1235 WITH PROMPT AUTO.
           IF NOTA1-INT = 10
               IF NOTA1-DEC > 0
                  PERFORM ERROR-NOTA1
               END-IF
               ELSE
                   IF NOTA1-INT > 10
                       PERFORM ERROR-NOTA1
                   END-IF
           END-IF
           DISPLAY "                         "      AT 1238.
           EXIT.

       ERROR-NOTA1.
           DISPLAY MSG-VALERROR            AT 1238.
           DISPLAY "00,00"                 AT 1232.
           PERFORM ENTRA-NOTA1.
           EXIT.

       ENTRA-NOTA2.
           ACCEPT NOTA2-INT                AT 1432 WITH PROMPT AUTO.
           ACCEPT NOTA2-DEC                AT 1435 WITH PROMPT AUTO.
           IF NOTA2-INT = 10
               IF NOTA2-DEC > 0
                  PERFORM ERROR-NOTA2
               END-IF
               ELSE
                   IF NOTA2-INT > 10
                       PERFORM ERROR-NOTA2
                   END-IF
           END-IF
           DISPLAY "                         " AT 1438.
           EXIT.

       ERROR-NOTA2.
           DISPLAY MSG-VALERROR AT 1438.
           DISPLAY "00,00"      AT 1432.
           PERFORM ENTRA-NOTA2.
           EXIT.

       CALCULO-MEDIA.
           ADD NOTA1-INT, NOTA2-INT TO MEDIA.
           ADD NOTA1-DEC, NOTA2-DEC TO CALC-DEC.
           DIVIDE MEDIA BY 2 GIVING MED-IMPAR.
           DIVIDE CALC-DEC BY 2 GIVING CALC-DEC.
           IF MED-IMPAR >= 0,50
               ADD 50 TO CALC-DEC
           END-IF
           IF CALC-DEC >= 100
               SUBTRACT 100 FROM CALC-DEC
               ADD 1 TO MEDIA
           END-IF
           DIVIDE MEDIA BY 2 GIVING MEDIA.
           PERFORM PRINT-MEDIA.
           IF MEDIA < 6
               IF NOTA1-INT = NOTA2-INT
                   IF NOTA1-DEC < NOTA2-DEC
                       MOVE NOTA2-DEC TO NOTA1-DEC
                   END-IF
               ELSE
                   IF NOTA2-INT > NOTA1-INT
                       MOVE NOTA2-INT TO NOTA1-INT
               END-IF
               IF TURN = 0
                   PERFORM ENTRA-EXAM
               ELSE
                   PERFORM RESULT-FALSE
               END-IF
           ELSE
               PERFORM RESULT-TRUE
           END-IF
           EXIT.

       START-EXAM.
           ADD 1 TO TURN.
           ADD 600 TO EX-LINE.
           DISPLAY MSG-IFEXAME        AT 2021.
           DISPLAY "00,00"            AT 2032.
           EXIT.

       ENTRA-EXAM.
           PERFORM START-EXAM.
           DISPLAY MSG-EXAME               AT 1821.
           ACCEPT NOTA2-INT                AT 2032 WITH PROMPT AUTO.
           ACCEPT NOTA2-DEC                AT 2035 WITH PROMPT AUTO.
           IF NOTA2-INT = 10
               IF NOTA2-DEC > 0
                  PERFORM ERROR-EXAM
               END-IF
               ELSE
                   IF NOTA2-INT > 10
                       PERFORM ERROR-EXAM
                   END-IF
           END-IF
           DISPLAY "                         "      AT 2038.
           MOVE 0 TO MEDIA, CALC-DEC.
           PERFORM CALCULO-MEDIA.
           EXIT.

       ERROR-EXAM.
           DISPLAY MSG-VALERROR            AT 2038.
           PERFORM ENTRA-EXAM.
           EXIT.

       RESULT-TRUE.
           IF TURN = 0
               DISPLAY MSG-APROVA         AT 1821
           ELSE
               DISPLAY MSG-APROVA         AT 2421
           END-IF
           MOVE 2 TO TURN.
           EXIT.

       RESULT-FALSE.
           DISPLAY MSG-REPROVA     AT 2421.
           ADD 1 TO TURN.
           EXIT.

       PRINT-MEDIA.
           MOVE CALC-DEC TO MASK-DEC
           IF TURN = 0
               DISPLAY MEDIA              AT 1632
               DISPLAY ','                AT 1634
               DISPLAY MASK-DEC           AT 1635
           ELSE
               DISPLAY "MEDIA ...: ("      AT 2221
               DISPLAY NOTA1-INT           AT 2233
               DISPLAY ","                 AT 2235
               DISPLAY NOTA1-DEC           AT 2236
               DISPLAY "+"                 AT 2239
               DISPLAY NOTA2-INT           AT 2241
               DISPLAY ","                 AT 2243
               DISPLAY NOTA2-DEC           AT 2244
               DISPLAY ") = "              AT 2246
               DISPLAY MEDIA               AT 2250
               DISPLAY ','                 AT 2252
               DISPLAY MASK-DEC            AT 2253
           END-IF
           EXIT.

       SAIDA.
           STOP RUN.
           EXIT.
