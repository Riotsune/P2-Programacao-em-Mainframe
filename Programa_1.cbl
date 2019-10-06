       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ex1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           Special-names.
           Decimal-point is comma.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 ALUNO.
           02 NOME          PIC X(30).
           02 NOTA1-INT     PIC 9(02).
           02 NOTA1-DEC     PIC V9(02).
           02 NOTA2-INT     PIC 9(02).
           02 NOTA2-DEC     PIC V9(02).
           02 MEDIA         PIC 9(02).
           02 MED-DEC       PIC V9(02).
           02 MEDIA-MASK    PIC 99V99.
       01 DATA-SISTEMA.
           02 ANO           PIC 9(02) VALUE ZEROS.
           02 MES           PIC 9(02) VALUE ZEROS.
           02 DIA           PIC 9(02) VALUE ZEROS.
       01 MSG-SAIDA.
           02 MSG-APROVA    PIC X(30) VALUE "ALUNO APROVADO !".
           02 MSG-REPROVA   PIC X(30) VALUE "REPROVADO !".
           02 MSG-CONTINUE  PIC X(20) VALUE "CONTINUAR? (S/N) < >".
           02 MSG-VALERROR  PIC X(30) VALUE "VALOR INVALIDO! MAX 10,00".
           02 MSG-NONAME    PIC X(30) VALUE ">> INSIRA UM NOME ! <<".
       01 SYS-CONTROL.
           02 CONTINUAR     PIC X(01).
           02 EX-LINE       PIC 9(04) VALUE 1810.
           02 TURN          PIC 9(01) VALUE ZEROS.
       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 05      PIC 9(02) USING DIA.
           02 LINE 02 COLUMN 08      PIC 9(02) USING MES.
           02 LINE 02 COLUMN 11      PIC 9(02) USING ANO.
           02 LINE 02 COLUMN 28      VALUE "CALCULO MEDIA".
           02 LINE 06 COLUMN 21      VALUE "NOME ...: ".
           02 LINE 10 COLUMN 21      VALUE "NOTA1 ...: 00,00".
           02 LINE 12 COLUMN 21      VALUE "NOTA2 ...: 00,00".

       PROCEDURE DIVISION.
       INICIO.
           ACCEPT DATA-SISTEMA             FROM DATE.
           DISPLAY TELA.
           PERFORM PROCESSO.
           EXIT.

       PROCESSO.
           MOVE 0 TO MEDIA.
           MOVE 0 TO MEDIA-MASK.
           MOVE 0 TO MED-DEC.
           MOVE 0 TO TURN.
           MOVE 1810 TO EX-LINE.
           PERFORM ENTRA-DADOS.
           EXIT.

       ENTRA-DADOS.
           ACCEPT NOME AT 0632 WITH PROMPT AUTO.
           IF NOME = ""
               DISPLAY MSG-NONAME AT 1930
               PERFORM ENTRA-DADOS
           ELSE
               DISPLAY "                      "  AT 1930
               PERFORM ENTRA-NOTA1
               PERFORM ENTRA-NOTA2
           END-IF
           EXIT.

       ENTRA-NOTA1.
           ACCEPT NOTA1-INT                AT 1032 WITH PROMPT AUTO.
           ACCEPT NOTA1-DEC                AT 1035 WITH PROMPT AUTO.
           IF (NOTA1-INT = 10 AND NOTA1-DEC > 0) OR NOTA1-INT > 10
               DISPLAY MSG-VALERROR            AT 1038
               DISPLAY "00,00"                 AT 1032
               PERFORM ENTRA-NOTA1
           ELSE
               DISPLAY NOTA1-INT          AT 1032
               DISPLAY NOTA1-DEC          AT 1035
               DISPLAY "                           " AT 1038
           END-IF
           EXIT.

       ENTRA-NOTA2.
           ACCEPT NOTA2-INT                AT 1232 WITH PROMPT AUTO.
           ACCEPT NOTA2-DEC                AT 1235 WITH PROMPT AUTO.
           IF (NOTA2-INT = 10 AND NOTA2-DEC > 0) OR NOTA2-INT > 10
               DISPLAY MSG-VALERROR            AT 1238
               DISPLAY "00,00"                 AT 1232
               PERFORM ENTRA-NOTA2
           ELSE
               DISPLAY NOTA2-INT          AT 1232
               DISPLAY NOTA2-DEC          AT 1235
               DISPLAY "                           " AT 1238
           END-IF
           PERFORM CALCULO-MEDIA.
           EXIT.

       CALCULO-MEDIA.
           ADD NOTA1-INT, NOTA2-INT TO MEDIA.
           ADD NOTA1-DEC, NOTA2-DEC TO MEDIA-MASK.
           DIVIDE MEDIA-MASK BY 2 GIVING MED-DEC.
           DIVIDE MEDIA BY 2 GIVING MEDIA-MASK.
           ADD MED-DEC TO MEDIA-MASK.
           PERFORM PRINT-MEDIA.
           EXIT.

       ENTRA-EXAME.
           DISPLAY  "ALUNO FICOU DE EXAME !" AT 1621.
           DISPLAY "EXAME ...: 00,00" AT 1821.
           MOVE 1 TO TURN.
           ADD 600 TO EX-LINE.
           MOVE 0 TO MEDIA.
           MOVE 0 TO MEDIA-MASK.
           MOVE 0 TO MED-DEC.
           ACCEPT NOTA2-INT                AT 1832 WITH PROMPT AUTO.
           ACCEPT NOTA2-DEC                AT 1835 WITH PROMPT AUTO.
           IF (NOTA2-INT = 10 AND NOTA2-DEC > 0) OR NOTA2-INT > 10
               DISPLAY MSG-VALERROR            AT 1838
               DISPLAY "00,00"                 AT 1832
               SUBTRACT 600 FROM EX-LINE
               PERFORM ENTRA-EXAME
           ELSE
               DISPLAY NOTA2-INT          AT 1832
               DISPLAY NOTA2-DEC          AT 1835
               DISPLAY "                           " AT 1838
               PERFORM CALCULO-MEDIA
           END-IF
           EXIT.

       PRINT-MEDIA.
           IF TURN = 0
               DISPLAY "MEDIA ...: "       AT 1421
               DISPLAY MEDIA-MASK
           ELSE
               DISPLAY "MEDIA ...: ("      AT 2021
               DISPLAY NOTA1-INT           AT 2033
               DISPLAY ","                 AT 2035
               DISPLAY NOTA1-DEC           AT 2036
               DISPLAY "+"                 AT 2039
               DISPLAY NOTA2-INT           AT 2041
               DISPLAY ","                 AT 2043
               DISPLAY NOTA2-DEC           AT 2044
               DISPLAY ") = "              AT 2046
               DISPLAY MEDIA-MASK
           END-IF
           PERFORM RESULT-MED.
           EXIT.

       RESULT-MED.
           IF TURN = 0 AND MEDIA-MASK > 6
               DISPLAY MSG-APROVA AT 1621
           ELSE
               IF TURN = 0 AND MEDIA-MASK < 6
                   IF (NOTA2-INT > NOTA1-INT) OR
                      ((NOTA2-INT = NOTA1-INT) AND
                      NOTA2-DEC > NOTA1-DEC)
                       MOVE NOTA2-INT TO NOTA1-INT
                       MOVE NOTA2-DEC TO NOTA1-DEC
                   END-IF
                   PERFORM ENTRA-EXAME
               END-IF
               IF TURN = 1 AND MEDIA-MASK > 6
                   DISPLAY MSG-APROVA AT 2221
               ELSE
                   DISPLAY MSG-REPROVA AT 2221
               END-IF
           END-IF
           PERFORM END-PROGRAM
           EXIT.

       END-PROGRAM.
           DISPLAY MSG-CONTINUE AT EX-LINE.
           ADD 18 TO EX-LINE.
           ACCEPT CONTINUAR AT EX-LINE WITH PROMPT AUTO.
           ADD 5 TO EX-LINE.
           IF CONTINUAR = 'S' OR 's'
               PERFORM INICIO
           ELSE
               IF CONTINUAR = 'N' OR 'n'
                   STOP RUN
               ELSE
                   DISPLAY "Responda com S ou N !" AT EX-LINE
                   SUBTRACT 23 FROM EX-LINE
                   PERFORM END-PROGRAM
               END-IF
           END-IF
           EXIT.
