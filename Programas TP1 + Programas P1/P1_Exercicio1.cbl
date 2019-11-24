      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 CALC.
           02 N1 PIC 9(02).
           02 N2 PIC 9(21).
           02 N3 PIC 9(21).
           02 N4 PIC 9(21).
           02 M1 PIC z(21) VALUE SPACES.
           02 CONT PIC 9(02).
           02 AUCX PIC 9(1).
           02 X PIC 9(04).

       01 MSG.
           02 SAIR PIC X(18) VALUE "Continuar (S/N) : ".
           02 RSPT PIC X(01) VALUE SPACES.
           02 ERRO PIC X(35) VALUE "Valor 0 invalido,nenhum resultado!".
           02 FIBO PIC X(30) VALUE "Numeros FIBONACCI : ".
           02 INVA PIC X(30) VALUE "Responda com S ou N".
       SCREEN SECTION.
       01 TELA.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 VALUE "Insira a qtd de N a exibir : ".
       PROCEDURE DIVISION.
       01-MAIN.
           DISPLAY TELA.
           MOVE ZERO TO CONT.
           MOVE 1 TO N2.
           MOVE 0 TO N3.
           MOVE 0302 TO X.
           MOVE 0 TO AUCX.
           PERFORM ENTRA-DADOS.
       EXIT.

       ENTRA-DADOS.
           ACCEPT N1 AT 0131 WITH PROMPT AUTO.
           IF N1 = 0
               DISPLAY ERRO AT X
           ELSE
               DISPLAY FIBO AT X
               ADD 42 TO X
           END-IF
           PERFORM FIBONACCI UNTIL CONT = N1.
           PERFORM CONTINUAR.
       EXIT.

       FIBONACCI.
           IF CONT <= 1
               DISPLAY CONT AT X
           ELSE
               MOVE N2 TO N4
               ADD N3 TO N2
               MOVE N2 TO M1
               DISPLAY M1 AT X
               MOVE N4 TO N3
           END-IF
           IF CONT = 2
               ADD 0100 TO X
           END-IF
           IF AUCX = 5
               MOVE 0 TO AUCX
               ADD 0100 TO X
               SUBTRACT 110 FROM X
           END-IF
           ADD 22 TO X.
           ADD 1 TO CONT.
           IF CONT > 2
               ADD 1 TO AUCX
           END-IF
       EXIT.

       CONTINUAR.
           DISPLAY SAIR AT 2601.
           ACCEPT RSPT AT 2619 WITH PROMPT AUTO.
           IF RSPT = 'N' OR RSPT = 'n'
               STOP RUN
           END-IF
           IF RSPT = 'S' OR RSPT = 's'
               PERFORM 01-MAIN
           ELSE IF RSPT <> 'S' OR RSPT <> 's'
               DISPLAY INVA AT 2801
               PERFORM CONTINUAR
           END-IF
           DISPLAY "                        " AT 2801.
       EXIT.
