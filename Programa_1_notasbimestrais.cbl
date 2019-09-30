       identification division.
       program-id. programa_1_notasbimestrais.
       
       environment division.
       configuration section.
       special-names.
           decimal-point is comma.

       data division.
       working-storage section.
       01 Aluno pic A(60) value space.
       01 Notas.
           02 NotaP1 pic 99V99 value zeros.
           02 NotaP2 pic 99V99 value zeros.
           02 NotaP3 pic 99V99 value zeros.
           02 NotaP1-M pic ZZ9,99 value zeros.
           02 NotaP2-M pic ZZ9,99 value zeros.
           02 NotaP3-M pic ZZ9,99 value zeros.
       01 Media pic 99V99 value zeros.
       01 Media-M pic ZZ9,99 value zeros.
       01 PressEnter pic A.

       procedure division.
        inicio.
           perform mostra-mensagens.
           perform recebe-valores.
           perform calcula-media.
           display Media-M at 1425.
           accept PressEnter at 1435.
           if Media < 6 then
              perform mensagem-reprovacao1
              perform recebe-exame-suplementar
              perform calcula-media
              if Media < 6 then
                 perform mensagem-reprovacao2
              else
                 perform mensagem-aprovacao
              end-if
           else
              perform mensagem-aprovacao
           end-if.
           perform finaliza.

       mostra-mensagens.
           display "----- Digite as suas notas -----" at 0308.
           display "Digite seu nome: "  at 0608.
           display "P1: "  at 0808.
           display "P2: "  at 1008.
           display "Media: "  at 1408.

       mensagem-reprovacao1.
           display "----- Sua media nao alcancou o valor de 6 -----"
               with blank screen 
           at 0308.
           display "P3: "  at 0608.
       
       mensagem-reprovacao2.
           display "----- REPROVADO -----"
               with blank screen  at 0308.
           display "Media: "  at 0608.
           display Media-M at 0625.
       
       mensagem-aprovacao.
           display "----- APROVADO -----"
               with blank screen  at 0308.
           display "Media: "  at 0608.
           display Media-M at 0625.

       recebe-valores.
           accept Aluno at 0625.
           perform recebe-notas.
       
       recebe-notas.
           accept NotaP1-M at 0825.
           move NotaP1-M to NotaP1.
           if NotaP1 > 10 or NotaP1 < 0
               perform recebe-notas
           end-if.
           accept NotaP2-M at 1025.
           move NotaP2-M to NotaP2.
           if NotaP2 > 10 or NotaP2 < 0
               display space with blank screen
               display Aluno at 0625
               perform mostra-mensagens
               perform recebe-notas
           end-if.
        
       recebe-exame-suplementar.
           accept NotaP3-M at 0625.
           move NotaP3-M to NotaP3.
           if NotaP3 > 10 or NotaP3 < 0
               perform recebe-exame-suplementar
           end-if.
           if NotaP1 < NotaP2 then
               move NotaP3 to NotaP1
           else
               move NotaP3 to NotaP2
           end-if.
       
       calcula-media.
           move zeros to Media.
           add NotaP1 NotaP2 
           to Media.
           divide Media by 2 giving Media.
           move Media to Media-M.
       
       finaliza.
           display "----- Fim do Programa ------" at 0808.
           stop space.
           stop run.
        
