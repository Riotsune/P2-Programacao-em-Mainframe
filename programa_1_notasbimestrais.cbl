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
           02 NotaP1 pic 9(2)V99 value zeros.
           02 NotaP2 pic 9(2)V99 value zeros.
           02 MascaraNota pic zz.zz9,99.
       01 Media pic 9(2)V99 value zeros.
       01 PressEnter pic A.

       procedure division.
           perform mostra-mensagens.
           perform recebe-valores.
           perform calcula-media.
           display MascaraNota at 1425.
           accept PressEnter at 2000
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
           stop run.

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
           display MascaraNota at 0625.
       
       mensagem-aprovacao.
           display "----- APROVADO -----"
               with blank screen  at 0308.
           display "Media: "  at 0608.
           display MascaraNota at 0625.

       recebe-valores.
           accept Aluno at 0625.
           accept MascaraNota at 0825.
           move MascaraNota to NotaP1.
           accept MascaraNota at 1025.
           move MascaraNota to NotaP2.
        
       recebe-exame-suplementar.
           accept MascaraNota at 0625.
           if NotaP1 < NotaP2 then
               move MascaraNota to NotaP1
           else
               move MascaraNota to NotaP2
           end-if.
       
       calcula-media.
           add NotaP1 NotaP2 
           to Media.
           divide Media by 2 giving Media.
           move Media to MascaraNota.
        