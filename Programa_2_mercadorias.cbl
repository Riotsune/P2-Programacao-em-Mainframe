       identification division.
       program-id. programa_2_mercadorias.
       
       environment division.
       configuration section.
       special-names.
           decimal-point is comma.
       
       data division.
       working-storage section.
       01 Mercadoria pic x(20) value space.
       01 Quantidade pic 9(4) value zeros.
       01 Preco_Unitario pic 9(4)V9(2) value zeros.
       01 Preco_Venda pic 9(4)V9(2) value zeros.
       01 Preco_Total pic 9(6)V9(2) value zeros.
       01 Quantidade-M pic ZZZZ value zeros.
       01 Preco_Unitario-M pic Z(4),ZZ value zeros.
       01 Preco_Venda-M pic Z(4),ZZ value zeros.
       01 Preco_Total-M pic Z(6),ZZ value zeros.
       01 SimNao pic a value space.
       01 LimpaLinha pic a(60) value space.

       screen section.
       01 Tela.
           02 line 01 column 06 value
           "DISTRIBUIDORA DE PRODUTOS LTDA".
           02 line 02 column 02 value "Nome da mercadoria: ".
           02 line 03 column 02 value "Quantidade: ".
           02 line 04 column 02 value "Preco Unitario: ".
           02 line 05 column 02 value "Preco Total: ".
           02 line 06 column 02 value "Preco de Venda: ".
           02 line 08 column 13 value "Continua (S/N) ? < >".
       
       procedure division.
        sumario.
           perform fluxo-padrao.
           perform finaliza-programa.

        fluxo-padrao.
           display Tela with blank screen at 0101.
           perform recebe-valores.
           perform calcula-preco-total.
           perform calcula-preco-venda.
           display Preco_Total-M at 0518.
           display Preco_Venda-M at 0620.
           perform recebe-simnao.
           if SimNao = "S" then
              perform fluxo-padrao
           end-if.
        
        recebe-simnao.
           accept SimNao with prompt at 0831.
           display LimpaLinha at 1519.
           if SimNao = Space then
               display "Nao deixe o campo em branco" at 1519
               perform recebe-simnao
            end-if.
           if SimNao not = "S" and SimNao not = "N" then
               display "Opcao invalida, digite novamente" at 1519
               perform recebe-simnao
            end-if.
           display LimpaLinha at 1519.
           
        recebe-valores.
           accept Mercadoria with prompt at 0222.
           display LimpaLinha at 1519.
           if Mercadoria = Space then
               display "Nao deixe o campo em branco" at 1519
               perform recebe-valores
           end-if.
           display LimpaLinha at 1519.
           perform recebe-quantidade.
           perform recebe-preco-unitario.

        recebe-quantidade.
           accept Quantidade-M with prompt at 0314.
           display LimpaLinha at 1519.
           move Quantidade-M to Quantidade.
           if Quantidade-M = Space then
               display "Nao deixe o campo em branco" at 1519
               perform recebe-quantidade
            end-if.
           if Quantidade <= 0 then
               display "Digite apenas um valor > 0 nesse campo" at 1519
               perform recebe-quantidade
           end-if.
           display LimpaLinha at 1519.

        recebe-preco-unitario.
           accept Preco_Unitario-M with prompt at 0419.
           display LimpaLinha at 1519.
           if Preco_Unitario-M = Space then
               display "Nao deixe o campo em branco" at 1519
               perform recebe-preco-unitario
            end-if.
           move Preco_Unitario-M to Preco_Unitario.
           if Preco_Unitario <= 0 then
               display "Digite apenas um valor > 0 nesse campo" at 1519
               perform recebe-preco-unitario
           end-if.
           display LimpaLinha at 1519.
        
        calcula-preco-total.
           multiply Quantidade by Preco_Unitario giving Preco_Total.
           move Preco_Total to Preco_Total-M.
        
        calcula-preco-venda.
           multiply Preco_Total by 1,3 giving Preco_Venda.
           move Preco_Venda to Preco_Venda-M.
        
        finaliza-programa.
           display "----- Fim do Programa ------" at 1002.
           stop space.
           stop run.
           

       
