       identification division.
       program-id. progama_2_mercadorias.

       data division.
       working-storage section.
       01 Mercadoria pic x(20) value space.
       01 Quantidade pic 9(4) value zeros.
       01 Preco_Unitario pic 9(4)V99 value zeros.
       01 Preco_Venda pic 9(4)V99 value zeros.
       01 Preco_Total pic 9(6)V99 value zeros.
       01 Quantidade-M pic ZZZZ value zeros.
       01 Preco_Unitario-M pic ZZZZ.ZZ value zeros.
       01 Preco_Venda-M pic ZZZZ.ZZ value zeros.
       01 Preco_Total-M pic ZZZZZZ.ZZ value zeros.
       01 SimNao pic a value space.

       screen section.
       01 Tela.
           02 line 01 column 06 value
           "DISTRIBUIDORA DE PRODUTOS LTDA".
           02 line 02 column 02 value "Nome da mercadoria: ".
           02 line 02 column 22 value "____________________".
           02 line 03 column 02 value "Quantidade: ".
           02 line 03 column 14 value "____".
           02 line 04 column 02 value "Preco Unitario: ".
           02 line 04 column 18 value "$______".
           02 line 05 column 02 value "Preco Total: ".
           02 line 05 column 15 value "$________".
           02 line 06 column 02 value "Preco de Venda: ".
           02 line 06 column 18 value "$______".
           02 line 08 column 13 value "Continua (S/N) ? <_>".
       
       procedure division.
        sumario.
           perform fluxo-padrao.
           perform finaliza-programa.

        fluxo-padrao.
           display Tela with blank screen at 0101.
           perform recebe-valores.
           perform calcula-preco-total.
           perform calcula-preco-venda.
           display Preco_Total-M at 0516.
           display Preco_Venda-M at 0619.
           accept SimNao at 0831.
           if SimNao = "S" then
              perform fluxo-padrao
           end-if.

        recebe-valores.
           accept Mercadoria at 0222.
           perform recebe-quantidade.
           perform recebe-preco-unitario.

        recebe-quantidade.
           accept Quantidade-M at 0314.
           move Quantidade-M to Quantidade.
           if Quantidade < 0 then
              perform recebe-quantidade
           end-if.

        recebe-preco-unitario.
           accept Preco_Unitario-M at 0419.
           move Preco_Unitario-M to Preco_Unitario.
           if Preco_Unitario < 0 then
              perform recebe-preco-unitario
           end-if.
        
        calcula-preco-total.
           multiply Quantidade by Preco_Unitario giving Preco_Total.
           move Preco_Total to Preco_Total-M.
        
        calcula-preco-venda.
           multiply Preco_Total by 1.3 giving Preco_Venda.
           move Preco_Venda to Preco_Venda-M.
        
        finaliza-programa.
           display "----- Fim do Programa ------" at 1002.
           stop space.
           stop run.
           

       
