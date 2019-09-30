       identification division.
       program-id. programa_1_notasbimestrais.
       
       environment division.
       configuration section.
       special-names.
           decimal-point is comma.

       data division.
       working-storage section.
       01 Notas.
           02 PrimeiraNota pic 9(2)V99 value zeros.
           02 SegundaNota pic 9(2)V99 value zeros.
           02 TerceiraNota pic 9(2)V99 value zeros.
           02 QuartaNota pic 9(2)V99 value zeros.
           02 MascaraNota pic zz.zz9,99.
       01 Media pic 9(2)V99 value zeros.

       procedure division.
           perform mostra-mensagens.
           perform recebe-valores.
           perform calcula-media.
           stop run.

       mostra-mensagens.
           display "----- Digite as suas notas -----" at 0303.
           display "1o Bimestre: "  at 0608.
           display "2o Bimestre: "  at 0808.
           display "3o Bimestre: "  at 1008.
           display "4o Bimestre: " at 1208.
           display "Media: " at 1508.

       recebe-valores.
           accept MascaraNota at 0625.
           move MascaraNota to PrimeiraNota.
           accept MascaraNota at 0825.
           move MascaraNota to SegundaNota.
           accept MascaraNota at 1025.
           move MascaraNota to TerceiraNota.
           accept MascaraNota at 1225.
           move MascaraNota to QuartaNota.
       
       calcula-media.
           add PrimeiraNota SegundaNota TerceiraNota QuartaNota 
           to Media.
           divide Media by 4 giving Media.
           move Media to MascaraNota.
           display MascaraNota at 1525.
        