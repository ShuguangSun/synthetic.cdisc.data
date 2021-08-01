library(respectables)
library(synthetic.cdisc.data)
library(dplyr)


tstadsl <- gen_table_data(N = 10, recipe = adsl_tbl_recipe)

tstadae <- gen_reljoin_table(adae_scaff_recipe,
                             adae_tbl_recipe,
                             db = list(ADSL = tstadsl))


tstadtte <- gen_reljoin_table(tte_scaff_recipe,
                              tte_tbl_recipe,
                              db = list(ADSL = tstadsl))

tstadeg <- gen_reljoin_table(adeg_scaff_recipe,
                             adeg_tbl_recipe,
                             db = list(ADSL = tstadsl))

tstadaette <- gen_reljoin_table(adaette_scaff_recipe,
                                adaette_tbl_recipe,
                                db = list(ADSL = tstadsl,
                                          ADAE = tstadae))
