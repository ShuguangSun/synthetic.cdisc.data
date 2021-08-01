
lookup_tte <- tribble(
      ~ARM,  ~PARAMCD, ~PARAM, ~LAMBDA, ~CNSR_P,
      "ARM A", "OS",  "Overall Survival",          1 / 80,  0.4,
      "ARM B", "OS",  "Overall Survival",          1 / 100, 0.2,
      "ARM C", "OS",  "Overall Survival",          1 / 60,  0.42,
      "ARM A", "PFS", "Progression Free Survival", 1 / 100, 0.3,
      "ARM B", "PFS", "Progression Free Survival", 1 / 150, 0.1,
      "ARM C", "PFS", "Progression Free Survival", 1 / 80,  0.32,
      "ARM A", "EFS", "Event Free Survival",       1 / 80,  0.2,
      "ARM B", "EFS", "Event Free Survival",       1 / 100, 0.08,
      "ARM C", "EFS", "Event Free Survival",       1 / 60,  0.23
)

tte_varnames <-c("ASEQ", "TTESEQ", "PARAM", "PARAMCD", "AVAL", "AVALU", "ADTM",
                 "ADY", "CNSR", "EVNTDESC", "CNSDTDSC")

s_tte_paramcd <- c("OS", "PFS", "EFS")
s_tte_param <- c("OS" = "Overall Survival",
                 "PFS" = "Progression Free Survival",
                 "Event Free Survival")

join_paramcd_tte <- function(n, .df, .db) {
    .dbtab <- .db[["ADSL"]]
    merge(.dbtab, lookup_tte, by = "ARMCD", by.y = "ARM", all = TRUE)
}



evntdescr_sel <- c(
      "Death",
      "Disease Progression",
      "Last Tumor Assessment",
      "Adverse Event",
      "Last Date Known To Be Alive"
)

cnsdtdscr_sel <- c(
    "Preferred Term",
    "Clinical Cut Off",
    "Completion or Discontinuation",
    "End of AE Reporting Period"
)


gen_tte_cnsr <- function(n, .df, ...) runif(NROW(.df)) <= .df$CNSR_P
gen_tte_aval <- function(n, .df, ...) tibble(AVAL = rexp(NROW(.df), .df$LAMBDA)/365.25,
                                             AVALU = "YEARS")

gen_tte_descs <- function(n, .df, edesc = evntdescr_sel, cnsdesc = cnsdtdscr_sel, ...) {
    n <- NROW(.df)
    tibble(EVNTDESC = ifelse(.df$CNSR, "", sample(edesc, n, replace = TRUE)),
           CNSTDESC = ifelse(.df$CNSR, sample(cnsdesc, n, replace = TRUE), ""))
}

#' Recipes for creating ADTTE CDISC Data
#'
#' @rdname adtte_recipes
#' @export
tte_rel_join_recipe <- tribble(
    ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables, ~dependencies, ~func, ~func_args,
    "ADSL", "USUBJID", "ARMCD", c("PARAMCD", "PARAM", "LAMBDA", "CNSR_P"), no_deps, join_paramcd_tte, NULL)

#' @rdname adtte_recipes
#' @export
tte_table_recipe <- tribble(
    ~variables, ~dependencies, ~func, ~func_args,
    "CNSR", "CNSR_P", gen_tte_cnsr, NULL,
    c("AVAL", "AVALU"), "LAMBDA", gen_tte_aval, NULL,
    c("EVNTDESC", "CNSTDESC"), "CNSR", gen_tte_descs, NULL)

