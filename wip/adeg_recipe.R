
lookup_adeg <- tribble(
  ~PARAMCD,     ~PARAM,                ~PARAMU,     ~mean_aval, ~sd_aval, ~ANRLO,   ~ANRHI,
  "QT",         "QT Duration",         "msec",      350,        100,      200,      500,
  "PR",         "RR Duration",         "msec",      1050,       300,      600,      1500,
  "HR",         "Heart Rate",          "beats/min", 70,         20,       40,       100,
  "ECGINTP",    "ECG Interpretation",  "",          NA_real_,   NA_real_, NA_real_, NA_real_
)

#' Helper functions and constants for ADEG
#'

adeg_varnames <-c("PARAM", "PARAMCD", "ADTM", "AVISIT", "AVISITN", "AVAL", "AVALU", "ABLF", "BASE", "CHG")

s_adeg_paramcd <- c("QT", "RR", "HR", "ECGINTP")

s_adeg_param <- c("QT" = "QT Duration",
                 "RR" = "RR Duration",
                 "HR" = "Heart Rate",
                 "ECGINTP" = "ECG Interpretation")

s_adeg_paramu <-  c("msec", "msec", "beats/min", "")

avaldescr_sel <- c("ABNORMAL","NORMAL")

join_paramcd_adeg <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_adeg, by = "PARAMCD", by.y = "PARAM", all = TRUE)
}

gen_adeg_aval <- function(n, .df, adesc = avaldesc_sel, ...) {
  tibble(AVAL = rnorm(nrow(.df), mean = 350, sd = 100)/365.25,
         AVALU = "YEARS")
}

gen_adeg_base <- function(n, .df, ...) {
  tibble(BASE = ifelse(.df$AVISITN >= 0,
                       retain(.df, .df$AVAL, .df$ABLFL == "Y"),
                       .df$AVAL),
         BASETYPE = "LAST"
  )
}

gen_adeg_chg <- function(n, .df, ...) {
  tibble(CHG = ifelse(.df$AVISITN > 0, .df$AVAL - .df$BASE, NA),
         PCHG = ifelse(.df$AVISITN > 0, 100 * (.df$CHG / .df$BASE), NA)
  )
}

gen_adeg_descs <- function(n, .df, adesc = avaldescr_sel, ...) {
  n <- NROW(.df)
  tibble(AVALC = ifelse(.df$PARAMCD == "ECGINTP",
                        as.character(sample_fct(adesc, n, prob = c(0.25, 0.75))),
                        as.character(.df$AVAL)),
         BASEC = ifelse(.df$PARAMCD == "ECGINTP",
                        .df$AVALC[.df$AVISIT == "BASELINE"],
                        as.character(.df$BASE))
  )
}

gen_adeg_anl <- function(n, .df, ...) {
  tibble(ANL01FL = factor(ifelse(
          (.df$ABLFL == "Y" |  (is.na(.df$DTYPE) & .df$WORS01FL == "Y"))
          & (.df$AVISIT != "SCREENING"),
          "Y",
          "")),
         ANL03FL = case_when(
          .df$DTYPE == "MINIMUM" ~ "Y",
          .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
          TRUE ~ ""),
         ANL04FL = case_when(
          .df$DTYPE == "MAXIMUM" ~ "Y",
          .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
          TRUE ~ "")
  )
}


#' Recipes for creating ADEG CDISC Data
#'
#' @rdname adeg_recipes
#' @export
#'

adeg_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables, ~dependencies, ~func, ~func_args,
  "ADSL", "USUBJID", "PARAMCD", c("PARAM", "PARAMU", "mean_aval", "sd_aval", "ANRLO", "ANRHI"), no_deps, join_paramcd_adeg, NULL)

adeg_table_recipe <- tribble(
  ~variables,                  ~dependencies,                                         ~func,                 ~func_args,
  c("AVAL", "AVALU"),          no_deps,                                               gen_adeg_aval,         NULL,
  c("BASE", "BASETYPE"),       c("AVAL", "AVISITN", "ABLFL"),                         gen_adeg_base,         NULL
)


# create the data
ADSL <- gen_table_data(N = 10, recipe = adsl_recipe)
gen_reljoin_table(adeg_rel_join_recipe, adeg_table_recipe, db = list(ADSL = ADSL))

