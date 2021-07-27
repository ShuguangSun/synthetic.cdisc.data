## TODO add PARAM, PARAMCD functions

join_paramcd_adaette <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_tte, by = "ARMCD", by.y = "ARM", all = TRUE)
}

adaette_vars <- c(
  "ASEQ",     "TTESEQ",   "PARAM",    "PARAMCD",  "AVAL",     "AVALU",    "ADTM",     "ADY",      "CNSR",
  "EVNTDESC", "CNSDTDSC"
)


adaette_lookup <- tribble(
    ~ARM,     ~CATCD,   ~CAT,                         ~LAMBDA,    ~CNSR_P,
    "ARM A",  "1",      "any adverse event",          1 / 80,     0.4,
    "ARM B",  "1",      "any adverse event",          1 / 100,    0.2,
    "ARM C",  "1",      "any adverse event",          1 / 60,     0.42,
    "ARM A",  "2",      "any serious adverse event",  1 / 100,    0.3,
    "ARM B",  "2",      "any serious adverse event",  1 / 150,    0.1,
    "ARM C",  "2",      "any serious adverse event",  1 / 80,     0.32,
    "ARM A",  "3",      "a grade 3-5 adverse event",  1 / 80,     0.2,
    "ARM B",  "3",      "a grade 3-5 adverse event",  1 / 100,    0.08,
    "ARM C",  "3",      "a grade 3-5 adverse event",  1 / 60,     0.23
)

evntdescr_sel <- c(
    "Preferred Term",
    "Clinical Cut Off",
    "Completion or Discontinuation",
    "End of AE Reporting Period")

cnsdtdscr_sel <- c(
    "Preferred Term",
    "Clinical Cut Off",
    "Completion or Discontinuation",
    "End of AE Reporting Period")


secs_per_year <- 31556952


#' Generate PARAM and PARAMCD
#'
#' @param n not used
#' @param .df data frame with required variables `CATCD` and `CAT`
#'
#' @examples
#' gen_adaette_param(NULL, adaette_lookup)
#'
# gen_adaette_param <- function(n, .df, ...) {
#   tibble(
#     PARAMCD = "AEREPTTE",
#     PARAM = "Time to end of AE reporting period"
#     ) %>%
#     tibble(PARAMCD = c(paste0("AETOT", .df$CATCD), paste0("AETTE", .df$CATCD)),
#            PARAM = c(paste("Time to first occurrence of", .df$CAT),
#                         paste("Number of occurrences of", .df$CAT))
#            )
# }


#' Generate Analysis Start and End Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' dtms <- data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                    TRTEDTM = "2021-09-26 09:43:22 EST")
#' gen_adaette_dtms(NULL, dtms)
#'
gen_adaette_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year) {
  stopifnot(all(c("TRTSDTM", "TRTEDTM") %in% names(.df)))
  sds <- study_duration_secs
  tstart <- .df$TRTSDTM
  tend <- .df$TRTEDTM
  .df$ASTDTM = rand_posixct(start = tstart, end = tend, max_duration_secs = sds)
  .df$AENDTM = rand_posixct(start = .df$ASTDTM, end = tend, max_duration_secs = sds)
  .df$ASTDY = ceiling(as.numeric(difftime(.df$ASTDTM, tstart, units = "days")))
  .df$AENDY = ceiling(as.numeric(difftime(.df$ASTDTM, tend, units = "days")))
  .df
}


#' Generate Analysis Values and Units
#'
#' @param n not used
#' @param .df data frame with required variable `LAMBDA`
#'
#' @examples
#' gen_adaette_aval(NULL, adaette_lookup)
#'
gen_adaette_aval <- function(n, .df, ...) {
  tibble(AVAL = rexp(NROW(.df), .df$LAMBDA) / 365.25,
         AVALU = "YEARS")
}


#' Generate Censor Indicators
#'
#' @param n not used
#' @param .df data frame with required variable `CNSR_P`
#'
#' @examples
#' gen_adaette_cnsr(NULL, adaette_lookup)
#'
gen_adaette_cnsr <- function(n, .df, ...) {
  tibble(CNSR = as.numeric(runif(nrow(.df)) > .df$CNSR_P))
}


#' Generate Descriptions of Events and Censor Dates
#'
#' @param n not used
#' @param .df data frame with required variable `CNSR`
#'
#' @examples
#' gen_adaette_desc(NULL, gen_adaette_cnsr(NULL, adaette_lookup))
#'
gen_adaette_desc <- function(n, .df, ...) {
  n <- nrow(.df)
  tibble(EVNTDESC = ifelse(.df$CNSR, "", sample(evntdescr_sel, n, replace = TRUE)),
         CNSDTDSC = ifelse(.df$CNSR, sample(cnsdtdscr_sel, n, replace = TRUE), ""))
}

tteseqvars <- c("ASEQ", "TTESEQ")

gen_adaette_seq <- function(n, .df) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               TTESEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, tteseqvars]
}

dtmvars <- c("ADTM", "ADY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")

adaette_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables, ~dependencies, ~func, ~func_args,
  "ADSL", "USUBJID", "ARMCD", c("CATCD", "CAT", "LAMBDA", "CNSR_P"), no_deps, join_paramcd_adaette, NULL)


#' @rdname adtte_recipes
#'
#' @export
#'
#' @examples
#'
#' ADSL <- gen_table_data(N = 10, recipe = adsl_recipe)
#' gen_reljoin_table(adaette_rel_join_recipe, adaette_recipe, db = list(ADSL = ADSL))
#'
adaette_recipe <- tribble(~variables,                 ~dependencies,     ~func,              ~func_args,                                    ~keep,
                          c("AVAL", "AVALU"),         "LAMBDA",          gen_adaette_aval,   NULL,                                          TRUE,
                          # c("PARAM", "PARAMCD"),      c("CATCD", "CAT"), gen_adaette_param,  NULL,                                          TRUE,
                          "CNSR",                     "CNSR_P",          gen_adaette_cnsr,   NULL,                                          TRUE,
                          c("EVNTDESC", "CNSTDESC"),  "CNSR",            gen_adaette_desc,   NULL,                                          TRUE,
                          dtmvars,                    dtmdeps,           gen_adaette_dtms,   list(study_duration_secs = 1 * secs_per_year), TRUE,
                          tteseqvars,                 "USUBJID",         gen_adaette_seq,    NULL,                                          TRUE
)


