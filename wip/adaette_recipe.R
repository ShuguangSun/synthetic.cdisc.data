join_paramcd_aette <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_aette, by = "ARMCD", by.y = "ARM", all = TRUE)
}

lookup_aette <- tribble(
  ~ARM,    ~PARAMCD,   ~PARAM,                                                    ~LAMBDA,  ~CNSR_P,
  "ARM A", "AETTE1",   "Time to first occurrence of any adverse event",           1 / 80,   0.4,
  "ARM B", "AETTE1",   "Time to first occurrence of any adverse event",           1 / 100,  0.2,
  "ARM C", "AETTE1",   "Time to first occurrence of any adverse event",           1 / 60,   0.42,
  "ARM A", "AETTE2",   "Time to first occurrence of any serious adverse event",   1 / 100,  0.3,
  "ARM B", "AETTE2",   "Time to first occurrence of any serious adverse event",   1 / 150,  0.1,
  "ARM C", "AETTE2",   "Time to first occurrence of any serious adverse event",   1 / 80,   0.32,
  "ARM A", "AETTE3",   "Time to first occurrence of a grade 3-5 adverse event",   1 / 80,   0.2,
  "ARM B", "AETTE3",   "Time to first occurrence of a grade 3-5 adverse event",   1 / 100,  0.08,
  "ARM C", "AETTE3",   "Time to first occurrence of a grade 3-5 adverse event",   1 / 60,   0.23
)

#' Helper functions and constants for ADAETTE

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


#' Generate Parameter Descriptions and Short Descriptions
#'
#' @param n not used
#' @param .df data frame with required variables `PARAM` and `PARAMCD`
#'
#' @examples
#' gen_aette_param(NULL, lookup_aette)
#'
gen_aette_param <- function(n, .df, ...) {
  tibble(PARAM = as.factor(.df$PARAM),
         PARAMCD = as.factor(.df$PARAMCD))
}


#' Generate Analysis Start and End Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#' @param study_duration_secs duration of the study in seconds
#'
#' @examples
#' dtms <- data.frame(TRTSDTM = c("2018-04-01 14:03:04 EST","2018-04-01 14:03:04 EST"),
#'                    TRTEDTM = c("2021-09-26 09:43:22 EST", "2021-09-26 09:43:22 EST"))
#' gen_aette_dtms(NULL, dtms)
#'
gen_aette_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year) {
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
#' gen_aette_aval(NULL, lookup_aette)
#'
gen_aette_aval <- function(n, .df, ...) {
  tibble(AVAL = rexp(NROW(.df), .df$LAMBDA) / 365.25,
         AVALU = "YEARS")
}


#' Generate Censor Indicators
#'
#' @param n not used
#' @param .df data frame with required variable `CNSR_P`
#'
#' @examples
#' gen_aette_cnsr(NULL, lookup_aette)
#'
gen_aette_cnsr <- function(n, .df, ...) {
  tibble(CNSR = as.numeric(runif(nrow(.df)) > .df$CNSR_P))
}


#' Generate Descriptions of Events and Censor Dates
#'
#' @param n not used
#' @param .df data frame with required variable `CNSR`
#'
#' @examples
#' gen_aette_desc(NULL, gen_aette_cnsr(NULL, lookup_aette))
#'
gen_aette_desc <- function(n, .df, ...) {
  n <- nrow(.df)
  tibble(EVNTDESC = ifelse(.df$CNSR, "", sample(evntdescr_sel, n, replace = TRUE)),
         CNSDTDSC = ifelse(.df$CNSR, sample(cnsdtdscr_sel, n, replace = TRUE), ""))
}


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_aette_seq(NULL, x)
#'
#' gen_aette_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_aette_seq <- function(n, .df) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               AETTESEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, aetteseqvars]
}


aetteseqvars <- c("ASEQ", "AETTESEQ")
dtmvars <- c("ADTM", "ADY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")


#' Recipe for creating ADAETTE CDISC Data
#'
#' @rdname adaette_recipes
#' @export
#'
aette_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables,                            ~dependencies,  ~func,              ~func_args,
  "ADSL",       "USUBJID",    "ARMCD",       c("CATCD", "CAT", "LAMBDA", "CNSR_P"), no_deps,        join_paramcd_aette, NULL)

#' @rdname adaette_recipes
#'
#' @export
#'
#' @examples
#'
#' ADSL <- gen_table_data(N = 10, recipe = adsl_recipe)
#' gen_reljoin_table(aette_rel_join_recipe, aette_recipe, db = list(ADSL = ADSL))
#'
aette_recipe <- tribble(
  ~variables,                 ~dependencies,            ~func,              ~func_args,                                     ~keep,
  c("AVAL", "AVALU"),         "LAMBDA",                 gen_aette_aval,     NULL,                                           TRUE,
  c("PARAM", "PARAMCD"),      c("PARAMCD", "PARAM"),    gen_aette_param,    NULL,                                           TRUE,
  "CNSR",                     "CNSR_P",                 gen_aette_cnsr,     NULL,                                           TRUE,
  c("EVNTDESC", "CNSTDESC"),  "CNSR",                   gen_aette_desc,     NULL,                                           TRUE,
  dtmvars,                    dtmdeps,                  gen_aette_dtms,     list(study_duration_secs = 1 * secs_per_year),  TRUE,
  tteseqvars,                 "USUBJID",                gen_aette_seq,      NULL,                                           TRUE)
