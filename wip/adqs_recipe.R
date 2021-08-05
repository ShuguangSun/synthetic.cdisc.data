adqs_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

adqs_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                      "ADSL",       "USUBJID",    adqs_scaff, NULL)

qs_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables,             ~dependencies, ~func,      ~func_args,
  "ADSL",       "USUBJID",    no_deps,       c("QSDECOD", "ATIREL"), no_deps,       adqs_scaff, NULL
)

lookup_qs <- tribble(
  ~PARAM,                                   ~PARAMCD,
  "BFI All Questions",                      "BFIALL",
  "Fatigue Interference",                   "FATIGI",
  "Function/Well-Being (GF1,GF3,GF7)",      "FKSI-FWB",
  "Treatment Side Effects (GP2,C5,GP5)",    "FKSI-TSE",
  "FKSI-19 All Questions",                  "FKSIALL"
)

#' Helper functions and constants for ADQS

s_qs_param = c(
  "BFI All Questions",
  "Fatigue Interference",
  "Function/Well-Being (GF1,GF3,GF7)",
  "Treatment Side Effects (GP2,C5,GP5)",
  "FKSI-19 All Questions")

s_qs_paramcd = c("BFIALL", "FATIGI", "FKSI-FWB", "FKSI-TSE", "FKSIALL")

secs_per_year <- 31556952


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_qs_seq(NULL, x)
#'
#' gen_qs_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_qs_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               QSSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, qsseqvars]
}


#' Generate Analysis Values
#'
#' @param n not used
#' @param .df data frame with required variable `AVISITN`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_qs_aval(NULL, gen_qs_avisitn(NULL, gen_qs_avisit(nrow(x), NULL)))
#'
gen_qs_aval <- function(n, .df, ...) {
  tibble(AVAL = rnorm(nrow(.df), mean = 50, sd = 8) + .df$AVISITN * rnorm(nrow(.df), mean = 5, sd = 2))
}


#' Generate Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `ABLFL`, and `ABLFL2`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' data <- cbind(gen_qs_aval(NULL, gen_qs_avisitn(NULL, gen_qs_avisit(nrow(x), NULL))),
#'               gen_qs_ablfl(NULL, gen_qs_avisit(nrow(x), NULL)))
#' gen_qs_base(NULL, data)
#'
gen_qs_base <- function(n, .df, ...) {
  tibble(BASE2 = retain(.df, .df$AVAL, .df$ABLFL2 == "Y"),
         BASE = ifelse(.df$ABLFL2 != "Y", retain(.df, .df$AVAL, .df$ABLFL == "Y"), NA))
}


#' Generate Analysis Baseline Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVISIT`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_qs_ablfl(NULL, gen_qs_avisit(nrow(x), NULL))
#'
gen_qs_ablfl <- function(n, .df, visit_format = "WEEK", ...) {
  tibble(ABLFL2 = ifelse(.df$AVISIT == "SCREENING", "Y", ""),
         ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
                        "Y",
                        ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1", "Y", "")))
}


#' Generate Limit of Quantification Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_qs_loqfl(NULL, gen_qs_aval(NULL, gen_qs_avisitn(NULL, gen_qs_avisit(nrow(x), NULL))))
#'
gen_qs_loqfl <- function(n, .df, ...) {
  tibble(LOQFL = ifelse(.df$AVAL < 32, "Y", "N"))
}


#' Generate Change from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `BASE2`, and `BASE`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' AVAL <- gen_qs_aval(NULL, gen_qs_avisitn(NULL, gen_qs_avisit(nrow(x), NULL)))
#' ABLFL <- gen_qs_ablfl(NULL, gen_qs_avisit(nrow(x), NULL))
#' BASE <- gen_qs_base(NULL, cbind(AVAL, ABLFL))
#' gen_qs_chg(NULL, cbind(AVAL, BASE))
#'
gen_qs_chg <- function(n, .df, ...) {
  tibble(CHG2 = .df$AVAL - .df$BASE2,
         CHG = .df$AVAL - .df$BASE)
}


#' Generate Percent Change from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `CHG2`, `CHG`, `BASE2`, and `BASE`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' AVAL <- gen_qs_aval(NULL, gen_qs_avisitn(NULL, gen_qs_avisit(nrow(x), NULL)))
#' ABLFL <- gen_qs_ablfl(NULL, gen_qs_avisit(nrow(x), NULL))
#' BASE <- gen_qs_base(NULL, cbind(AVAL, ABLFL))
#' CHG <- gen_qs_chg(NULL, cbind(AVAL, BASE))
#' gen_qs_pchg(NULL, cbind(BASE, CHG))
#'
gen_qs_pchg <- function(n, .df, ...) {
  tibble(PCHG2 = 100 * (.df$CHG2 / .df$BASE2),
         PCHG = 100 * (.df$CHG / .df$BASE))
}


#' Generate Analysis Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#' @param study_duration duration of the study in years
#'
#' @examples
#' gen_qs_adtm(NULL, data.frame(TRTSDTM = c("2018-04-01 14:03:04 EST","2018-04-01 14:03:04 EST"),
#'                              TRTEDTM = c("2021-09-26 09:43:22 EST", "2021-09-26 09:43:22 EST")))
#'
gen_qs_adtm <- function(n, .df, study_duration = 2, ...) {
  study_duration_secs <- secs_per_year * study_duration
  trtsdt_int <- as.numeric(as.Date(.df$TRTSDTM))
  trtedt_int <- dplyr::case_when(!is.na(.df$TRTEDTM) ~ as.numeric(as.Date(.df$TRTEDTM)),
                                  is.na(.df$TRTEDTM) ~ floor(trtsdt_int + (study_duration_secs) / 86400))
  tibble(ADTM = as.POSIXct(mapply(trtsdt_int, trtedt_int, FUN = function(s, e) sample(s:e, size = 1) * 86400), origin = "1970-01-01"))
}


#' Generate Analysis Relative Day
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' gen_qs_ady(NULL, data.frame(ADTM = c("2018-05-01 14:03:04 EST", "2021-11-20 19:00:00"),
#'                             TRTSDTM = c("2018-04-01 14:03:04 EST", "2018-04-01 14:03:04 EST")))
#'
gen_qs_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}


#' Generate Analysis Visits
#'
#' @param n number of rows
#' @param .df not used
#' @param visit_format
#' @param n_assessments number of assessments taken per subject
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_qs_avisit(nrow(x), NULL)
#'
gen_qs_avisit <- function(n, .df, visit_format = "WEEK", n_assessments = 5L, n_days = 5L, ...) {
  tibble(AVISIT = rep(visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days),
                      ceiling(n/(ifelse(visit_format == "WEEK", 2, 1)+n_assessments)))[1:n])
}


#' Generate Analysis Visit Numbers
#'
#' @param n not used
#' @param .df data frame with required variable `AVISIT`
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_qs_avisitn(NULL, gen_qs_avisit(nrow(x), NULL))
#'
gen_qs_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = dplyr::case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_))
}


qsseqvars <- c("ASEQ", "QSSEQ")

qs_recipe <- tribble(
  ~variables,             ~dependencies,                      ~func,          ~func_args,                                                   ~keep,
  qsseqvars,              "USUBJID",                          gen_qs_seq,     NULL,                                                         TRUE,
  "AVAL",                 "AVISITN",                          gen_qs_aval,    NULL,                                                         TRUE,
  c("BASE2", "BASE"),     c("AVAL", "ABLFL2", "ABLFL"),       gen_qs_base,    NULL,                                                         TRUE,
  c("AVISIT2", "ABLFL"),  "AVISIT",                           gen_qs_ablfl,   NULL,                                                         TRUE,
  "LOQFL",                "AVAL",                             gen_qs_loqfl,   NULL,                                                         TRUE,
  c("CHG2", "CHG"),       c("AVAL", "BASE2", "BASE"),         gen_qs_chg,     NULL,                                                         TRUE,
  c("PCHG2", "PCHG"),     c("BASE2", "BASE", "CHG2", "CHG"),  gen_qs_pchg,    NULL,                                                         TRUE,
  "ADTM",                 c("TRTSDTM", "TRTEDTM"),            gen_qs_adtm,    list(study_duration = 2),                                     TRUE,
  "ADY",                  c("TRTSDTM", "TRTEDTM"),            gen_qs_ady,     NULL,                                                         TRUE,
  "AVISIT",               no_deps,                            gen_qs_avisit,  list(visit_format = "WEEK", n_assessments = 5L, n_days = 5L), TRUE,
  "AVISITN",              "AVISIT",                           gen_qs_avisitn, NULL,                                                         TRUE)
