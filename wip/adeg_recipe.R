join_paramcd_eg <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_eg, by = "USUBJID", all = TRUE)
}

lookup_eg <- tribble(
  ~PARAMCD,     ~PARAM,                ~AVALU,       ~EGCAT,         ~mean_aval, ~sd_aval, ~ANRLO,   ~ANRHI,
  "QT",         "QT Duration",         "msec",       "INTERVAL",     350,        100,      200,      500,
  "PR",         "RR Duration",         "msec",       "INTERVAL",     1050,       300,      600,      1500,
  "HR",         "Heart Rate",          "beats/min",  "MEASUREMENT",  70,         20,       40,       100,
  "ECGINTP",    "ECG Interpretation",  "",           "FINDING",      NA_real_,   NA_real_, NA_real_, NA_real_
)

#' Helper functions and constants for ADEG

eg_varnames <- c("ASEQ", "EGSEQ", "EGTESTCD", "EGTEST", "EGCAT",   "ASPID",   "PARAM",    "PARAMCD",  "AVAL",    "AVALC",   "AVALU",
                   "BASE", "BASEC", "BASETYPE", "ABLFL",  "CHG",     "PCHG",    "DTYPE",    "ANRLO",    "ANRHI",   "ANRIND",  "BNRIND",
                   "ADTM", "ADY",   "ATPTN",    "AVISIT", "AVISITN", "ONTRTFL", "WORS01FL", "WORS02FL", "ANL01FL", "ANL03FL", "ANL04FL")

s_eg_paramcd <- c("QT", "RR", "HR", "ECGINTP")

s_eg_param <- c("QT" = "QT Duration",
                "RR" = "RR Duration",
                "HR" = "Heart Rate",
                "ECGINTP" = "ECG Interpretation")

s_eg_avalu <-  c("msec", "msec", "beats/min", "")

secs_per_year <- 31557600

avaldescr_sel <- c("ABNORMAL","NORMAL")


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_eg_seq(NULL, x)
#'
#' gen_eg_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_eg_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               EGSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, c("ASEQ", "EGSEQ")]
}


#' Generate Analysis Timepoints
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_eg_atptn(length(x), x)
#'
gen_eg_atptn <- function(n, .df, ...) {
  tibble(ATPTN = rep(1, n))
}


#' Generate Name & Description of Tests to Obtain EG Data
#'
#' @param n not used
#' @param .df data frame with required variables `PARAMCD` and `PARAM`
#'
#' @examples
#' x <- data.frame(PARAMCD = s_eg_paramcd,
#'                 PARAM = s_eg_param)
#' gen_eg_eg(NULL, x)
#'
gen_eg_eg <- function(n, .df, ...) {
  tibble(EGTESTCD = .df$PARAMCD,
         EGTEST = .df$PARAM
  )
}


#' Generate Derivation Types and Analysis Visits
#'
#' @param n not used
#' @param .df data frame with required variables `USUBJID`, `PARAMCD`, `BASETYPE`, `ADTM`, `ASPID`, `EGSEQ`, `AVISIT`, `ONTRTFL`, `TRTSDTM`, and `AVAL`
#' @param minimum
#' @param visit_format format of analysis visit
#' @param n_assessments number of analysis assessments
#' @param n_days number of days of analysis
#'
gen_eg_avisit_dtype <- function(n, .df, minimum = TRUE, visit_format = "WEEK", n_assessments = 5L, n_days = 5L, ...) {
  data <- tibble(AVISIT = visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days),
                 DTYPE = NA) %>%
    dplyr::group_by(.df$USUBJID, .df$PARAMCD, .df$BASETYPE) %>%
    dplyr::arrange(.df$ADTM, .df$ASPID, .df$EGSEQ) %>%
    dplyr::filter(
      (.df$AVISIT != "BASELINE" & .df$AVISIT != "SCREENING")
      & (.df$ONTRTFL == "Y" | .df$ADTM <= .df$TRTSDTM)
    ) %>%
    {
      if (minimum == TRUE) dplyr::filter(., .df$AVAL == min(.df$AVAL)) %>%
        dplyr::mutate(., DTYPE = "MINIMUM", AVISIT = "POST-BASELINE MINIMUM")
      else dplyr::filter(., .df$AVAL == max(.df$AVAL)) %>%
        dplyr::mutate(., DTYPE = "MAXIMUM", AVISIT = "POST-BASELINE MAXIMUM")
    } %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  return(data)
}


#' Generate Sponsor Identifiers
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 3))
#' gen_eg_aspid(NULL, x)
#'
gen_eg_aspid <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASPID = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o]
}


#' Generate Analysis Values
#'
#' @param n not used
#' @param .df data frame with required variables `mean_aval` and `sd_aval`
#'
#' @examples
#' gen_eg_aval(NULL, lookup_eg)
#'
gen_eg_aval <- function(n, .df, ...) {
  tibble(AVAL = ifelse(!is.na(.df$mean_aval),
                       rnorm(nrow(.df), mean = .df$mean_aval, sd = .df$sd_aval)/365.25,
                       NA_real_))
}


#' Generate Character Type Analysis Values
#'
#' @param n not used
#' @param .df data frame with required variables `PARAMCD` and `AVAL`
#'
#' @examples
#' aval <- gen_eg_aval(NULL, lookup_eg)
#' gen_eg_adesc(NULL, data.frame(aval, lookup_eg))
#'
gen_eg_adesc <- function(n, .df, adesc = avaldescr_sel, ...) {
  n <- nrow(.df)
  tibble(AVALC = ifelse(.df$PARAMCD == "ECGINTP",
                        as.character(sample_fct(adesc, n, prob = c(0.25, 0.75))),
                        as.character(.df$AVAL)))
}


#' Generate Analysis Reference Range Indicators
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `ANRLO`, and `ANRHI`
#'
#' @examples
#' aval <- gen_eg_aval(NULL, lookup_eg)
#' gen_eg_anrind(NULL, data.frame(aval, lookup_eg))
#'
gen_eg_anrind <- function(n, .df, ...) {
  tibble(ANRIND = factor(dplyr::case_when(
    .df$AVAL < .df$ANRLO ~ "LOW",
    .df$AVAL >= .df$ANRLO & .df$AVAL <= .df$ANRHI ~ "NORMAL",
    .df$AVAL > .df$ANRHI ~ "HIGH"),
    levels = c("LOW", "NORMAL", "HIGH"))
  )
}


#' Generate Baseline Values and Types
#'
#' @param n not used
#' @param .df data frame with required variables `AVISITN`, `AVAL`, and `ABLFL`
#'
#' @examples
#' x <- data.frame(
#'    AVISITN = sample(-1:5, 10, replace = TRUE),
#'    ABLFL = sample(c("Y", "N"), 10, replace = TRUE),
#'    AVAL = sample(0:1000, 10, replace = TRUE))
#' gen_eg_base(NULL, x)
#'
gen_eg_base <- function(n, .df, ...) {
  tibble(BASE = ifelse(.df$AVISITN >= 0,
                       retain(.df, .df$AVAL, .df$ABLFL == "Y"),
                       .df$AVAL),
         BASETYPE = "LAST")
}


#' Generate Character Type Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `PARAMCD`, `AVALC`, `AVISIT`, and `BASE`
#'
#' @examples
#' x <- data.frame(
#'    PARAMCD = sample(s_eg_paramcd, 5, replace = TRUE),
#'    AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22"),
#'    AVALC = c("ABNORMAL", "NORMAL", "NORMAL", "NORMAL", "NORMAL"),
#'    BASE = c(121, 583, NA, 583, 583))
#' gen_eg_basec(NULL, x)
#'
gen_eg_basec <- function(n, .df, ...) {
  tibble(BASEC = ifelse(.df$PARAMCD == "ECGINTP",
                        .df$AVALC[.df$AVISIT == "BASELINE"],
                        as.character(.df$BASE))
  )
}


#' Generate Baseline Reference Range Indicators
#'
#' @param n not used
#' @param .df data frame with required variables `ANRIND` and `ABLFL`
#'
#' @examples
#' aval <- gen_eg_aval(NULL, lookup_eg)
#' anrind <- gen_eg_anrind(NULL, data.frame(aval, lookup_eg))
#' ablfl <- gen_eg_ablfl(NULL, data.frame(AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")))
#' gen_eg_bnrind(NULL, data.frame(anrind, ablfl))
#'
gen_eg_bnrind <- function(n, .df, ...) {
  tibble(BNRIND = factor(.df$ANRIND[.df$ABLFL == "Y"],
                         levels = c("LOW", "NORMAL", "HIGH"))
  )
}


#' Generate Change from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVISITN`, `AVAL`, and `BASE`
#'
#' @examples
#' x <- data.frame(
#'    AVISITN = sample(-1:5, 5, replace = TRUE),
#'    AVAL = sample(0:1000, 5, replace = TRUE),
#'    BASE = c(121, 583, NA, 583, 583))
#' gen_eg_chg(NULL, x)
#'
gen_eg_chg <- function(n, .df, ...) {
  tibble(CHG = ifelse(.df$AVISITN > 0, .df$AVAL - .df$BASE, NA))
}


#' Generate Percent Change from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVISITN`, `CHG`, and `BASE`
#'
#' @examples
#' x <- data.frame(
#'    AVISITN = sample(-1:5, 5, replace = TRUE),
#'    AVAL = sample(0:1000, 5, replace = TRUE),
#'    BASE = c(121, 583, NA, 583, 583))
#' gen_eg_pchg(NULL, cbind(x, gen_eg_chg(NULL, x)))
#'
gen_eg_pchg <- function(n, .df, ...) {
  tibble(PCHG = ifelse(.df$AVISITN > 0,
                       100 * (.df$CHG / .df$BASE),
                       NA))
}


#' Generate Analysis Baseline Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVISIT`
#' @param visit_format
#'
#' @examples
#' gen_eg_ablfl(NULL, data.frame(AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")))
#'
gen_eg_ablfl <- function(n, .df, visit_format = "WEEK", ...) {
  tibble(ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
                        "Y",
                        ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1",
                               "Y",
                               "")))
}


#' Generate Analysis Flags 01 Baseline Post-Baseline
#'
#' @param n not used
#' @param .df data frame with required variables `ABLFL`, `DTYPE`, `WORS01FL`, and `AVISIT`
#'
#' @examples
#'
gen_eg_anl01fl <- function(n, .df, ...) {
  tibble(ANL01FL = factor(ifelse(
    (.df$ABLFL == "Y" |  (is.na(.df$DTYPE) & .df$WORS01FL == "Y"))
    & (.df$AVISIT != "SCREENING"),
    "Y",
    ""))
  )
}


#' Generate Analysis Flags 03 Min Obs Within BType
#'
#' @param n not used
#' @param .df data frame with required variables `ABLFL`, `DTYPE`, `PARAMCD`
#'
#' @examples
#' gen_eg_anl03fl(NULL,
#'                  data.frame(ABLFL = c("", "", "Y", "", "Y"),
#'                             DTYPE = c(NA, NA, "MINIMUM", NA, "MAXIMUM"),
#'                             PARAMCD = sample(s_eg_paramcd, 5, replace = TRUE)))
#'
gen_eg_anl03fl <- function(n, .df, ...) {
  tibble(ANL03FL = dplyr::case_when(
    .df$DTYPE == "MINIMUM" ~ "Y",
    .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
    TRUE ~ "")
  )
}


#' Generate Analysis Flags 04 Max Obs Within BType
#'
#' @param n not used
#' @param .df data frame with required variables `ABLFL`, `DTYPE`, `PARAMCD`
#'
#' @examples
#' gen_eg_anl04fl(NULL,
#'                  data.frame(ABLFL = c("", "", "Y", "", "Y"),
#'                             DTYPE = c(NA, NA, "MINIMUM", NA, "MAXIMUM"),
#'                             PARAMCD = sample(s_eg_paramcd, 5, replace = TRUE)))
#'
gen_eg_anl04fl <- function(n, .df, ...) {
  tibble(ANL04FL = dplyr::case_when(
    .df$DTYPE == "MAXIMUM" ~ "Y",
    .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
    TRUE ~ "")
  )
}


#' Generate On Treatment Record Flags
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM`, `TRTEDTM`, `ADTM`
#'
#' @examples
#' gen_eg_ontrtfl(NULL,
#'                  data.frame(
#'                  TRTSDTM = c("2019-03-06 14:03:04 EST", "2018-04-01 14:03:04 EST"),
#'                  TRTEDTM = c("2021-03-06 01:41:28 EST", "2021-09-26 09:43:22 EST"),
#'                  ADTM = c("2021-09-09 20:00:00 EDT", NA)))
#'
gen_eg_ontrtfl <- function(n, .df, ...) {
  tibble(ONTRTFL = factor(dplyr::case_when(
    is.na(.df$TRTSDTM) ~ "",
    is.na(.df$ADTM) ~ "Y",
    (.df$ADTM < .df$TRTSDTM) ~ "",
    (.df$ADTM > .df$TRTEDTM) ~ "",
    TRUE ~ "Y"))
  )
}


#' Generate Analysis Visit Numbers
#'
#' @param n not used
#' @param .df data frame with required variables `AVISIT`
#'
#' @examples
#' gen_eg_avisitn(NULL, data.frame(AVISIT = as.factor(c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"))))
#'
gen_eg_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = dplyr::case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_)
  )
}


#' Generate Worst Observation in Window Flags 01
#'
#' @param n not used
#' @param .df data frame with required variables `USUBJID`, `PARAMCD`, `BASETYPE`, `AVISIT`, `ADTM`, `ASPID`, `EGSEQ`, `AVISITN`, `ONTRTFL`, `TRTSDTM`, `DTYPE`, `AVALC`, and `AVAL`
#'
gen_eg_wors01fl <- function(n, .df, ...) {
  tibble(WORS01FL = flag_variables(.df, FALSE)$new_var)
}


#' Generate Worst Post-Baseline Observation Flags
#'
#' @param n not used
#' @param .df data frame with required variables `USUBJID`, `PARAMCD`, `BASETYPE`, `AVISIT`, `ADTM`, `ASPID`, `EGSEQ`, `AVISITN`, `ONTRTFL`, `TRTSDTM`, `DTYPE`, `AVALC`, and `AVAL`
#'
gen_eg_wors02fl <- function(n, .df, ...) {
  tibble(WORS02FL = flag_variables(.df, TRUE)$new_var)
}


#' Generate Analysis Datetime
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#' @param study_duration duration of the study in seconds
#'
#' @examples
#' gen_eg_adtm(NULL,
#'               data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                          TRTEDTM = "2021-09-26 09:43:22 EST"))
#'
gen_eg_adtm <- function(n, .df, study_duration = 2, ...) {
  study_duration_secs <- secs_per_year * study_duration
  trtsdt_int <- as.numeric(as.Date(.df$TRTSDTM))
  trtedt_int <- dplyr::case_when(!is.na(.df$TRTEDTM) ~ as.numeric(as.Date(.df$TRTEDTM)),
                          is.na(.df$TRTEDTM) ~ floor(trtsdt_int + (study_duration_secs) / 86400))
  tibble(ADTM = as.POSIXct((sample(trtsdt_int:trtedt_int, size = 1) * 86400), origin = "1970-01-01"))
}


#' Generate Analysis Relative Day
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' gen_eg_ady(NULL,
#'              data.frame(ADTM = "2021-11-20 19:00:00",
#'                         TRTSDTM = "2018-04-01 14:03:04 EST"))
#'
gen_eg_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}

wors_depvars <- c("USUBJID", "PARAMCD", "BASETYPE", "AVISIT", "ADTM", "ASPID", "EGSEQ", "AVISITN", "ONTRTFL", "TRTSDTM", "DTYPE", "AVALC", "AVAL")
dtype_depvars <- c("USUBJID", "PARAMCD", "BASETYPE", "ADTM", "ASPID", "EGSEQ", "AVISIT", "ONTRTFL", "TRTSDTM", "AVAL")
basec_depvars <- c("PARAMCD", "AVALC", "AVISIT", "BASE")

#' Recipe for creating ADEG CDISC Data
#'
#' @rdname eg_recipes
#' @export
#'
eg_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables,                                                             ~dependencies, ~func,           ~func_args,
  "ADSL",       "USUBJID",    "PARAMCD",     c("PARAM", "AVALU", "EGCAT", "mean_aval", "sd_aval", "ANRLO", "ANRHI"), no_deps,       join_paramcd_eg, NULL)

eg_recipe <- tribble(
  ~variables,                          ~dependencies,                                         ~func,               ~func_args,
  c("ASEQ", "EGSEQ"),                  "USUBJID",                                             gen_eg_seq,          NULL,
  "ATPTN",                             no_deps,                                               gen_eg_atptn,        NULL,
  c("EGTESTCD", "EGTEST"),             c("PARAMCD", "PARAM"),                                 gen_eg_eg,           NULL,
  c("DTYPE", "AVISIT"),                dtype_depvars,                                         gen_eg_avisit_dtype, list(minimum = TRUE, visit_format = "WEEK", n_assessments = 5L, n_days = 5L),
  "ASPID",                             "USUBJID",                                             gen_eg_aspid,        NULL,
  "AVAL",                              c("mean_aval", "sd_aval"),                             gen_eg_aval,         NULL,
  "AVALC",                             c("PARAMCD", "AVAL"),                                  gen_eg_adesc,        NULL,
  "ANRIND",                            c("AVAL", "ANRLO", "ANRHI"),                           gen_eg_anrind,       NULL,
  c("BASE", "BASETYPE"),               c("AVAL", "AVISITN", "ABLFL"),                         gen_eg_base,         NULL,
  "BASEC",                             basec_depvars,                                         gen_eg_basec,        NULL,
  "BNRIND",                            c("ANRIND", "ABLFL"),                                  gen_eg_bnrind,       NULL,
  "CHG",                               c("AVISITN", "AVAL", "BASE"),                          gen_eg_chg,          NULL,
  "PCHG",                              c("AVISITN", "CHG", "BASE"),                           gen_eg_pchg,         NULL,
  "ABLFL",                             "AVISIT",                                              gen_eg_ablfl,        list(visit_format = "WEEK"),
  "ANL01FL",                           c("ABLFL", "DTYPE", "WORS01FL", "AVISIT"),             gen_eg_anl01fl,      NULL,
  "ANL03FL",                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_eg_anl03fl,      NULL,
  "ANL04FL",                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_eg_anl04fl,      NULL,
  "ONTRTFL",                           c("TRTSDTM", "TRTEDTM", "ADTM"),                       gen_eg_ontrtfl,      NULL,
  "AVISITN",                           "AVISIT",                                              gen_eg_avisitn,      NULL,
  "WORS01FL",                          wors_depvars,                                          gen_eg_wors01fl,     NULL,
  "WORS02FL",                          wors_depvars,                                          gen_eg_wors02fl,     NULL,
  "ADTM",                              c("TRTSDTM", "TRTEDTM"),                               gen_eg_adtm,         list(study_duration = 2),
  "ADY",                               c("ADTM", "TRTSDTM"),                                  gen_eg_ady,          NULL
)

# ADSL <- gen_table_data(N = 10, recipe = adsl_recipe)
# ADEG <- gen_reljoin_table(eg_rel_join_recipe, eg_table_recipe, db = list(ADSL = ADSL))
