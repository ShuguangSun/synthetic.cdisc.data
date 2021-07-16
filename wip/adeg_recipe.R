
lookup_adeg <- tribble(
  ~PARAMCD,     ~PARAM,                ~AVALU,       ~EGCAT,         ~mean_aval, ~sd_aval, ~ANRLO,   ~ANRHI,
  "QT",         "QT Duration",         "msec",       "INTERVAL",     350,        100,      200,      500,
  "PR",         "RR Duration",         "msec",       "INTERVAL",     1050,       300,      600,      1500,
  "HR",         "Heart Rate",          "beats/min",  "MEASUREMENT",  70,         20,       40,       100,
  "ECGINTP",    "ECG Interpretation",  "",           "FINDING",      NA_real_,   NA_real_, NA_real_, NA_real_
)

#' Helper functions and constants for ADEG

adeg_varnames <- c("ASEQ", "EGSEQ", "EGTESTCD", "EGTEST", "EGCAT",   "ASPID",   "PARAM",    "PARAMCD",  "AVAL",    "AVALC",   "AVALU",
                   "BASE", "BASEC", "BASETYPE", "ABLFL",  "CHG",     "PCHG",    "DTYPE",    "ANRLO",    "ANRHI",   "ANRIND",  "BNRIND",
                   "ADTM", "ADY",   "ATPTN",    "AVISIT", "AVISITN", "ONTRTFL", "WORS01FL", "WORS02FL", "ANL01FL", "ANL03FL", "ANL04FL")

s_adeg_paramcd <- c("QT", "RR", "HR", "ECGINTP")

s_adeg_param <- c("QT" = "QT Duration",
                  "RR" = "RR Duration",
                  "HR" = "Heart Rate",
                  "ECGINTP" = "ECG Interpretation")

s_adeg_avalu <-  c("msec", "msec", "beats/min", "")

secs_per_year <- 31557600

avaldescr_sel <- c("ABNORMAL","NORMAL")


join_paramcd_adeg <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_adeg, by = "USUBJID", all = TRUE)
}


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_adeg_seq(NULL, x)
#'
#' gen_adeg_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_adeg_seq <- function(n, .df, ...) {
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
#' @param n
#' @param .df data frame
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_adeg_atptn(NULL, x)
#'
gen_adeg_atptn <- function(n, .df, ...) {
  tibble(ATPTN = 1)
}


#' Generate Name & Description of Tests to Obtain EG Data
#'
#' @param n
#' @param .df data frame with required variables `PARAMCD` and `PARAM`
#'
#' @examples
#' x <- data.frame(PARAMCD = s_adeg_paramcd,
#'                 PARAM = s_adeg_param)
#' gen_adeg_eg(NULL, x)
#'
gen_adeg_eg <- function(n, .df, ...) {
  tibble(EGTESTCD = .df$PARAMCD,
         EGTEST = .df$PARAM
  )
}


#' Helper Function to Generate `AVISIT`
#'
#' @param visit_format
#' @param n_assessments
#' @param n_days
#'
visit_schedule <- function(visit_format = "WEEK",
                           n_assessments = 10L,
                           n_days = 5L) {
    # trap invalid assessment format
  if (!(toupper(visit_format) %in% c("WEEK", "CYCLE"))) {
    message("Visit format value must either be: WEEK or CYCLE")
    return(NA)
  }

  if (toupper(visit_format) == "WEEK") {
    # numeric vector of n assessments/cycles/days
    assessments <- 1:n_assessments
    # numeric vector for ordering including screening (-1) and baseline (0) place holders
    assessments_ord <- -1:n_assessments
    # character vector of nominal visit values
    visit_values <- c("SCREENING", "BASELINE", paste(toupper(visit_format), assessments, "DAY", (assessments * 7) + 1))
  } else if (toupper(visit_format) == "CYCLE") {
    cycles <- sort(rep(1:n_assessments, times = 1, each = n_days))
    days <- rep(seq(1:n_days), times = n_assessments, each = 1)
    assessments_ord <- 0:(n_assessments * n_days)
    visit_values <- c("SCREENING", paste(toupper(visit_format), cycles, "DAY", days))
  }

  # create and order factor variable to return from function
  visit_values <- stats::reorder(factor(visit_values), assessments_ord)
}


dtype_depvars <- c("USUBJID", "PARAMCD", "BASETYPE", "ADTM", "ASPID", "EGSEQ", "AVISIT", "ONTRTFL", "TRTSDTM", "AVAL")

#' Generate Derivation Types and Analysis Visits
#'
#' @param n
#' @param .df data frame with required variables `USUBJID`, `PARAMCD`, `BASETYPE`, `ADTM`, `ASPID`, `EGSEQ`, `AVISIT`, `ONTRTFL`, `TRTSDTM`, and `AVAL`
#' @param minimum
#' @param visit_format format of analysis visit
#' @param n_assessments number of analysis assessments
#' @param n_days number of days of analysis
#'
gen_adeg_avisit_dtype <- function(n, minimum = TRUE, .df, visit_format = "WEEK", n_assessments = 5L, n_days = 5L, ...) {
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
#' @param n
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 3))
#' gen_adeg_aspid(NULL, x)
#'
gen_adeg_aspid <- function(n, .df, ...) {
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
#' @param n
#' @param .df data frame with required variables `mean_aval` and `sd_aval`
#'
#' @examples
#' gen_adeg_aval(NULL, lookup_adeg)
#'
gen_adeg_aval <- function(n, .df, ...) {
  tibble(AVAL = ifelse(!is.na(.df$mean_aval),
                       rnorm(nrow(.df), mean = .df$mean_aval, sd = .df$sd_aval)/365.25,
                       NA_real_)
  )
}


#' Generate Character Type Analysis Values
#'
#' @param n
#' @param .df data frame with required variables `PARAMCD` and `AVAL`
#'
#' @examples
#' aval <- gen_adeg_aval(NULL, lookup_adeg)
#' gen_adeg_adesc(NULL, data.frame(aval, lookup_adeg))
#'
gen_adeg_adesc <- function(n, .df, adesc = avaldescr_sel, ...) {
  n <- nrow(.df)
  tibble(AVALC = ifelse(.df$PARAMCD == "ECGINTP",
                        as.character(sample_fct(adesc, n, prob = c(0.25, 0.75))),
                        as.character(.df$AVAL))
  )
}


#' Generate Analysis Reference Range Indicators
#'
#' @param n
#' @param .df data frame with required variables `AVAL`, `ANRLO`, and `ANRHI`
#'
#' @examples
#' aval <- gen_adeg_aval(NULL, lookup_adeg)
#' gen_adeg_anrind(NULL, data.frame(aval, lookup_adeg))
#'
gen_adeg_anrind <- function(n, .df, ...) {
  tibble(ANRIND = factor(dplyr::case_when(
    .df$AVAL < .df$ANRLO ~ "LOW",
    .df$AVAL >= .df$ANRLO & .df$AVAL <= .df$ANRHI ~ "NORMAL",
    .df$AVAL > .df$ANRHI ~ "HIGH"),
    levels = c("LOW", "NORMAL", "HIGH"))
  )
}


#' Generate Baseline Values and Types
#'
#' @param n
#' @param .df data frame with required variables `AVISITN`, `AVAL`, and `ABLFL`
#'
#' @examples
#' x <- data.frame(
#'  AVISITN = sample(-1:5, 10, replace = TRUE),
#'  ABLFL = sample(c("Y", "N"), 10, replace = TRUE),
#'  AVAL = sample(0:1000, 10, replace = TRUE))
#' gen_adeg_base(NULL, x)
#'
# gen_adeg_base <- function(n, .df, ...) {
#   tibble(BASE = ifelse(.df$AVISITN >= 0,
#                        retain(.df, .df$AVAL, .df$ABLFL == "Y"),
#                        .df$AVAL),
#          BASETYPE = "LAST"
#   )
# }


basec_depvars <- c("PARAMCD", "AVALC", "AVISIT", "BASE")

#' Generate Character Type Baseline Values
#'
#' @param n
#' @param .df data frame with required variables `PARAMCD`, `AVALC`, `AVISIT`, and `BASE`
#'
# gen_adeg_bdesc <- function(n, .df, ...) {
#   n <- nrow(.df)
#   tibble(BASEC = ifelse(.df$PARAMCD == "ECGINTP",
#                         .df$AVALC[.df$AVISIT == "BASELINE"],
#                         as.character(.df$BASE))
#   )
# }


#' Generate Baseline Reference Range Indicators
#'
#' @param n
#' @param .df data frame with required variables `ANRIND` and `ABLFL`
#'
#' aval <- gen_adeg_aval(NULL, lookup_adeg)
#' anrind <- gen_adeg_anrind(NULL, data.frame(aval, lookup_adeg))
#' gen_adeg_bnrind(NULL, data.frame(anrind, ...))
#'
# gen_adeg_bnrind <- function(n, .df, ...) {
#   tibble(BNRIND = factor(.df$ANRIND[.df$ABLFL == "Y"],
#                          levels = c("LOW", "NORMAL", "HIGH"))
#   )
# }


#' Generate Change from Baseline Values
#'
#' @param n
#' @param .df data frame with required variables `AVISITN`, `AVAL`, and `BASE`
#'
# gen_adeg_chg <- function(n, .df, ...) {
#   tibble(CHG = ifelse(.df$AVISITN > 0, .df$AVAL - .df$BASE, NA))
# }


#' Generate Percent Change from Baseline Values
#'
#' @param n
#' @param .df data frame with required variables `AVISITN`, `CHG`, and `BASE`
#'
# gen_adeg_pchg <- function(n, .df, ...) {
#   tibble(PCHG = ifelse(.df$AVISITN > 0,
#                        100 * (.df$CHG / .df$BASE),
#                        NA)
#   )
# }


#' Generate Analysis Baseline Flags
#'
#' @param n
#' @param .df data frame with required variable `AVISIT`
#' @param visit_format
#'
#' @examples
#' gen_adeg_ablfl(NULL, data.frame(AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")))
#'
gen_adeg_ablfl <- function(n, .df, visit_format = "WEEK", ...) {
  tibble(ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
                        "Y",
                        ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1",
                               "Y",
                               "")))
}


#' Generate Analysis Flags 01 Baseline Post-Baseline
#'
#' @param n
#' @param .df data frame with required variables `ABLFL`, `DTYPE`, `WORS01FL`, and `AVISIT`
#'
#' @examples
#'
gen_adeg_anl01fl <- function(n, .df, ...) {
  tibble(ANL01FL = factor(ifelse(
    (.df$ABLFL == "Y" |  (is.na(.df$DTYPE) & .df$WORS01FL == "Y"))
    & (.df$AVISIT != "SCREENING"),
    "Y",
    ""))
  )
}


#' Generate Analysis Flags 03 Min Obs Within BType
#'
#' @param n
#' @param .df data frame with required variables `ABLFL`, `DTYPE`, `PARAMCD`
#'
#' @examples
#' gen_adeg_anl03fl(NULL,
#'                  data.frame(ABLFL = c("", "", "Y", "", "Y"),
#'                             DTYPE = c(NA, NA, "MINIMUM", NA, "MAXIMUM"),
#'                             PARAMCD = sample(s_adeg_paramcd, 5, replace = TRUE)))
#'
gen_adeg_anl03fl <- function(n, .df, ...) {
  tibble(ANL03FL = dplyr::case_when(
    .df$DTYPE == "MINIMUM" ~ "Y",
    .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
    TRUE ~ "")
  )
}


#' Generate Analysis Flags 04 Max Obs Within BType
#'
#' @param n
#' @param .df data frame with required variables `ABLFL`, `DTYPE`, `PARAMCD`
#'
#' @examples
#' gen_adeg_anl04fl(NULL,
#'                  data.frame(ABLFL = c("", "", "Y", "", "Y"),
#'                             DTYPE = c(NA, NA, "MINIMUM", NA, "MAXIMUM"),
#'                             PARAMCD = sample(s_adeg_paramcd, 5, replace = TRUE)))
#'
gen_adeg_anl04fl <- function(n, .df, ...) {
  tibble(ANL04FL = dplyr::case_when(
    .df$DTYPE == "MAXIMUM" ~ "Y",
    .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
    TRUE ~ "")
  )
}


#' Generate On Treatment Record Flags
#'
#' @param n
#' @param .df data frame with required variables `TRTSDTM`, `TRTEDTM`, `ADTM`
#'
#' @examples
#' gen_adeg_ontrtfl(NULL,
#'                  data.frame(
#'                  TRTSDTM = c("2019-03-06 14:03:04 EST", "2018-04-01 14:03:04 EST"),
#'                  TRTEDTM = c("2021-03-06 01:41:28 EST", "2021-09-26 09:43:22 EST"),
#'                  ADTM = c("2021-09-09 20:00:00 EDT", NA)))
#'
gen_adeg_ontrtfl <- function(n, .df, ...) {
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
#' @param n
#' @param .df data frame with required variables `AVISIT`
#'
#' @examples
#' gen_adeg_avisitn(NULL, data.frame(AVISIT = as.factor(c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36"))))
#'
gen_adeg_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = dplyr::case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_)
  )
}


#' Helper Function to easily match rows which comply to ONTRTFL derivation
#'
#' @param data
#' @param worst_obs
#'
flag_variables <- function(data, worst_obs) {

  data_compare <- data %>%
    mutate(row_check = seq_len(nrow(data)))

  data <- data_compare %>%
    {
      if (worst_obs == FALSE) group_by(., .df$USUBJID, .df$PARAMCD, .df$BASETYPE, .df$AVISIT) %>%
        arrange(., .df$ADTM, .df$ASPID, .df$EGSEQ)
      else group_by(., .df$USUBJID, .df$PARAMCD, .df$BASETYPE)
    } %>%
    filter(.df$AVISITN > 0 & (.df$ONTRTFL == "Y" | .df$ADTM <= .df$TRTSDTM) & is.na(.df$DTYPE)) %>%
    {
      if (worst_obs == TRUE) arrange(., .df$AVALC) %>% filter(., ifelse(
        .df$PARAMCD == "ECGINTP",
        ifelse(.df$AVALC == "ABNORMAL", .df$AVALC == "ABNORMAL", .df$AVALC == "NORMAL"),
        .df$AVAL == min(.df$AVAL)))
      else filter(., ifelse(
        .df$PARAMCD == "ECGINTP",
        .df$AVALC == "ABNORMAL" | .df$AVALC == "NORMAL",
        .df$AVAL == min(.df$AVAL)))
    } %>%
    slice(1) %>%
    {
      if (worst_obs == TRUE)
        mutate(., new_var = case_when(
          (.df$AVALC == "ABNORMAL" | .df$AVALC == "NORMAL") ~ "Y",
          (!is.na(.df$AVAL) & is.na(.df$DTYPE)) ~ "Y",
          TRUE ~ ""
        ))
      else
        mutate(., new_var = case_when(
          (.df$AVALC == "ABNORMAL" | .df$AVALC == "NORMAL") ~ "Y",
          (!is.na(.df$AVAL) & is.na(.df$DTYPE)) ~ "Y",
          TRUE ~ ""
        ))
    } %>%
    ungroup()

  data_compare$new_var <- ifelse(data_compare$row_check %in% data$row_check, "Y", "")
  data_compare <- data_compare[, -which(names(data_compare) %in% c("row_check"))]

  return(data_compare)
}

wors_depvars <- c("USUBJID", "PARAMCD", "BASETYPE", "AVISIT", "ADTM", "ASPID", "EGSEQ", "AVISITN", "ONTRTFL", "TRTSDTM", "DTYPE", "AVALC", "AVAL")

#' Generate Worst Observation in Window Flags 01
#'
#' @param n
#' @param .df data frame with required variables `USUBJID`, `PARAMCD`, `BASETYPE`, `AVISIT`, `ADTM`, `ASPID`, `EGSEQ`, `AVISITN`, `ONTRTFL`, `TRTSDTM`, `DTYPE`, `AVALC`, and `AVAL`
#'
gen_adeg_wors01fl <- function(n, .df, ...) {
  tibble(WORS01FL = flag_variables(.df, FALSE)$new_var)
}


#' Generate Worst Post-Baseline Observation Flags
#'
#' @param n
#' @param .df data frame with required variables `USUBJID`, `PARAMCD`, `BASETYPE`, `AVISIT`, `ADTM`, `ASPID`, `EGSEQ`, `AVISITN`, `ONTRTFL`, `TRTSDTM`, `DTYPE`, `AVALC`, and `AVAL`
#'
gen_adeg_wors02fl <- function(n, .df, ...) {
  tibble(WORS02FL = flag_variables(.df, TRUE)$new_var)
}


#' Generate Analysis Datetime
#'
#' @param n
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' gen_adeg_adtm(NULL,
#'               data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                          TRTEDTM = "2021-09-26 09:43:22 EST"))
#'
gen_adeg_adtm <- function(n, .df, study_duration = 2, ...) {
  study_duration_secs <- secs_per_year * study_duration
  trtsdt_int <- as.numeric(as.Date(.df$TRTSDTM))
  trtedt_int <- dplyr::case_when(!is.na(.df$TRTEDTM) ~ as.numeric(as.Date(.df$TRTEDTM)),
                          is.na(.df$TRTEDTM) ~ floor(trtsdt_int + (study_duration_secs) / 86400))
  tibble(ADTM = as.POSIXct((sample(trtsdt_int:trtedt_int, size = 1) * 86400), origin = "1970-01-01"))
}


#' Generate Analysis Relative Day
#'
#' @param n
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' gen_adeg_ady(NULL,
#'              data.frame(ADTM = "2021-11-20 19:00:00",
#'                         TRTSDTM = "2018-04-01 14:03:04 EST"))
#'
gen_adeg_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}

#' Recipes for creating ADEG CDISC Data
#'
#' @rdname adeg_recipes
#' @export
#'
adeg_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables, ~dependencies, ~func, ~func_args,
  "ADSL", "USUBJID", "PARAMCD", c("PARAM", "AVALU", "EGCAT", "mean_aval", "sd_aval", "ANRLO", "ANRHI"), no_deps, join_paramcd_adeg, NULL)

adeg_table_recipe <- tribble(
  ~variables,                          ~dependencies,                                         ~func,                 ~func_args,
  c("ASEQ", "EGSEQ"),                  "USUBJID",                                             gen_adeg_seq,          NULL,
  "ATPTN",                             no_deps,                                               gen_adeg_atptn,        NULL,
  c("EGTESTCD", "EGTEST"),             c("PARAMCD", "PARAM"),                                 gen_adeg_eg,           NULL,
  c("DTYPE", "AVISIT"),                dtype_depvars,                                         gen_adeg_avisit_dtype, list(minimum = TRUE),
  "ASPID",                             "USUBJID",                                             gen_adeg_aspid,        NULL,
  "AVAL",                              c("mean_aval", "sd_aval"),                             gen_adeg_aval,         NULL,
  "AVALC",                             c("PARAMCD", "AVAL"),                                  gen_adeg_adesc,        NULL,
  "ANRIND",                            c("AVAL", "ANRLO", "ANRHI"),                           gen_adeg_anrind,       NULL,
  # c("BASE", "BASETYPE"),               c("AVAL", "AVISITN", "ABLFL"),                         gen_adeg_base,         NULL,
  # "BASEC",                             basec_depvars,                                         gen_adeg_bdesc,        NULL,
  # "BNRIND",                            c("ANRIND", "ABLFL"),                                  gen_adeg_bnrind,       NULL,
  # "CHG",                               c("AVISITN", "AVAL", "BASE"),                          gen_adeg_chg,          NULL,
  # "PCHG",                              c("AVISITN", "CHG", "BASE"),                           gen_adeg_pchg,         NULL,
  "ABLFL",                             "AVISIT",                                              gen_adeg_ablfl,        list(visit_format = "WEEK"),
  "ANL01FL",                           c("ABLFL", "DTYPE", "WORS01FL", "AVISIT"),             gen_adeg_anl01fl,      NULL,
  "ANL03FL",                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_adeg_anl03fl,      NULL,
  "ANL04FL",                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_adeg_anl04fl,      NULL,
  "ONTRTFL",                           c("TRTSDTM", "TRTEDTM", "ADTM"),                       gen_adeg_ontrtfl,      NULL,
  "AVISITN",                           "AVISIT",                                              gen_adeg_avisitn,      NULL,
  "WORS01FL",                          wors_depvars,                                          gen_adeg_wors01fl,     NULL,
  "WORS02FL",                          wors_depvars,                                          gen_adeg_wors02fl,     NULL,
  "ADTM",                              c("TRTSDTM", "TRTEDTM"),                               gen_adeg_adtm,         list(study_duration = 2),
  "ADY",                               c("ADTM", "TRTSDTM"),                                  gen_adeg_ady,          NULL
)

# ADSL <- gen_table_data(N = 10, recipe = adsl_recipe)
# ADEG <- gen_reljoin_table(adeg_rel_join_recipe, adeg_table_recipe, db = list(ADSL = ADSL))

