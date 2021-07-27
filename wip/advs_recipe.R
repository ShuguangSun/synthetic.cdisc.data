advs_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

advs_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                      "ADSL",       "USUBJID",    advs_scaff, NULL)

advs_params <- c("ASEQ",     "VSSEQ",    "VSTESTCD", "VSTEST",   "VSCAT",    "VSSTRESC", "ASPID",    "PARAM",
                 "PARAMCD",  "AVAL",     "AVALU",    "BASE2",    "BASE",     "BASETYPE", "ABLFL2",   "ABLFL",
                 "CHG2",     "PCHG2",    "CHG",      "PCHG",     "DTYPE",    "ANRIND",   "ADTM",     "ADY",
                 "ATPTN",    "AVISIT",   "AVISITN",  "LOQFL",    "ONTRTFL")

s_advs_param = c("Diastolic Blood Pressure",
                 "Pulse Rate",
                 "Respiratory Rate",
                 "Systolic Blood Pressure",
                 "Temperature",
                 "Weight")

s_advs_paramcd = c("DIABP", "PULSE", "RESP", "SYSBP", "TEMP", "WEIGHT")

s_advs_paramu = c("Pa", "beats/min", "breaths/min", "Pa", "C", "Kg")

s_anrind <- c("HIGH", "LOW", "NORMAL")

visit_format = "WEEK"

advs_lookup <- tribble(~PARAM,                      ~PARAMCD,     ~AVALU,
                       "Diastolic Blood Pressure",  "DIABP",      "Pa",
                       "Pulse Rate",                "PULSE",      "beats/min",
                       "Respiratory Rate",          "RESP",       "breaths/min",
                       "Systolic Blood Pressure",   "SYSBP",      "Pa",
                       "Temperature",               "TEMP",       "C",
                       "Weight",                    "WEIGHT",     "Kg"
                       )

secs_per_year <- 31556952

#' Helper functions and constants for advs

vsseqvars <- c("ASEQ", "VSSEQ")

#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_advs_seq(NULL, x)
#'
#' gen_advs_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_advs_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               EGSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, c("ASEQ", "VSSEQ")]
}


#' Generate Vital Signs Test Names and Short Names
#'
#' @param n not used
#' @param .df data frame with required variables `PARAM` and `PARAMCD`
#'
#' @examples
#' gen_advs_vstest(NULL, advs_lookup)
#'
gen_advs_vstest <- function(n, .df, ...) {
  tibble(VSTEST = .df$PARAM,
         VSTESTCD = .df$PARAMCD
  )
}

#' Generate Vital Signs Category Labels
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_advs_vscat(nrow(x), x)
#'
gen_advs_vscat <- function(n, .df, ...) {
  tibble(VSCAT = rep("VITAL SIGNS", n))
}


#' Generate Analysis Values
#'
#' @param n not used
#' @param .df data frame
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_advs_aval(nrow(x), x)
#'
gen_advs_aval <- function(n, .df, ...) {
  tibble(AVAL = rnorm(n, mean = 50, sd = 8))
}


#' Generate Vital Signs Result Findings
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' gen_advs_vsstresc(NULL, gen_advs_aval(nrow(x), x))
#'
gen_advs_vsstresc <- function(n, .df, ...) {
  tibble(VSSTRESC = .df$AVAL)
}


#' Generate Sponsor Identifiers
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 3))
#' gen_advs_aspid(NULL, x)
#'
gen_advs_aspid <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASPID = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o]
}


#' Generate Analysis Timepoints
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_advs_atptn(nrow(x), x)
#'
gen_advs_atptn <- function(n, .df, ...) {
  tibble(ATPTN = rep(1, n))
}


# Helper function for variables `BASE` and `BASE2`
retain <- function(df, value_var, event, outside = NA) {
  indices <- c(1, which(event == TRUE), nrow(df) + 1)
  values <- c(outside, value_var[event == TRUE])
  rep(values, diff(indices))
}


#' Generate Baseline Values and Type
#'
#' @param n number of rows
#' @param .df data frame with required variables `AVAL`, `ABLFL`, and `ABLFL2`
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' data <- data.frame(
#'            AVAL = gen_advs_aval(nrow(x), x),
#'            ABLFL = sample(c("Y", ""), 10, replace = TRUE),
#'            ABLFL2 = sample(c("Y", ""), 10, replace = TRUE))
#' gen_advs_base(NULL, data)
#'
gen_advs_base <- function(n, .df, ...) {
  tibble(
    BASE = ifelse(.df$ABLFL2 != "Y", retain(.df, .df$AVAL, .df$ABLFL == "Y"), NA),
    BASE2 = retain(.df, .df$AVAL, .df$ABLFL2 == "Y"),
    BASETYPE = "LAST")
}


#' Generate Analysis Baseline Flags
#'
#' @param n number of rows
#' @param .df data frame with required variables `AVISIT`
#'
#' @examples
#' x <- data.frame(AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22"))
#' gen_advs_ablfl(NULL, x)
#'
gen_advs_ablfl <- function(n, .df, visit_format = "WEEK", ...) {
  tibble(
    ABLFL2 = ifelse(.df$AVISIT == "SCREENING", "Y", ""),
    ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
                   "Y",
                   ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1",
                          "Y",
                          "")))
}

#' Generate Changes from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `BASE2`, and `BASE`
#'
#'@examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' data <- data.frame(
#'            AVAL = gen_advs_aval(nrow(x), x),
#'            ABLFL = sample(c("Y", ""), 10, replace = TRUE),
#'            ABLFL2 = sample(c("Y", ""), 10, replace = TRUE))
#' gen_advs_chg(NULL,
#'              cbind(data.frame(AVAL = gen_advs_aval(nrow(x), x)),
#'                    gen_advs_base(NULL, data)))
#'
gen_advs_chg <- function(n, .df, ...) {
  tibble(CHG2 = .df$AVAL - .df$BASE2,
         CHG = .df$AVAL - .df$BASE)
}


#' Generate Percent Changes from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `CHG2`, `CHG`, `BASE2` and `BASE`
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' data <- data.frame(
#'            AVAL = gen_advs_aval(nrow(x), x),
#'            ABLFL = sample(c("Y", ""), 10, replace = TRUE),
#'            ABLFL2 = sample(c("Y", ""), 10, replace = TRUE))
#' gen_advs_pchg(NULL,
#'               cbind(gen_advs_base(NULL, data),
#'                     gen_advs_chg(NULL,
#'                          cbind(data.frame(AVAL = gen_advs_aval(nrow(x), x)),
#'                                gen_advs_base(NULL, data)))))
#'
gen_advs_pchg <- function(n, .df, ...) {
  tibble(PCHG2 = 100 * (.df$CHG2 / .df$BASE2),
         PCHG = 100 * (.df$CHG / .df$BASE))
}


#' Generate Derivation Types
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_advs_dtype(nrow(x), x)
#'
gen_advs_dtype <- function(n, .df, ...) {
  tibble(DTYPE = rep(NA, n))
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


#' Generate Analysis Visits
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_advs_avisit(nrow(x), x)
#'
gen_advs_avisit <- function(n, .df, visit_format = "WEEK", n_assessments = 5L, ...) {
  tibble(AVISIT = rep(visit_schedule(visit_format = visit_format, n_assessments = n_assessments),
                      ceiling(n/(ifelse(visit_format == "WEEK", 2, 1)+n_assessments)))[1:n])
}


#' Generate Analysis Visit Numbers
#'
#' @param n not used
#' @param .df data frame with required variable `AVISIT`
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_advs_avisitn(NULL, gen_advs_avisit(nrow(x), x))
#'
gen_advs_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = case_when(
          .df$AVISIT == "SCREENING" ~ -1,
          .df$AVISIT == "BASELINE" ~ 0,
          (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
          TRUE ~ NA_real_))
}


#' Generate Analysis Reference Range Indicators
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_advs_anrind(nrow(x), x)
#'
gen_advs_anrind <- function(n, .df, ...) {
  tibble(ANRIND = sample_fct(s_anrind, n, prob = c(0.1, 0.1, 0.8)))
}


#' Generate Analysis Datetime
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' gen_advs_adtm(NULL,
#'               data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                          TRTEDTM = "2021-09-26 09:43:22 EST"))
#'
gen_advs_adtm <- function(n, .df, study_duration = 2, ...) {
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
#' gen_advs_ady(NULL,
#'              data.frame(ADTM = "2021-11-20 19:00:00",
#'                         TRTSDTM = "2018-04-01 14:03:04 EST"))
#'
gen_advs_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}


#' Generate Limit of Quantification Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_advs_loqfl(NULL, gen_advs_aval(nrow(x), x))
#'
gen_advs_loqfl <- function(n, .df, ...) {
  tibble(LOQFL = ifelse(.df$AVAL < 32, "Y", "N"))
}


#' Generate On Treatment Record Flags
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM`, `TRTEDTM` and `ADTM`
#'
#' @examples
#' gen_advs_ontrtfl(NULL, data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                                   TRTEDTM = "2021-09-26 09:43:22 EST",
#'                                   ADTM = "2018-09-03 20:00:00 EST"))
#'
gen_advs_ontrtfl <- function(n, .df, ...) {
  tibble(ONTRTFL = factor(case_when(
    is.na(.df$TRTSDTM) ~ "",
    is.na(.df$ADTM) ~ "Y",
    (.df$ADTM < .df$TRTSDTM) ~ "",
    (.df$ADTM > .df$TRTEDTM) ~ "",
    TRUE ~ "Y")))
}

advs_params <- c("VSSTRESC", "ASPID",    "PARAM",
                 "PARAMCD",  "AVAL",     "AVALU",    "BASE2",    "BASE",     "BASETYPE", "ABLFL2",   "ABLFL",
                 "CHG2",     "PCHG2",    "CHG",      "PCHG",     "DTYPE",    "ANRIND",   "BNRIND",   "ADTM",
                 "ADY",      "ATPTN",    "AVISIT",   "AVISITN",  "LOQFL",    "ONTRTFL")


#' Recipes for creating advs CDISC Data
#'
#' @rdname advs_recipes
#' @export
#'
advs_recipe <- tribble(~variables,                      ~dependencies,                      ~func,             ~func_args,                                     ~keep,
                       vsseqvars,                       "USUBJID",                          gen_advs_vsseq,    NULL,                                           TRUE,
                       c("VSTESTCD", "VSTEST"),         c("PARAMCD", "PARAM"),              gen_advs_vstest,   NULL,                                           TRUE,
                       "VSCAT",                         no_deps,                            gen_advs_vscat,    NULL,                                           TRUE,
                       "VSSTRESC",                      "AVAL",                             gen_advs_vsstresc, NULL,                                           TRUE,
                       "ASPID",                         "USUBJID",                          gen_advs_aspid,    NULL,                                           TRUE,
                       "AVAL",                          no_deps,                            gen_advs_aval,     NULL,                                           TRUE,
                       c("BASE2", "BASE", "BASETYPE"),  c("AVAL", "ABLFL2", "ABLFL"),       gen_advs_base,     NULL,                                           TRUE,
                       c("ABLFL2", "ABLFL"),            "AVISIT",                           gen_advs_ablfl,    NULL,                                           TRUE,
                       c("CHG2", "CHG"),                c("AVAL", "BASE2", "BASE"),         gen_advs_chg,      NULL,                                           TRUE,
                       c("PCHG2", "PCHG"),              c("CHG2", "CHG", "BASE2", "BASE"),  gen_advs_pchg,     NULL,                                           TRUE,
                       "DTYPE",                         no_deps,                            gen_advs_dtype,    NULL,                                           TRUE,
                       "ANRIND",                        no_deps,                            gen_advs_anrind,   NULL,                                           TRUE,
                       "ADTM",                          c("TRTSDTM", "TRTEDTM"),            gen_advs_adtm,     list(study_duration = 2),                       TRUE,
                       "ADY",                           c("TRTSDTM", "TRTEDTM"),            gen_advs_ady,      NULL,                                           TRUE,
                       "ATPTN",                         no_deps,                            gen_advs_atptn,    NULL,                                           TRUE,
                       "AVISIT",                        no_deps,                            gen_advs_avisit,   list(visit_format = "WEEK", n_assessments = 5), TRUE,
                       "AVISITN",                       "AVISIT",                           gen_advs_avisitn,  NULL,                                           TRUE,
                       "LOQFL",                         "AVAL",                             gen_advs_loqfl,    NULL,                                           TRUE,
                       "ONTRTFL",                       c("TRTSDTM", "TRTEDTM", "ADTM"),    gen_advs_ontrtfl,  NULL,                                           TRUE
)
