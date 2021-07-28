vs_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

vs_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                    "ADSL",       "USUBJID",    vs_scaff,   NULL)

lookup_vs <- tribble(
  ~PARAM,                      ~PARAMCD,     ~AVALU,
  "Diastolic Blood Pressure",  "DIABP",      "Pa",
  "Pulse Rate",                "PULSE",      "beats/min",
  "Respiratory Rate",          "RESP",       "breaths/min",
  "Systolic Blood Pressure",   "SYSBP",      "Pa",
  "Temperature",               "TEMP",       "C",
  "Weight",                    "WEIGHT",     "Kg"
)

#' Helper functions and constants for ADVS

s_vs_param <- c("Diastolic Blood Pressure",
                "Pulse Rate",
                "Respiratory Rate",
                "Systolic Blood Pressure",
                "Temperature",
                "Weight")

s_vs_paramcd <- c("DIABP", "PULSE", "RESP", "SYSBP", "TEMP", "WEIGHT")

s_vs_paramu <- c("Pa", "beats/min", "breaths/min", "Pa", "C", "Kg")

s_anrind <- c("HIGH", "LOW", "NORMAL")

visit_format <- "WEEK"

secs_per_year <- 31556952


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_vs_seq(NULL, x)
#'
#' gen_vs_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_vs_seq <- function(n, .df, ...) {
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
#' gen_vs_vstest(NULL, lookup_vs)
#'
gen_vs_vstest <- function(n, .df, ...) {
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
#' gen_vs_vscat(nrow(x), x)
#'
gen_vs_vscat <- function(n, .df, ...) {
  tibble(VSCAT = rep("VITAL SIGNS", n))
}


#' Generate Analysis Values
#'
#' @param n not used
#' @param .df data frame
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_vs_aval(nrow(x), x)
#'
gen_vs_aval <- function(n, .df, ...) {
  tibble(AVAL = rnorm(n, mean = 50, sd = 8))
}


#' Generate Vital Signs Result Findings
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' gen_vs_vsstresc(NULL, gen_vs_aval(nrow(x), x))
#'
gen_vs_vsstresc <- function(n, .df, ...) {
  tibble(VSSTRESC = .df$AVAL)
}


#' Generate Sponsor Identifiers
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 3))
#' gen_vs_aspid(NULL, x)
#'
gen_vs_aspid <- function(n, .df, ...) {
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
#' gen_vs_atptn(nrow(x), x)
#'
gen_vs_atptn <- function(n, .df, ...) {
  tibble(ATPTN = rep(1, n))
}


#' Generate Baseline Values and Type
#'
#' @param n number of rows
#' @param .df data frame with required variables `AVAL`, `ABLFL`, and `ABLFL2`
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' data <- data.frame(
#'            AVAL = gen_vs_aval(nrow(x), x),
#'            ABLFL = sample(c("Y", ""), 10, replace = TRUE),
#'            ABLFL2 = sample(c("Y", ""), 10, replace = TRUE))
#' gen_vs_base(NULL, data)
#'
gen_vs_base <- function(n, .df, ...) {
  tibble(
    BASE = ifelse(.df$ABLFL2 != "Y", retain(.df, .df$AVAL, .df$ABLFL == "Y"), NA),
    BASE2 = retain(.df, .df$AVAL, .df$ABLFL2 == "Y"),
    BASETYPE = "LAST")
}


#' Generate Analysis Baseline Flags
#'
#' @param n number of rows
#' @param .df data frame with required variables `AVISIT`
#' @param visit_format
#'
#' @examples
#' x <- data.frame(AVISIT = c("SCREENING", "BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22"))
#' gen_vs_ablfl(NULL, x)
#'
gen_vs_ablfl <- function(n, .df, visit_format = "WEEK", ...) {
  tibble(
    ABLFL2 = ifelse(.df$AVISIT == "SCREENING", "Y", ""),
    ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
                   "Y",
                   ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1",
                          "Y", "")))
}


#' Generate Changes from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `BASE2`, and `BASE`
#'
#'@examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' data <- data.frame(
#'            AVAL = gen_vs_aval(nrow(x), x),
#'            ABLFL = sample(c("Y", ""), 10, replace = TRUE),
#'            ABLFL2 = sample(c("Y", ""), 10, replace = TRUE))
#' gen_vs_chg(NULL,
#'              cbind(data.frame(AVAL = gen_vs_aval(nrow(x), x)),
#'                    gen_vs_base(NULL, data)))
#'
gen_vs_chg <- function(n, .df, ...) {
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
#'            AVAL = gen_vs_aval(nrow(x), x),
#'            ABLFL = sample(c("Y", ""), 10, replace = TRUE),
#'            ABLFL2 = sample(c("Y", ""), 10, replace = TRUE))
#' gen_vs_pchg(NULL,
#'               cbind(gen_vs_base(NULL, data),
#'                     gen_vs_chg(NULL,
#'                          cbind(data.frame(AVAL = gen_vs_aval(nrow(x), x)),
#'                                gen_vs_base(NULL, data)))))
#'
gen_vs_pchg <- function(n, .df, ...) {
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
#' gen_vs_dtype(nrow(x), x)
#'
gen_vs_dtype <- function(n, .df, ...) {
  tibble(DTYPE = rep(NA, n))
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
#' gen_vs_avisit(nrow(x), x)
#'
gen_vs_avisit <- function(n, .df, visit_format = "WEEK", n_assessments = 5L, ...) {
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
#' gen_vs_avisitn(NULL, gen_vs_avisitn(nrow(x), x))
#'
gen_vs_avisitn <- function(n, .df, ...) {
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
#' gen_vs_anrind(nrow(x), x)
#'
gen_vs_anrind <- function(n, .df, ...) {
  tibble(ANRIND = sample_fct(s_anrind, n, prob = c(0.1, 0.1, 0.8)))
}


#' Generate Analysis Datetime
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#' @param study_duration duration of the study in seconds
#'
#' @examples
#' gen_vs_adtm(NULL,
#'             data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                        TRTEDTM = "2021-09-26 09:43:22 EST"))
#'
gen_vs_adtm <- function(n, .df, study_duration = 2, ...) {
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
#' gen_vs_ady(NULL,
#'            data.frame(ADTM = "2021-11-20 19:00:00",
#'                       TRTSDTM = "2018-04-01 14:03:04 EST"))
#'
gen_vs_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}


#' Generate Limit of Quantification Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_vs_loqfl(NULL, gen_vs_aval(nrow(x), x))
#'
gen_vs_loqfl <- function(n, .df, ...) {
  tibble(LOQFL = ifelse(.df$AVAL < 32, "Y", "N"))
}


#' Generate On Treatment Record Flags
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM`, `TRTEDTM` and `ADTM`
#'
#' @examples
#' gen_vs_ontrtfl(NULL, data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                                 TRTEDTM = "2021-09-26 09:43:22 EST",
#'                                 ADTM = "2018-09-03 20:00:00 EST"))
#'
gen_vs_ontrtfl <- function(n, .df, ...) {
  tibble(ONTRTFL = factor(case_when(
    is.na(.df$TRTSDTM) ~ "",
    is.na(.df$ADTM) ~ "Y",
    (.df$ADTM < .df$TRTSDTM) ~ "",
    (.df$ADTM > .df$TRTEDTM) ~ "",
    TRUE ~ "Y")))
}

vsseqvars <- c("ASEQ", "VSSEQ")

#' Recipes for creating ADVS CDISC Data
#'
#' @rdname advs_recipes
#' @export
#'
vs_recipe <- tribble(
  ~variables,                      ~dependencies,                      ~func,           ~func_args,                                     ~keep,
  vsseqvars,                       "USUBJID",                          gen_vs_vsseq,    NULL,                                           TRUE,
  c("VSTESTCD", "VSTEST"),         c("PARAMCD", "PARAM"),              gen_vs_vstest,   NULL,                                           TRUE,
  "VSCAT",                         no_deps,                            gen_vs_vscat,    NULL,                                           TRUE,
  "VSSTRESC",                      "AVAL",                             gen_vs_vsstresc, NULL,                                           TRUE,
  "ASPID",                         "USUBJID",                          gen_vs_aspid,    NULL,                                           TRUE,
  "AVAL",                          no_deps,                            gen_vs_aval,     NULL,                                           TRUE,
  c("BASE2", "BASE", "BASETYPE"),  c("AVAL", "ABLFL2", "ABLFL"),       gen_vs_base,     NULL,                                           TRUE,
  c("ABLFL2", "ABLFL"),            "AVISIT",                           gen_vs_ablfl,    list(visit_format = "WEEK"),                    TRUE,
  c("CHG2", "CHG"),                c("AVAL", "BASE2", "BASE"),         gen_vs_chg,      NULL,                                           TRUE,
  c("PCHG2", "PCHG"),              c("CHG2", "CHG", "BASE2", "BASE"),  gen_vs_pchg,     NULL,                                           TRUE,
  "DTYPE",                         no_deps,                            gen_vs_dtype,    NULL,                                           TRUE,
  "ANRIND",                        no_deps,                            gen_vs_anrind,   NULL,                                           TRUE,
  "ADTM",                          c("TRTSDTM", "TRTEDTM"),            gen_vs_adtm,     list(study_duration = 2),                       TRUE,
  "ADY",                           c("TRTSDTM", "TRTEDTM"),            gen_vs_ady,      NULL,                                           TRUE,
  "ATPTN",                         no_deps,                            gen_vs_atptn,    NULL,                                           TRUE,
  "AVISIT",                        no_deps,                            gen_vs_avisit,   list(visit_format = "WEEK", n_assessments = 5), TRUE,
  "AVISITN",                       "AVISIT",                           gen_vs_avisitn,  NULL,                                           TRUE,
  "LOQFL",                         "AVAL",                             gen_vs_loqfl,    NULL,                                           TRUE,
  "ONTRTFL",                       c("TRTSDTM", "TRTEDTM", "ADTM"),    gen_vs_ontrtfl,  NULL,                                           TRUE)
