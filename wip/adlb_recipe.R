# TODO fix BNRIND add examples BNRIND SHIFT1 BTOXGR ONTRTFL WFL ANL01FL

lb_vars <- c("ASEQ",     "LBSEQ",    "LBTESTCD", "LBTEST",   "LBCAT",    "LBSTRESC",
             "ASPID",    "PARAM",    "PARAMCD",  "AVAL",     "AVALU",    "BASE2",
             "BASE",     "BASETYPE", "ABLFL2",   "ABLFL",    "CHG2",     "PCHG2",
             "CHG",      "PCHG",     "DTYPE",    "ANRIND",   "BNRIND",   "SHIFT1",
             "ATOXGR",   "BTOXGR",   "ADTM",     "ADY",      "ATPTN",    "AVISIT",
             "AVISITN",  "LOQFL",    "ONTRTFL",  "WORS01FL", "WGRHIFL",  "WGRLOFL",
             "WGRHIVFL", "WGRLOVFL", "ANL01FL")

lookup_lb <- tribble(
  ~PARAM,                                  ~PARAMCD, ~AVALU,   ~LBCAT,
  "Alanine Aminotransferase Measurement",  "ALT",    "g/L",    "CHEMISTRY",
  "C-Reactive Protein Measurement",        "CRP",    "mg/L",   "CHEMISTRY",
  "Immunoglobulin A Measurement",          "IGA",    "U/L",    "IMMUNOLOGY"
)

#' Helper functions and constants for ADLB

s_lb_lbcat <- c("CHEMISTRY", "CHEMISTRY", "IMMUNOLOGY")

s_lb_param <- c(
  "Alanine Aminotransferase Measurement",
  "C-Reactive Protein Measurement",
  "Immunoglobulin A Measurement"
)

s_anrind <- c("HIGH", "LOW", "NORMAL")

s_lb_paramcd <- c("ALT", "CRP", "IGA")

s_lb_paramu <- c("U/L", "mg/L", "g/L")

visit_format <- "WEEK"


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_lb_seq(NULL, x)
#'
#' gen_lb_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_lb_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               LBSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, lbseqvars]
}


#' Generate Sponsor Identifiers
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 3))
#' gen_lb_aspid(NULL, x)
#'
gen_lb_aspid <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASPID = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  tibble(ASPID = retdf[o, "ASPID"])
}


#' Generate Analysis Values
#'
#' @param n not used
#' @param .df data frame
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_lb_aval(nrow(x), x)
#'
gen_lb_aval <- function(n, .df, ...) {
  tibble(AVAL = rnorm(n, mean = 50, sd = 8))
}


#' Generate Analysis Timepoints
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_lb_atptn(nrow(x), x)
#'
gen_lb_atptn <- function(n, .df, ...) {
  tibble(ATPTN = rep(1, n))
}


#' Generate Lab Test or Examination Name and Short Name
#'
#' @param n not used
#' @param .df data frame with required variables `PARAM` and `PARAMCD`
#'
#' @examples
#' gen_lb_lbtest(NULL, lookup_lb)
#'
gen_lb_lbtest <- function(n, .df, ...) {
  tibble(LBTEST = .df$PARAM,
         LBTESTCD = .df$PARAMCD)
}


#' Generate Lab Test Result Values
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_lb_lbstresc(NULL, gen_lb_aval(nrow(x), x))
#'
gen_lb_lbstresc <- function(n, .df, ...) {
  tibble(LBSTRESC = as.character(.df$AVAL))
}


#' Generate Baseline Values and Types
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `ABLFL`, and `ABLFL2`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_lb_base(NULL, cbind(gen_lb_aval(nrow(x), x),
#'                         gen_lb_ablfl(NULL, gen_lb_avisit(nrow(x), NULL))))
#'
gen_lb_base <- function(n, .df, ...) {
  tibble(BASE2 = retain(.df, .df$AVAL, .df$ABLFL2 == "Y"),
         BASE = ifelse(.df$ABLFL2 != "Y", retain(.df, .df$AVAL, .df$ABLFL == "Y"), NA),
         BASETYPE = "LAST")
}


#' Generate Analysis Baseline Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVISIT`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_lb_ablfl(NULL, gen_lb_avisit(nrow(x), NULL))
#'
gen_lb_ablfl <- function(n, .df, visit_format = "WEEK", ...) {
  tibble(ABLFL2 = ifelse(.df$AVISIT == "SCREENING", "Y", ""),
         ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
                        "Y",
                        ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1", "Y", "")))
}


#' Generate Change from Baseline Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVAL`, `BASE2`, and `BASE`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' BASE <- gen_lb_base(NULL, cbind(gen_lb_aval(nrow(x), x),
#'                     gen_lb_ablfl(NULL, gen_lb_avisit(nrow(x), NULL))))
#' gen_lb_chg(NULL, cbind(gen_lb_aval(nrow(x), x), BASE))
#'
gen_lb_chg <- function(n, .df, ...) {
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
#' BASE <- gen_lb_base(NULL, cbind(gen_lb_aval(nrow(x), x),
#'                     gen_lb_ablfl(NULL, gen_lb_avisit(nrow(x), NULL))))
#' CHG <- gen_lb_chg(NULL, cbind(gen_lb_aval(nrow(x), x), BASE))
#' gen_lb_pchg(NULL, cbind(BASE, CHG))
#'
gen_lb_pchg <- function(n, .df, ...) {
  tibble(PCHG2 = 100 * (.df$CHG2 / .df$BASE2),
         PCHG = 100 * (.df$CHG / .df$BASE))
}


#' Generate Derivation Types
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_lb_dtype(nrow(x), NULL)
#'
gen_lb_dtype <- function(n, .df, ...) {
  tibble(DTYPE = rep(NA, n))
}


#' Generate Analysis Reference Range Indicators
#'
#' @param n number of rows
#' @param .df not used
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_lb_anrind(nrow(x), NULL)
#'
gen_lb_anrind <- function(n, .df, ...) {
  tibble(ANRIND = sample_fct(s_anrind, n, prob = c(0.1, 0.1, 0.8)))
}


#' Generate Baseline Reference Range Indicators
#'
#' @param n not used
#' @param .df data frame with required variables `ANRIND` and `ABLFL`
#'
#' @examples
#' x <- data.frame(ID = 1:5)
#' gen_lb_bnrind(NULL, NA)
#'
gen_lb_bnrind <- function(n, .df, ...) {
  tibble(BNRIND = .df$ANRIND[.df$ABLFL == "Y"])
}


#' Generate Shifts from Baselines to Analysis Values
#'
#' @param n not used
#' @param .df data frame with required variables `AVISITN`, `BNRIND`, and `ANRIND`
#'
#' @examples
#' x <- data.frame(ID = 1:5)
#' gen_lb_shift1(NULL, NA)
#'
gen_lb_shift1 <- function(n, .df, ...) {
  tibble(SHIFT1 = factor(ifelse(
    .df$AVISITN > 0,
    paste(retain(.df, as.character(.df$BNRIND), .df$AVISITN == 0), .df$ANRIND, sep = " to "),
    "")))
}


#' Generate Analysis Toxicity Grades
#'
#' @param n not used
#' @param .df data frame with required variables `AVISITN`, `BNRIND`, and `ANRIND`
#'
#' @examples
#' x <- data.frame(ID = 1:10)
#' gen_lb_atoxgr(NULL, gen_lb_anrind(nrow(x), NULL))
#'
gen_lb_atoxgr <- function(n, .df, ...) {
  tibble(ATOXGR = factor(case_when(
    .df$ANRIND == "LOW" ~ sample(
      c("-1", "-2", "-3", "-4", "-5"),
      nrow(.df),
      replace = TRUE,
      prob = c(0.30, 0.25, 0.20, 0.15, 0)),
    .df$ANRIND == "HIGH" ~ sample(
      c("1", "2", "3", "4", "5"),
      nrow(.df),
      replace = TRUE,
      prob = c(0.30, 0.25, 0.20, 0.15, 0)),
    .df$ANRIND == "NORMAL" ~ "0")))
}


#' Generate Baseline Toxicity Grades
#'
#' @param n not used
#' @param .df data frame with required variables `ATOXGR` and `ABLFL`
#'
#' @examples
#' x <- data.frame(ID = 1:5)
#' gen_lb_btoxgr(NULL, cbind(data.frame(ABLFL = c("Y", "Y", "", "", "Y")),
#'                           gen_lb_atoxgr(NULL, gen_lb_anrind(nrow(x), NULL))))
#'
gen_lb_btoxgr <- function(n, .df, ...) {
  tibble(BTOXGR = .df$ATOXGR[.df$ABLFL == "Y"])
}


#' Generate Analysis Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#' @param study_duration duration of the study in years
#'
#' @examples
#' gen_lb_adtm(NULL, data.frame(TRTSDTM = c("2018-04-01 14:03:04 EST","2018-04-01 14:03:04 EST"),
#'                              TRTEDTM = c("2021-09-26 09:43:22 EST", "2021-09-26 09:43:22 EST")))
#'
gen_lb_adtm <- function(n, .df, study_duration = 2, ...) {
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
#' gen_lb_ady(NULL,
#'            data.frame(ADTM = c("2018-05-01 14:03:04 EST", "2021-11-20 19:00:00"),
#'                       TRTSDTM = c("2018-04-01 14:03:04 EST", "2018-04-01 14:03:04 EST")))
#'
gen_lb_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}


#' Generate Analysis Visits
#'
#' @param n number of rows
#' @param .df not used
#' @param visit_format
#' @param n_assessments number of assessments taken per subject
#' @param n_days
#'
#' @examples
#' x <- data.frame(A = 1:10)
#' gen_lb_avisit(nrow(x), NULL)
#'
gen_lb_avisit <- function(n, .df, visit_format = "WEEK", n_assessments = 5L, n_days = 5L, ...) {
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
#' gen_lb_avisitn(NULL, gen_lb_avisit(nrow(x), NULL))
#'
gen_lb_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = dplyr::case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_))
}


#' Generate Limit of Quantification Flags
#'
#' @param n not used
#' @param .df data frame with required variable `AVAL`
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_lb_loqfl(NULL, gen_lb_aval(nrow(x), x))
#'
gen_lb_loqfl <- function(n, .df, ...) {
  tibble(LOQFL = ifelse(.df$AVAL < 32, "Y", "N"))
}


#' Generate On Treatment Record Flags
#'
#' @param n not used
#' @param .df data frame with required variables `ADTM`, `TRTSDTM`, and `TRTEDTM`
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_lb_ontrtfl(NULL, NA)
#'
gen_lb_ontrtfl <- function(n, .df, ...) {
  tibble(ONTRTFL = factor(case_when(
    is.na(.df$TRTSDTM) ~ "",
    is.na(.df$ADTM) ~ "Y",
    (.df$ADTM < .df$TRTSDTM) ~ "",
    (.df$ADTM > .df$TRTEDTM) ~ "",
    TRUE ~ "Y")))
}


#' Generate Various Flags
#'
#' @param n not used
#' @param .df data frame
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_lb_wfl(NULL, NA)
#'
gen_lb_wfl <- function(n, .df, ...) {
  tibble(WORS01FL = flag_variables(.df, TRUE, "ELSE", FALSE),
         WGRHIFL = flag_variables(.df, FALSE, TRUE, TRUE),
         WGRLOFL = flag_variables(.df, FALSE, FALSE, TRUE),
         WGRHIVFL = flag_variables(.df, TRUE, TRUE, TRUE),
         WGRLOVFL = flag_variables(.df, TRUE, FALSE, TRUE))
}


#' Generate Analysis Flags 01 Baseline Post-Baseline
#'
#' @param n not used
#' @param .df data frame
#'
#' @examples
#' x <- data.frame(ID = rep(1:10, each = 3))
#' gen_lb_anl01fl(NULL, NA)
#'
gen_lb_anl01fl <- function(n, .df, ...) {
  tibble(ANL01FL = ifelse(
    (.df$ABLFL == "Y" | (.df$WORS01FL == "Y" & is.na(.df$DTYPE)))
    & (.df$AVISIT != "SCREENING"),
    "Y",
    ""))
}


lbseqvars <- c("ASEQ", "LBSEQ")
dtmvars <- c("ASTDTM", "ASTDY", "AENDTM", "AENDY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")
wflvars <- c("WORS01FL", "WGRHIFL", "WGRLOFL", "WGRHIVFL", "WGRLOVFL")
anlfldeps <- c("ABLFL", "WORS01FL", "DTYPE", "AVISIT")


lb_recipe <- tribble(
  ~variables,                       ~dependencies,                      ~func,            ~func_args,                                                     ~keep,
  lbseqvars,                        "USUBJID",                          gen_lb_seq,       NULL,                                                           TRUE,
  c("LBTEST", "LBTESTCD"),          c("PARAM", "PARAMCD"),              gen_lb_lbtest,    NULL,                                                           TRUE,
  "ASPID",                          "USUBJID",                          gen_lb_aspid,     NULL,                                                           TRUE,
  "AVAL",                           no_deps,                            gen_lb_aval,      NULL,                                                           TRUE,
  "ATPTN",                          no_deps,                            gen_lb_atptn,     NULL,                                                           TRUE,
  "LBSTRESC",                       "AVAL",                             gen_lb_lbstresc,  NULL,                                                           TRUE,
  c("BASE2", "BASE", "BASETYPE"),   c("AVAL", "ABLFL2", "ABLFL"),       gen_lb_base,      NULL,                                                           TRUE,
  c("ABLFL2", "ABLFL"),             "AVISIT",                           gen_lb_ablfl,     list(visit_format = "WEEK"),                                    TRUE,
  c("CHG2", "CHG"),                 c("AVAL", "BASE2", "BASE"),         gen_lb_chg,       NULL,                                                           TRUE,
  c("PCHG2", "PCHG"),               c("CHG2", "CHG", "BASE2", "BASE"),  gen_lb_pchg,      NULL,                                                           TRUE,
  "DTYPE",                          no_deps,                            gen_lb_dtype,     NULL,                                                           TRUE,
  "ANRIND",                         no_deps,                            gen_lb_anrind,    NULL,                                                           TRUE,
  "BNRIND",                         c("ANRIND", "ABLFL"),               gen_lb_bnrind,    NULL,                                                           TRUE,
  "SHIFT1",                         c("AVISITN", "ANRIND", "BNRIND"),   gen_lb_shift1,    NULL,                                                           TRUE,
  "ATOXGR",                         c("AVISITN", "ANRIND", "BNRIND"),   gen_lb_atoxgr,    NULL,                                                           TRUE,
  "BTOXGR",                         c("ATOXGR", "ABLFL"),               gen_lb_btoxgr,    NULL,                                                           TRUE,
  "ADTM",                           c("TRTSDTM", "TRTEDTM"),            gen_lb_adtm,      list(study_duation = 2),                                        TRUE,
  "ADY",                            c("TRTSDTM", "TRTEDTM"),            gen_lb_ady,       NULL,                                                           TRUE,
  "AVISIT",                         no_deps,                            gen_lb_avisit,    list(visit_format = "WEEK", n_assessments = 5L, n_days = 5L),   TRUE,
  "AVISITN",                        "AVISIT",                           gen_lb_avisitn,   NULL,                                                           TRUE,
  "LOQFL",                          "AVAL",                             gen_lb_loqfl,     NULL,                                                           TRUE,
  "ONTRTFL",                        c("ADTM", "TRTSDTM", "TRTEDTM"),    gen_lb_ontrtfl,   NULL,                                                           TRUE,
  wflvars,                          no_deps,                            gen_lb_wfl,       NULL,                                                           TRUE,
  "ANL01FL",                        anlfl_deps,                         gen_lb_anl01fl,   NULL,                                                           TRUE)
