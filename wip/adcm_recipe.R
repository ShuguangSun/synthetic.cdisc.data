cm_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

cm_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                    "ADSL",       "USUBJID",    cm_scaff,   NULL)

cm_vars <- c("ASEQ",   "CMSEQ",  "CMDECOD", "CMCAT",  "CMCLAS", "ATC1",   "ATC2",
             "ATC3",   "ATC4",   "ATC1CD",  "ATC2CD", "ATC3CD", "ATC4CD", "ATIREL",
             "ASTDTM", "AENDTM", "ASTDY",   "AENDY")

lookup_cm <- tribble(
  ~CMCLAS,   ~CMDECOD,        ~ATIREL,
  "medcl A", "medname A_1/3", "PRIOR",
  "medcl A", "medname A_2/3", "CONCOMITANT",
  "medcl A", "medname A_3/3", "CONCOMITANT",
  "medcl B", "medname B_1/4", "CONCOMITANT",
  "medcl B", "medname B_2/4", "PRIOR",
  "medcl B", "medname B_3/4", "PRIOR",
  "medcl B", "medname B_4/4", "CONCOMITANT",
  "medcl C", "medname C_1/2", "CONCOMITANT",
  "medcl C", "medname C_2/2", "CONCOMITANT"
)

#' Helper functions and constants for ADCM

secs_per_year <- 31556952


#' Generate Analysis Start and End Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' dtms <- data.frame(TRTSDTM = c("2018-04-01 14:03:04 EST","2018-04-01 14:03:04 EST"),
#'                    TRTEDTM = c("2021-09-26 09:43:22 EST", "2021-09-26 09:43:22 EST"))
#' gen_cm_dtms(NULL, dtms)
#'
gen_cm_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year, ...) {
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


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' gen_cm_seq(NULL, x)
#'
#' gen_cm_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_cm_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               CMSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, cmseqvars]
}


#' Generate Medication Categories
#'
#' @param n not used
#' @param .df data frame with required variable `CMCLAS`
#'
#' @examples
#' gen_cm_cmcat(NULL, lookup_cm)
#'
gen_cm_cmcat <- function(n, .df, ...) {
  tibble(CMCAT = .df$CMCLAS)
}


#' Generate ATC Levels Text
#'
#' @param n not used
#' @param .df data frame with required variable `CMDECOD`
#'
#' @examples
#' gen_cm_atc(NULL, lookup_cm)
#'
gen_cm_atc <- function(n, .df, ...) {
  tibble(
    ATC1 = paste("ATCCLAS1", substr(.df$CMDECOD, 9, 9)),
    ATC2 = paste("ATCCLAS2", substr(.df$CMDECOD, 9, 9)),
    ATC3 = paste("ATCCLAS3", substr(.df$CMDECOD, 9, 9)),
    ATC4 = paste("ATCCLAS4", substr(.df$CMDECOD, 9, 9))
  )
}


#' Generate ATC Level Codes Text
#'
#' @param n not used
#' @param .df data frame with required variables `ATC1`, `ATC2`, `ATC3`, and `ATC4`
#'
#' @examples
#' x <- data.frame(ATC1 = "ATCCLAS1 A", ATC2 = "ATCCLAS2 B", ATC3 = "ATCCLAS3 C", ATC4 = "ATCCLAS4 A")
#' gen_cm_atccd(NULL, x)
#'
gen_cm_atccd <- function(n, .df, ...) {
  tibble(
    ATC1CD = .df$ATC1,
    ATC2CD = .df$ATC2,
    ATC3CD = .df$ATC3,
    ATC4CD = .df$ATC4
  )
}

cmseqvars <- c("ASEQ", "CMSEQ")
dtmvars <- c("ASTDTM", "ASTDY", "AENDTM", "AENDY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")
cmatcvars <- c("ATC1", "ATC2", "ATC3", "ATC4")
cmatccdvars <- c("ATC1CD", "ATC2CD", "ATC3CD", "ATC4CD")


#' Recipes for creating ADCM CDISC Data
#'
#' @rdname adcm_recipes
#' @export
#'
cm_recipe <- tribble(
  ~variables,  ~dependencies, ~func,          ~func_args,                                    ~keep,
  dtmvars,     dtmdeps,       gen_cm_dtms,    list(study_duration_secs = 1 * secs_per_year), TRUE,
  cmseqvars,   "USUBJID",     gen_cm_seq,     NULL,                                          TRUE,
  "CMCAT",     "CMCLAS",      gen_cm_cmcat,   NULL,                                          TRUE,
  cmatcvars,   "CMDECOD",     gen_cm_atc,     NULL,                                          TRUE,
  cmatccdvars, cmatcvars,     gen_cm_atccd,   NULL,                                          TRUE)
