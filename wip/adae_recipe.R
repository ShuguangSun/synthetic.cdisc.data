ae_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

ae_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                    "ADSL",       "USUBJID",    ae_scaff,   NULL)

lookup_ae <- tribble(
    ~AEBODSYS, ~AELLT,          ~AEDECOD,        ~AEHLT,        ~AEHLGT,      ~AETOXGR, ~AESOC, ~AESER, ~AREL,
    "cl A.1",  "llt A.1.1.1.1", "dcd A.1.1.1.1", "hlt A.1.1.1", "hlgt A.1.1", "1",      "cl A", "N",    "N",
    "cl A.1",  "llt A.1.1.1.2", "dcd A.1.1.1.2", "hlt A.1.1.1", "hlgt A.1.1", "2",      "cl A", "Y",    "N",
    "cl B.1",  "llt B.1.1.1.1", "dcd B.1.1.1.1", "hlt B.1.1.1", "hlgt B.1.1", "5",      "cl B", "N",    "Y",
    "cl B.2",  "llt B.2.1.2.1", "dcd B.2.1.2.1", "hlt B.2.1.2", "hlgt B.2.1", "3",      "cl B", "N",    "N",
    "cl B.2",  "llt B.2.2.3.1", "dcd B.2.2.3.1", "hlt B.2.2.3", "hlgt B.2.2", "1",      "cl B", "Y",    "N",
    "cl C.1",  "llt C.1.1.1.3", "dcd C.1.1.1.3", "hlt C.1.1.1", "hlgt C.1.1", "4",      "cl C", "N",    "Y",
    "cl C.2",  "llt C.2.1.2.1", "dcd C.2.1.2.1", "hlt C.2.1.2", "hlgt C.2.1", "2",      "cl C", "N",    "Y",
    "cl D.1",  "llt D.1.1.1.1", "dcd D.1.1.1.1", "hlt D.1.1.1", "hlgt D.1.1", "5",      "cl D", "Y",    "N",
    "cl D.1",  "llt D.1.1.4.2", "dcd D.1.1.4.2", "hlt D.1.1.4", "hlgt D.1.1", "3",      "cl D", "N",    "N",
    "cl D.2",  "llt D.2.1.5.3", "dcd D.2.1.5.3", "hlt D.2.1.5", "hlgt D.2.1", "1",      "cl D", "N",    "Y"
)

#' Helper functions and constants for ADAE

secs_per_year <- 31556952

#' Generate Analysis Start and End Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#'
#' @examples
#' dtms <- data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                    TRTEDTM = "2021-09-26 09:43:22 EST")
#' gen_ae_dtms(NULL, dtms)
#'
gen_ae_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year) {
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


#' Generate Reported Terms for the Adverse Events
#'
#' @param n not used
#' @param .df data frame with required variables `AEDECOD`
#'
#' @examples
#' gen_ae_aeterm(NULL, lookup_ae)
#'
gen_ae_aeterm <- function(n, .df) gsub("dcd", "trm", .df$AEDECOD, fixed = TRUE)


#' Generate Severity/Intensity Levels
#'
#' @param n not used
#' @param .df data frame with required variables `AETOXGR`
#'
#' @examples
#' gen_ae_aesev(NULL, lookup_ae)
#'
gen_ae_aesev <- function(n, .df, ...) {
  stopifnot("AETOXGR" %in% names(.df))
  tibble(AESEV = case_when(.df$AETOXGR == 1 ~ "MILD",
                        .df$AETOXGR %in% c(2, 3) ~ "MODERATE",
                        .df$AETOXGR %in% c(4, 5) ~ "SEVERE"))
}


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n not used
#' @param .df data frame with required variable `USUBJID`
#'
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#' aeseq_func(NULL, x)
#'
#' aeseq_func(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_ae_aeseq <- function(n, .df) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               AESEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, aeseqvars]
}

aeseqvars <- c("ASEQ", "AESEQ")
dtmvars <- c("ASTDTM", "ASTDY", "AENDTM", "AENDY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")


#' Recipes for creating ADAE CDISC Data
#'
#' @rdname adae_recipes
#' @export
#'
ae_recipe <- tribble(
  ~variables, ~dependencies, ~func,            ~func_args,                                    ~keep,
  "AETERM",   "AEDECOD",     gen_adae_aeterm,  NULL,                                          TRUE,
  "AESERV",   "AETOXGR",     gen_adae_aesev,   NULL,                                          TRUE,
  dtmvars,    dtmdeps,       gen_adae_dtms,    list(study_duration_secs = 1 * secs_per_year), TRUE,
  aeseqvars,  "USUBJID",     gen_adae_aeseq,   NULL,                                          TRUE)
