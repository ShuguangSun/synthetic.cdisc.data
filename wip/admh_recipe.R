mh_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

mh_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                    "ADSL",       "USUBJID",    mh_scaff,   NULL)

mh_vars <- c("ASEQ",     "MHSEQ",    "MHTERM",   "MHDECOD",  "MHBODSYS", "MHSOC",
             "ASTDTM",   "AENDTM",   "ASTDY",    "AENDY")

lookup_mh <- tribble(
  ~MHBODSYS, ~MHDECOD, ~MHSOC,
  "cl A", "trm A_1/2", "cl A",
  "cl A", "trm A_2/2", "cl A",
  "cl B", "trm B_1/3", "cl B",
  "cl B", "trm B_2/3", "cl B",
  "cl B", "trm B_3/3", "cl B",
  "cl C", "trm C_1/2", "cl C",
  "cl C", "trm C_2/2", "cl C",
  "cl D", "trm D_1/3", "cl D",
  "cl D", "trm D_2/3", "cl D",
  "cl D", "trm D_3/3", "cl D"
)

#' Helper functions and constants for ADMH

secs_per_year <- 31556952


#' Generate Analysis Start and End Datetimes
#'
#' @param n not used
#' @param .df data frame with required variables `TRTSDTM` and `TRTEDTM`
#' @param study_duration_secs duration of the study in seconds
#'
#' @examples
#' dtms <- data.frame(TRTSDTM = "2018-04-01 14:03:04 EST",
#'                    TRTEDTM = "2021-09-26 09:43:22 EST")
#' gen_mh_dtms(NULL, dtms)
#'
gen_mh_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year, ...) {
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
#' gen_mh_seq(NULL, x)
#'
#' gen_mh_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_mh_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               MHSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, mhseqvars]
}


#' Generate Reported Terms for Medical Histories
#'
#' @param n not used
#' @param .df data frame with required variable `MHDECOD`
#'
#' @examples
#' x <- data.frame(MHDECOD = c("A", "B", "C", "D"))
#' gen_mh_mhterm(NULL, x)
#'
gen_mh_mhterm <- function(n, .df, ...) {
  tibble(MHTERM = .df$MHDECOD)
}

mhseqvars <- c("ASEQ", "MHSEQ")
dtmvars <- c("ASTDTM", "ASTDY", "AENDTM", "AENDY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")

#' Recipes for creating ADMH CDISC Data
#'
#' @rdname admh_recipes
#' @export
#'
mh_recipe <- tribble(
  ~variables,  ~dependencies, ~func,            ~func_args,                                    ~keep,
  dtmvars,     dtmdeps,       gen_mh_dtms,      list(study_duration_secs = 1 * secs_per_year), TRUE,
  mhseqvars,   "USUBJID",     gen_mh_seq,       NULL,                                          TRUE,
  "MHTERM",    "MHDECOD",     gen_mh_mhterm,    NULL,                                          TRUE)

