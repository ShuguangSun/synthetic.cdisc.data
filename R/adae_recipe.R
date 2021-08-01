
#' ADAE recipe helper functions and constants
#' @rdname adae_helpers
#' @aliases adae, ADAE
#' @export
adae_sjf <- rand_per_key("USUBJID", tblnm = "ADSL", mincount = 0, maxcount = 10,
                          prop_present = 1)

#' @rdname cdisc_recs
#' @export
adae_scaff_recipe <- tribble(~foreign_tbls, ~foreign_key, ~func, ~func_args,
                      "ADSL", "USUBJID", adae_sjf, NULL)


adae_lookup <- tribble(
    ~AEBODSYS, ~AELLT,          ~AEDECOD,        ~AEHLT,        ~AEHLGT,      ~AETOXGR, ~AESOC, ~AESER, ~AREL,
    "cl A.1",  "llt A.1.1.1.1", "dcd A.1.1.1.1", "hlt A.1.1.1", "hlgt A.1.1", "1",        "cl A", "N",    "N",
    "cl A.1",  "llt A.1.1.1.2", "dcd A.1.1.1.2", "hlt A.1.1.1", "hlgt A.1.1", "2",        "cl A", "Y",    "N",
    "cl B.1",  "llt B.1.1.1.1", "dcd B.1.1.1.1", "hlt B.1.1.1", "hlgt B.1.1", "5",        "cl B", "N",    "Y",
    "cl B.2",  "llt B.2.1.2.1", "dcd B.2.1.2.1", "hlt B.2.1.2", "hlgt B.2.1", "3",        "cl B", "N",    "N",
    "cl B.2",  "llt B.2.2.3.1", "dcd B.2.2.3.1", "hlt B.2.2.3", "hlgt B.2.2", "1",        "cl B", "Y",    "N",
    "cl C.1",  "llt C.1.1.1.3", "dcd C.1.1.1.3", "hlt C.1.1.1", "hlgt C.1.1", "4",        "cl C", "N",    "Y",
    "cl C.2",  "llt C.2.1.2.1", "dcd C.2.1.2.1", "hlt C.2.1.2", "hlgt C.2.1", "2",        "cl C", "N",    "Y",
    "cl D.1",  "llt D.1.1.1.1", "dcd D.1.1.1.1", "hlt D.1.1.1", "hlgt D.1.1", "5",        "cl D", "Y",    "N",
    "cl D.1",  "llt D.1.1.4.2", "dcd D.1.1.4.2", "hlt D.1.1.4", "hlgt D.1.1", "3",        "cl D", "N",    "N",
    "cl D.2",  "llt D.2.1.5.3", "dcd D.2.1.5.3", "hlt D.2.1.5", "hlgt D.2.1", "1",        "cl D", "N",    "Y"
)

aeterm_func <- function(n, .df) gsub("dcd", "trm", .df$AEDECOD, fixed=TRUE)
aesev_func <- function(n, .df) {
    stopifnot("AETOXGR" %in% names(.df))
    mutate(.df, case_when(AETOXGR == 1 ~ "MILD",
                          AETOXGR %in% c(2, 3) ~ "MODERATE",
                          AETOXGR %in% c(4, 5) ~ "SEVERE"))
}


aeseqvars <- c("ASEQ", "AESEQ")
aeseq_func <- function(n, .df) {
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


dtmvars <- c("ASTDTM", "ASTDY", "AENDTM", "AENDY")
dtmdeps <- c("TRTEDTM", "TRTSDTM")


#' Add Random date and day columns
#'
#' This function generates random values for the \code{ASTDM} \code{AENDTM} \code{ASTDY} and \code{AENDY} columns and returns a data.frame containing those columns.
#' @param df data.frame
#' @export
gen_ase_vars <- function(n, .df, study_duration_secs = 2*secs_per_year) {
    sds <- .df$study_duration_secs[1]
    tstart <- .df$TRTSDTM
    tend <- .df$TRTEDTM
    ret <- data.frame( ASTDTM = rand_posixct(start = tstart, end = tend, max_duration_secs = sds))

    ret$AENDTM = rand_posixct(start = ret$ASTDTM, end = tend, max_duration_secs = sds)
    ret$ASTDY = ceiling(as.numeric(difftime(ret$ASTDTM, tstart, units = "days")))
    ret$AENDY = ceiling(as.numeric(difftime(ret$AENDTM, tstart, units = "days")))
    ret

}

#' @rdname adae_helpers
#' @param lookup data.frame. Data frame with possible adverse-event information to be sampled from.
#' @export
aes_func <- function(n, .df, lookup = adae_lookup) {
    lookup[sample(1:NROW(lookup), NROW(.df), replace =TRUE),]
}




#' @rdname cdisc_recs
#' @export
adae_tbl_recipe <- tribble(~variables, ~dependencies, ~func, ~func_args, ~keep,
                       names(adae_lookup), "USUBJID", aes_func, list(lookup = adae_lookup), TRUE,
                       "AETERM", "AEDECOD", aeterm_func, NULL, TRUE,
                       "AESEV", "AETOXGR", aesev_func, NULL, TRUE,
                       dtmvars, dtmdeps, gen_ase_vars, NULL, rep(TRUE, 4),
                       aeseqvars, "USUBJID", aeseq_func, NULL, TRUE)





