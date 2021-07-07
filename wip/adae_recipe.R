adae_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

adae_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                      "ADSL",       "USUBJID",    adae_scaff, NULL)

join_adae <- function(n, .df, .dbtab) {
  merge(.dbtab, adae_lookup, by = "USUBJID")
}

adae_lookup <- tribble(
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

secs_per_year <- 31556952

gen_ase_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year) {
  stopifnot(all(c("TRTSDTM", "TRTEDTM")) %in% names(.df))
  sds <- study_duration_secs
  tstart <- .df$TRTSDTM
  tend <- .df$TRTEDTM
  .df$ASTDTM = rand_posixct(start = tstart, end = tend, max_duration_secs = sds)
  .df$AENDTM = rand_posixct(start = .df$ASTDTM, end = tend, max_duration_secs = sds)
  .df$ASTDY = ceiling(as.numeric(difftime(.df$ASTDTM, tstart, units = "days")))
  .df$AENDY = ceiling(as.numeric(difftime(.df$ASTDTM, tend, units = "days")))
  .df
}

aeterm_func <- function(n, .df) gsub("dcd", "trm", .df$AEDECOD, fixed = TRUE)

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
dtmdeps <- c("TRTSDTM", "TRTEDTM")

adae_recipe <- tribble(~variables, ~dependencies, ~func,        ~func_args,                                    ~keep,
                       "AETERM",   "AEDECOD",     aeterm_func,  NULL,                                          TRUE,
                       "AESERV",   "AETOXGR",     aesev_func,   NULL,                                          TRUE,
                       dtmvars,    dtmdeps,       gen_ase_dtms, list(study_duration_secs = 1 * secs_per_year), TRUE,
                       aeseqvars,  "USUBJID",     aeseq_func,   NULL,                                          TRUE
               )
