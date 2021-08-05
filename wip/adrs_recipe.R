## TODO avalc, EXAMPLES

adrs_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

adrs_sjrec <- tribble(~foreign_tbl, ~foreign_key, ~func,      ~func_args,
                      "ADSL",       "USUBJID",    adrs_scaff, NULL)

rs_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables,             ~dependencies, ~func,      ~func_args,
  "ADSL",       "USUBJID",    no_deps,       c("RSDECOD", "ATIREL"), no_deps,       adrs_scaff, NULL
)

join_adrs <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_rs, by = "USUBJID")
}

rs_vars <- c("ASEQ",   "RSSEQ",  "PARAM",  "PARAMCD","AVAL",   "AVALC",  "ADTM",
               "ADY",    "AVISIT", "AVISITN")

todo_vars <- c("ADTM","ADY",    "AVISIT")

lookup_rs <- tribble(
  ~RSCLAS,   ~RSDECOD,        ~ATIREL,
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

s_rs_avisit <- c("SCREENING", "BASELINE", "END OF INDUCTION", "FOLLOW UP")

secs_per_year <- 31556952


gen_rs_dtms <- function(n, .df, study_duration_secs = 2 * secs_per_year, ...) {
  stopifnot(all(c("TRTSDTM", "TRTEDTM")) %in% names(.df))
  sds <- study_duration_secs
  tstart <- .df$TRTSDTM
  tend <- .df$TRTEDTM
  .df$ADTM = rand_posixct(start = tstart, end = tend, max_duration_secs = sds)
  .df$ADY = ceiling(as.numeric(difftime(.df$ASTDTM, tstart, units = "days")))
  .df
}


gen_rs_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               RSSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, rsseqvars]
}


gen_rs_paramcd <- function(n, .df, ...) {
  tribble(PARAMCD = as.factor(c(rep("OVRINV", 6), "BESRSPI", "INVET")))
}


gen_rs_param <- function(n, .df, ...) {
  tribble(PARAM = as.factor(recode(
    .df$PARAMCD,
    OVRINV = "Overall Response by Investigator - by visit",
    OVRSPI = "Best Overall Response by Investigator (no confirmation required)",
    BESRSPI = "Best Confirmed Overall Response by Investigator",
    INVET = "Investigator End Of Induction Response"))
  )
}


gen_rs_rscat <- function(n, .df, ...) {
  tribble(RSCAT = .df$RSCLAS)
}


gen_rs_aval <- function(n, .df, ...) {
  param_codes <- setNames(1:5, c("CR", "PR", "SD", "PD", "NE"))
  tribble(AVAL = param_codes[.df$AVALC]) %>%
    mutate(AVAL = ifelse(.df$BMEASIFL == "N" & .df$AVISIT == "BASELINE", NA, AVAL))
}


gen_rs_avalc <- function(n, .df, ...) {
  probs <- dplyr::filter(lookup_rs, .df$ARM == as.character(.df$ACTARM))
  tribble(AVALC = c(
    sample(probs$AVALC, 1, prob = probs$p_scr) %>% as.character(),
    rsp_bsl,
    rsp_eoi,
    rsp_fu,
    names(param_codes)[best_rsp],
    rsp_eoi
  ))
}


gen_rs_avisit <- function(n, .df, ...) {
  tribble(AVISIT =
            )
}


gen_rs_avisitn <- function(n, .df, ...) {
  tribble(AVISITN = case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    .df$AVISIT == "END OF INDUCTION" ~ 999.1,
    .df$AVISIT == "FOLLOW UP" ~ 999.2,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_
  ))
}

AVISIT = factor(c(s_rs_avisit, s_rs_avisit[best_rsp_i], s_rs_avisit[5]), levels = s_rs_avisit)


rsseqvars <- c("ASEQ", "RSSEQ")
dtmvars <- c("ASTDTM", "ASTDY", "AENDTM", "AENDY")
dtmdeps <- c("TRTSDTM", "TRTEDTM")
rsatcvars <- c("ATC1", "ATC2", "ATC3", "ATC4")
rsatccdvars <- c("ATC1CD", "ATC2CD", "ATC3CD", "ATC4CD")
avaldeps <- c("AVALC", "AVISIT", "BMEASIFL")

rs_recipe <- tribble(
  ~variables,  ~dependencies, ~func,            ~func_args,                                     ~keep,
  dtmvars,     dtmdeps,       gen_rs_dtms,      list(study_duration_secs = 1 * secs_per_year),  TRUE,
  rsseqvars,   "USUBJID",     gen_rs_seq,       NULL,                                           TRUE,
  "PARAMCD",   no_deps,       gen_rs_paramcd,   NULL,                                           TRUE,
  "PARAM",     "PARAMCD",     gen_rs_param,     NULL,                                           TRUE,
  "RSCAT",     "RSCLAS",      gen_rs_rscat,     NULL,                                           TRUE,
  "AVAL",      avaldeps,      gen_rs_aval,      NULL,                                           TRUE,
  "AVISITN",   "AVISIT",      gen_rs_avisitn,   NULL,                                           TRUE,
  "AVALC",
  )

gen_reljoin_table(rs_rel_join_recipe, rs_recipe, db = list(ADSL = ADSL))

