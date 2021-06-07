

s_countries <- c("CHN", "USA", "BRA", "PAK", "NGA", "RUS", "JPN", "GBR", "CAN", "CHE")
country_site_prob <- c(.5, .121, .077, .077, .075, .052, .046, .025, .014, .003)

s_armcds <- c("ARM A", "ARM B", "ARM C")
s_armprobs <- rep(1/length(s_armcds), length(s_armcds))


s_sex <- c("F", "M", "U", "UNDIFFERENTIATED")
s_sexprobs <- c(.5, .48, .015, .005)

s_race <- c(
      "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
      "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER", "UNKNOWN"
)
s_raceprobs <- c(.55, .23, .16, .05, .004, .003, .002, .002)


##original code
## sapply(rchisq(N, df = 5, ncp = 10), max, 0) + 20,
rchisq_age <- function(n) {
    rv <- rchisq(n, df = 5, ncp = 10)
    ## this seems silly what is going on with this???
    rv[rv<0] <-  0
    rv + 20
}


s_strat1 <- c("A", "B", "C")
s_strat2 <- c("S1", "S2")

s_bmrkr2 <- c("LOW", "MEDIUM", "HIGH")





trtdtm_varnames <- c("TRTSDTM", "RANDDT", "TRTEDTM")
sample_trtdtmvars<- function(n, study_duration = 2,
                           sys_dtm = as.numeric(strptime("20/2/2019 11:16:16.683", "%d/%m/%Y %H:%M:%OS")),
                           discons = floor(n * .3)) {
    study_duration_secs <- secs_per_year * study_duration
    trtsdtm <- rand_posixct(as.POSIXct(sys_dtm, origin = pct_orig), max_duration_secs = study_duration_secs, end = NA, n = n)
    st_posixn <- as.numeric(trtsdtm)
    randdt <- as.Date(trtsdtm - floor(runif(n, min = 0, max = 5)))
    trtedtm <- as.POSIXct(st_posixn + study_duration_secs, origin = pct_orig)
    disc_inds <- sample(n, discons, replace = FALSE)
    trtedtm[disc_inds] <- as.POSIXct(sample(seq(from = max(st_posixn[disc_inds]),
                                                to = sys_dtm + study_duration_secs),
                                            size = discons), origin = pct_orig)
    ## I have no idea why the original code did this, but it did
    natrtedtm_inds <- (1:n %in% disc_inds) &
        st_posixn >= quantile(st_posixn)[2] &
        st_posixn <= quantile(st_posixn)[3]
    trtedtm[natrtedtm_inds] <- as.POSIXct(NA, origin = pct_orig)

    data.frame(TRTSDTM = trtsdtm,
               RANDDT = randdt,
               TRTEDTM = trtedtm)
}

eos_varnames <- c("EOSDT", "EOSDY", "EOSSTT", "DSCREAS")
s_discon <-  c("ADVERSE EVENT",
        "LACK OF EFFICACY",
        "PHYSICIAN DECISION",
        "PROTOCOL VIOLATION",
        "WITHDRAWAL BY PARENT/GUARDIAN",
        "WITHDRAWAL BY SUBJECT")

make_eosvars <- function(.df, n = NROW(.df)) {
    eosdt <- as.Date(.df$TRTEDTM)
    eosdy <- as.numeric(ceiling(difftime(.df$TRTEDTM, .df$TRTSDTM, units = "days")))
    eosstt <- rep("DISCONTINUED", NROW(.df))
    eosstt[which(eosdy == max(eosdy, na.rm = TRUE))] <- "COMPLETED"
    eosstt[is.na(eosdy)] <- "ONGOING"
    ## could be seperate generator block *shrug*
    dcreas <- rep(NA, n)
    disc_pos <- eosstt == "DISCONTINUED"

    dcreas <- sample_fct(s_discon, n = n)
    dcreas[!disc_pos] <- NA
    data.frame(EOSDT = eosdt,
               EOSDY = eosdy,
               EOSSTT = eosstt,
               DSCREAS = dcreas,
               stringsAsFactors = FALSE)
}

sample_siteid <- function(.df, n, x, prob) {
    raw <- sample_fct(x = x, prob = prob, n = n)
    siteid <- paste0(.df$COUNTRY, "-", raw)
    usubjid <- paste(.df$STUDYID, siteid, .df$SUBJID, sep = "-")
    ## is this a bug in the original program??? SITEID and INVID are identical except for col name
    data.frame(SITEID = siteid, INDIV = siteid, USUBJID = usubjid, stringsAsFactors = FALSE)
}

arm_varnames <- c("ARM", "ARMCD", "ACTARM", "ACTARMCD")
sample_armcd <- function(n, narms = 3, armnms = c("ARM A" = "A: Drug X", "ARM B" = "B: Placebo", "ARM C" = "C: Combination") ) {
    armcd <- sample_fct(paste("ARM", LETTERS[1:narms]), n = n)
    arm <- unname(factor(armnms[armcd], levels = armnms))
    data.frame(ARM = arm, ARMCD = armcd, ACTARM = arm, ACTARMCD = armcd)
}

usubj_deps <- c("STUDYID", "COUNTRY", "SUBJID")
usubj_vars <- c("SITEID", "INVID", "USUBJID")



#' Recipe for the ADSL dataset
#'
#' @export
#'
#'
#' @examples
#'
#' adsl_recipe
#'
#' gen_table_data(adsl_recipe)
#'
adsl_recipe <- tribble(
  ~variables,       ~dependencies,   ~func,                  ~func_args,
  "STUDYID",        no_deps,          rep_n,                 list(val = "AB12345"),
  "COUNTRY",        no_deps,          sample_fct,            list(x = s_countries, prob = country_site_prob),
  arm_varnames,     no_deps,          sample_armcd,          NULL,
  usubj_vars,       usubj_deps,       sample_siteid,         list(x = 1:20, prob = rep(country_site_prob, 2)),
  "SUBJID",         no_deps,          subjid_func,            NULL,
  "SEX",            no_deps,          sample_fct,            list(x = s_sex, prob = s_sexprobs),
  "AGE",            no_deps,          rchisq_age,            NULL,
  "RACE",           no_deps,          sample_fct,            list(x = s_race, prob = s_raceprobs),
  "STRATA1",        no_deps,          sample_fct,            list(x = s_strat1),
  "STRATA2",        no_deps,          sample_fct,            list(x = s_strat2),
  "BMRKR1",         no_deps,          rchisq,                list(df = 6),
  "BMRKR2",         no_deps,          sample_fct,            list(x = s_bmrkr2),
  "BMEASIFL",       no_deps,          sample_yn,             NULL,
  "BEP01FL",        no_deps,          sample_yn,             NULL,
  trtdtm_varnames,  no_deps,          sample_trtdtmvars,     NULL,
  eos_varnames,     trtdtm_varnames,  make_eosvars,          NULL
)
