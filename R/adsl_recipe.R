

                                        #
#' Helper functions and constants for ADSL
#'
#' @rdname adsl_helpers
#' @export
s_countries <- c("CHN", "USA", "BRA", "PAK", "NGA", "RUS", "JPN", "GBR", "CAN", "CHE")
#' @rdname adsl_helpers
#' @export
country_site_prob <- c(.5, .121, .077, .077, .075, .052, .046, .025, .014, .003)

#' @rdname adsl_helpers
#' @export
s_armcds <- c("ARM A", "ARM B", "ARM C")
#' @rdname adsl_helpers
#' @export
s_armprobs <- rep(1/length(s_armcds), length(s_armcds))


#' @rdname adsl_helpers
#' @export
s_sex <- c("F", "M", "U", "UNDIFFERENTIATED")
#' @rdname adsl_helpers
#' @export
s_sexprobs <- c(.5, .48, .015, .005)

#' @rdname adsl_helpers
#' @export
s_race <- c(
      "ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
      "MULTIPLE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "OTHER", "UNKNOWN"
)
#' @rdname adsl_helpers
#' @export
s_raceprobs <- c(.55, .23, .16, .05, .004, .003, .002, .002)


##original code
## sapply(rchisq(N, df = 5, ncp = 10), max, 0) + 20,
#' @rdname adsl_helpers
#' @export
rchisq_age <- function(n) {
    rv <- rchisq(n, df = 5, ncp = 10)
    ## this seems silly what is going on with this???
    rv[rv<0] <-  0
    rv + 20
}


#' @rdname adsl_helpers
#' @export
s_strat1 <- c("A", "B", "C")
#' @rdname adsl_helpers
#' @export
s_strat2 <- c("S1", "S2")

#' @rdname adsl_helpers
#' @export
s_bmrkr2 <- c("LOW", "MEDIUM", "HIGH")





#' @rdname adsl_helpers
#' @export
trtdtm_varnames <- c("TRTSDTM", "RANDDT", "TRTEDTM")

#' @rdname adsl_helpers
#' @export
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

#' @rdname adsl_helpers
#' @export
eos_varnames <- c("EOSDT", "EOSDY", "EOSSTT", "DCSREAS")
#' @rdname adsl_helpers
#' @export
s_discon <-  c("ADVERSE EVENT",
        "LACK OF EFFICACY",
        "PHYSICIAN DECISION",
        "PROTOCOL VIOLATION",
        "WITHDRAWAL BY PARENT/GUARDIAN",
        "WITHDRAWAL BY SUBJECT")

#' @rdname adsl_helpers
#' @export
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
               DCSREAS = dcreas,
               stringsAsFactors = FALSE)
}

#' @rdname adsl_helpers
#' @export
sample_siteid <- function(.df, n, x, prob) {
    raw <- sample_fct(x = x, prob = prob, n = n)
    siteid <- paste0(.df$COUNTRY, "-", raw)
    usubjid <- paste(.df$STUDYID, siteid, .df$SUBJID, sep = "-")
    ## is this a bug in the original program??? SITEID and INVID are identical except for col name
    data.frame(SITEID = siteid, INDIV = siteid, USUBJID = usubjid, stringsAsFactors = FALSE)
}

#' @rdname adsl_helpers
#' @export
arm_varnames <- c("ARM", "ARMCD", "ACTARM", "ACTARMCD")
#' @rdname adsl_helpers
#' @export
sample_armcd <- function(n, narms = 3, armnms = c("ARM A" = "A: Drug X", "ARM B" = "B: Placebo", "ARM C" = "C: Combination") ) {
    armcd <- sample_fct(paste("ARM", LETTERS[1:narms]), n = n)
    arm <- unname(factor(armnms[armcd], levels = armnms))
    data.frame(ARM = arm, ARMCD = armcd, ACTARM = arm, ACTARMCD = armcd)
}


#' @rdname adsl_helpers
#' @export
lup_dcreas <- data.frame(stringsAsFactors = FALSE,
    choices = c(
      "ADVERSE EVENT", "DEATH", "LACK OF EFFICACY", "PHYSICIAN DECISION",
      "PROTOCOL VIOLATION", "WITHDRAWAL BY PARENT/GUARDIAN", "WITHDRAWAL BY SUBJECT"
    ),
    prob = c(.2, 1, .1, .1, .2, .1, .1)
)

#' @rdname adsl_helpers
#' @export
lup_dthother <- data.frame(stringsAsFactors = FALSE,
                      choices = c(
                          "Post-study reporting of death",
                          "LOST TO FOLLOW UP",
                          "MISSING",
                          "SUICIDE",
                          "UNKNOWN"
                      ),
                      prob = c(.1, .3, .3, .2, .1)
)



## #' @rdname adsl_helpers
## #' @export
## adsl_gen_dcsreas <- function(n, .df, dcreas_lup) {
##     .df$DCSREAS <- NA_character_
##     inds <- which(.df$EOSSTT == "DISCONTINUED")
##     .df$DCSREAS[inds] <- sample(dcreas_lup$choices,
##                                 length(inds),
##                                 replace = TRUE,
##                                 prob = dcreas_lup$prop)
##     .df[, "DCSREAS", drop = TRUE]
## }


#' @rdname adsl_helpers
#' @export
dthvarclasses <- c(DTHFL = NA_character_,
                   DTHCAT = NA_character_,
                   DTHCAUS = NA_character_,
                   LDDTHELD = "integer",
                   LDDTHGR1 = NA_character_,
                   DTHDT = NA_character_,
                   LSTALVDT = NA_character_)
#' @rdname adsl_helpers
#' @export
dth_varnames <- names(dthvarclasses)
dth_deps <- c("DCSREAS", "TRTEDTM")

adsl_gen_dthvars <- function(n = NROW(.df), .df, dth_lup) {
    ret <- init_new_cols(n, colnames = head(dth_varnames, 5),
                         colclasses = head(dthvarclasses, 5)) ## dates currently not supported

    ret$DTHFL <- ifelse(is.na(.df$DCSREAS) | .df$DCSREAS != "DEATH", "N", "Y")
    inds <- which(ret$DTHFL == "Y")
    ret$DTHCAT[inds] <- sample(c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER"),
                               replace = TRUE,
                               size = length(inds))
    ret$DTHCAUS[inds] <- ret$DTHCAT[inds]
    othinds <- which(ret$DTHCAT == "OTHER")
    ret$DTHCAUS[othinds] <- sample(dth_lup$choices,
                                   length(othinds),
                                   replace = TRUE,
                                   prob = dth_lup$prob)


    ret$LDDTHELD[inds] <- sample(0:50, length(inds), replace = TRUE)
    ret$LDDTHGR1[inds] <- ifelse(ret$LDDTHELD[inds] <= 30, "<=30", ">30")
    ## NAs handled by addition here
    ret$DTHDT <-  .df$TRTEDTM + ret$LDDTHELD*secs_per_day
    ret$LSTALVDT <- ret$DTHDT
    ret$LSTALVDT[-inds] <- rand_posixct(as.POSIXct(.df$TRTEDTM[-inds] + 10 * secs_per_day),
                                        as.POSIXct(.df$TRTEDTM[-inds] + 30 * secs_per_day))
    ret
}








#' @rdname adsl_helpers
#' @export
usubj_deps <- c("STUDYID", "COUNTRY", "SUBJID")
#' @rdname adsl_helpers
#' @export
usubj_vars <- c("SITEID", "INVID", "USUBJID")







#' Recipe for the ADSL dataset
#'
#' Subject Level Dataset
#'
#' @details This particular recipe implements a simple generic ADSL dataset.
#'
#'
#' @export
#' @rdname cdisc_recs
#'
#' @examples
#'
#' adsl_recipe
#'
#' adsl <- gen_table_data(N = 10, adsl_recipe)
#'
adsl_tbl_recipe <- tribble(
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
  eos_varnames,     trtdtm_varnames,  make_eosvars,          NULL,
#  "DCSREAS",        "EOSSTT",         adsl_gen_dcsreas,       list(dcreas_lup = lup_dcreas),
  dth_varnames,     dth_deps,         adsl_gen_dthvars,      list(dth_lup = lup_dthother),
  "study_duration_secs", no_deps, function(n, study_duration) study_duration*secs_per_year, list(study_duration = 2)

)
