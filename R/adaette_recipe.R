
#' ADAETTE recipe helper constants and functions
#' @export
#' @rdname adaette_helpers
#' @inheritParams gen_args
adaette_fdeps <- c("STUDYID", "TRTSDTM", "SITEID", "ARM", "ASTDTM")

#' @export
#' @rdname adaette_helpers
adaette_sjvars <- c("PARAM", "PARAMCD", "CATCD", "LAMBDA", "CNSR_P")

#' @export
#' @rdname adaette_helpers
adaette_paramcds <- tribble(
    ~PARAMCD, ~PARAM, ~AVALU,
    "AEREPTTE", "Time to end of AE reporting period", "YEARS",
    "AETOT1", "Number of occurances of any adverse event", NA_character_,
    "AETOT2", "Number of occurances of any serious adverse event", NA_character_,
    "AETOT3", "Number of occurances of a grade 3-5 adverse event", NA_character_,
    "AETTE1", "Time to first occurance of any adverse event", "YEARS",
    "AETTE2", "Time to first occurance of any serious adverse event", "YEARS",
    "AETTE3", "Time to first occurance of a grade 3-5 adverse event", "YEARS")


#' @export
#' @rdname adaette_helpers
#' @param usubjid character(1). USUBJID
#' @param aereptte numeric(1). Time to end of AE reporting in years.
#' @param adtm POSIXct(1). Analysis datetime
#' @param ady numeric(1). Relative day number of analysis time
fullcensor <- function(usubjid, aereptte, adtm, ady ) {

    avals <- c(aereptte, 0, 0, 0, aereptte, aereptte, aereptte)

    cbind.data.frame(USUBJID = usubjid,
                     adaette_paramcds,
                     AVAL = avals,
                     CNSR = rep(c(0, NA, 1), times = c(1, 3, 3)),
                     ADTM = adtm,
                     ADY = ady)
}

#' @rdname adaette_helpers
#' @param pat_info data.frame. ADAE data for a single subjid
#' @param s_dur_secs numeric(1). Total study duration in seconds
#' @export
adaette_one_pat <- function(pat_info, s_dur_secs = pat_info$study_duration_secs) {

    trtedtm <- pat_info$TRTEDTM[1]
    trtsdtm <- pat_info$TRTSDTM[1]
    eosdt <- as.POSIXct(pat_info$EOSDT[1])

    if(is.na(trtedtm))
        trtedtm <- trtsdtm + s_dur_secs
    compl <- (!is.na(eosdt) && !is.na(trtedtm)) && eosdt < trtedtm

    adtm <- if(compl) eosdt else trtedtm
    ady <- as.double(adtm - trtsdtm, units = "days") + 1


    if(is.na(eosdt)) {
        eosdt <- pat_info$AENDTM[1]
    }

    if(is.na(eosdt)) {
        ##   eosdt <- as.POSIXct(floor(as.numeric(as.Date(trtsdtm)) +
        eosdt <- as.POSIXct(floor((as.numeric(trtsdtm) +
                                   pat_info$study_duration_secs) / 86400),
                            origin = pct_orig)
    }

    usubjid = pat_info$USUBJID[1]



    aereptte <- as.double(adtm - trtsdtm + 1, unit = "days")/365.25
    if(all(is.na(pat_info$AETOXGR)))
        return(fullcensor(usubjid, aereptte, adtm, ady))

    aetot1 <- sum(!is.na(pat_info$AETOXGR), na.rm = TRUE)
    aetot2 <- sum(pat_info$AESEV == "Y", na.rm = TRUE)
    aetot3 <- sum(as.numeric(as.character(pat_info$AETOXGR)) >= 3, na.rm=TRUE)
    aette1 <- (pat_info$ASTDTM[which.min(pat_info$ASTDTM)] - trtsdtm + 1)/ 365.25

    sevdat <- subset(pat_info, AESEV == "Y")
    if(NROW(sevdat)) {
        aette2 <- (sevdat$ASTDTM[which.min(sevdat$ASTDTM)] - trtsdtm + 1)/ 365.25
    } else {
        aette2 <- aereptte
    }

    gr3plus <- subset(pat_info, as.numeric(as.character(AETOXGR)) >=3)
    if(NROW(gr3plus)) {
        aette3 <- (gr3plus$ASTDTM[which.min(gr3plus$ASTDTM)] - trtsdtm + 1)/ 365.25
    } else {
        aette3 <- aereptte
    }

    cnsred <- ifelse(c(aetot1, aetot2, aetot3) == 0, 1, 0)
    aereptte_evdsc <- if(compl) "Completion or Discontinuation" else "End of AE Reporting Period"
    cdescvec <- rep(NA_character_, length(cnsred))
    if(any(cnsred == 1)) {
        cnsrdtdesc <- sample(c("Last Post-Baseline ALT or AST Result", "Treatment Start"),
                             prob = c(.9, .1),
                             size = 1)
        cdescvec[cnsred==1] <- cnsrdtdesc
    } else {
        cnsrdtdesc <- NA_character_
    }


    ret <- cbind.data.frame(USUBJID = usubjid,
                            adaette_paramcds,
                            AVAL = c(aereptte, aetot1, aetot2, aetot3, aette1, aette2, aette3),
                            CNSR = c(0, rep(NA, 3), cnsred),
                            ADTM = adtm,
                            ADY = ady)
                            ## CNSDTDSC = c(rep(NA_character_, 4),
                            ##              ifelse(cnsred ==1, "", NA_character_)),
                            ## EVNTDESC = c(aereptte_evdsc,
                            ##              rep(NA_character_, 3),
                            ##              ifelse(cnsred ==0, "", NA_character_)))
    ret
}


#' @rdname adaette_helpers
#' @export
adaette_sjf <- function(.db, lookup = adaette_paramcds) {
    ADSL <- .db[["ADSL"]]
    ADAE <- .db[["ADAE"]]
    tmp <- merge(ADSL, ADAE[,c("USUBJID", setdiff(names(ADAE), names(ADSL)))], by = "USUBJID", all.x = TRUE)


    spl <- split(tmp, tmp$USUBJID)

    rws <- lapply(spl, adaette_one_pat, s_dur_secs = ADSL$study_duration_secs[1])

    ret <- do.call(rbind, rws)
    ret <- merge(ret, ADSL, by = "USUBJID")
    ret
}


#' @rdname cdisc_recs
#' @export
adaette_scaff_recipe <- tribble(
    ~foreign_tbls, ~foreign_keys, ~func, ~func_args,
    c("ADSL", "ADAE"), c(ADSL="USUBJID", ADAE="USUBJID"), adaette_sjf, list(lookup = adaette_paramcds))

#' @rdname adaette_helpers
#' @export
evntdescr_sel <-  c(
    "Preferred Term"
)

#' @rdname adaette_helpers
#' @export
cnsdtdscr_sel <- c(
    "Clinical Cut Off",
    "Completion or Discontinuation",
    "End of AE Reporting Period"
)

#' @rdname adaette_helpers
#' @param evnt_sel character. Event descriptions to sample from.
#' @param cns_sel character. Reasons for censoring to sample from.
#' @export
adaette_cnsrdtdsc <- function(n, .df, evnt_sel = evntdescr_sel, cns_sel = cnsdtdscr_sel ) {

    unqids <- unique(.df$USUBJID)
    tomrg <- expand.grid(USUBJID = unqids,
                         CNSR = c(0, 1, NA))
    tomrg$EVNTDESC <- NA_character_
    tomrg$CNSDTDSC <- NA_character_
    cns <- which(tomrg$CNSR == 1)
    evt <- which(tomrg$CNSR == 0)
    tomrg$EVNTDESC[evt] <- sample(evnt_sel, length(evt), replace = TRUE)
    tomrg$EVNTDESC[cns] <- ""
    tomrg$CNSDTDSC[evt] <-  ""
    tomrg$CNSDTDSC[cns] <- sample(cns_sel, length(cns), replace = TRUE)

    ret <- merge(.df, tomrg, by = c("USUBJID", "CNSR"), sort = FALSE)
    ret[,c("CNSDTDSC", "EVNTDESC")]
    ##                     EVNTDESC = c("", sample(evnt_sel, 1)),


    ## ncns <- length(cns)
    ## ret_evt <- rep("", NROW(.df))
    ## ret_cns <- ret_evt


    ## ret_evt[-cns] <- sample(evnt_sel, size = NROW(.df) - ncns, replace = TRUE)
    ## ret_cns[cns] <- sample(cns_sel, size = ncns, replace = TRUE)

    ## data.frame(CNSDTDSC = ret_cns,
    ##            EVNTDESC = ret_evt,
    ##            stringsAsFactors = FALSE)
}


#' @rdname cdisc_recs
#' @export
#' @examples
#'
adaette_tbl_recipe <- tribble(
    ~variables, ~dependencies, ~func, ~func_args, ~keep,
    c("CNSDTDSC", "EVNTDESC"), "CNSR", adaette_cnsrdtdsc, NULL, TRUE)
