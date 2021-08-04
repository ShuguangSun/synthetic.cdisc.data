## TODO remove some of these if they are ever not needed (and they
## should some day be not needed!)

#' @import tibble
#' @import dplyr
#' @importFrom stats quantile rchisq rexp rnorm runif
#' @importFrom utils head
#' @import respectables
NULL

#' Create visit schedule
#'
#' @description Create a visit schedule as factor.
#'
#' X number of visits or X number of cycles and Y number of days.
#'
#' @param visit_format as character string. Valid values: WEEK, CYCLE.
#' @param n_assessments number of assessments. Valid values: integer.
#' @param n_days number of days for each cycle: Valid values: integer.
#' @param add_min_max logical(1). Should the min/max post-baseline values be added.
#'
#' @return a factor of length n_assessments
#' @export
#' @examples
#' visit_schedule(visit_format = "WEeK", n_assessments = 10L)
#' visit_schedule(visit_format = "CyCLE", n_assessments = 5L, n_days = 2L)
visit_schedule <- function(visit_format = "WEEK",
                           n_assessments = 10L,
                           n_days = 5L,
                           add_min_max = TRUE) {

    ## trap invalid assessment format
  if (!(toupper(visit_format) %in% c("WEEK", "CYCLE"))) {
    message("Visit format value must either be: WEEK or CYCLE")
    return(NA)
  }

  if (toupper(visit_format) == "WEEK") {
    # numeric vector of n assessments/cycles/days
    assessments <- 1:n_assessments
    # numeric vector for ordering including screening (-1) and baseline (0) place holders
    assessments_ord <- -1:n_assessments
                                        # character vector of nominal visit values

      visit_values <- c("SCREENING", "BASELINE", paste(toupper(visit_format), assessments, "DAY", (assessments * 7) + 1))
  } else if (toupper(visit_format) == "CYCLE") {
    cycles <- sort(rep(1:n_assessments, times = 1, each = n_days))
    days <- rep(seq(1:n_days), times = n_assessments, each = 1)
    assessments_ord <- 0:(n_assessments * n_days)
    visit_values <- c("SCREENING", paste(toupper(visit_format), cycles, "DAY", days))
  }
    if(add_min_max) {
        visit_values <- c(visit_values, paste("POST-BASELINE", c("MINIMUM", "MAXIMUM")))
        assessments_ord <- c(assessments_ord, max(assessments_ord) + 1:2)
    }
    ## create and order factor variable to return from function
    visit_values <- stats::reorder(factor(visit_values), assessments_ord)
}

#' Helper functions and constants for ADEG CDISC data recipe
#' @export
#' @inheritParams gen_args
#' @rdname adeg_helpers
lookup_adeg <- tribble(
  ~PARAMCD,     ~PARAM,                ~PARAMU,      ~EGCAT,         ~mean_aval, ~sd_aval, ~ANRLO,   ~ANRHI,
  "QT",         "QT Duration",         "msec",       "INTERVAL",     350,        100,      200,      500,
  "PR",         "RR Duration",         "msec",       "INTERVAL",     1050,       300,      600,      1500,
  "HR",         "Heart Rate",          "beats/min",  "MEASUREMENT",  70,         20,       40,       100,
  "ECGINTP",    "ECG Interpretation",  "",           "FINDING",      NA_real_,   NA_real_, NA_real_, NA_real_
)

#' @export
#' @rdname adeg_helpers
adeg_varnames <- c("ASEQ", "EGSEQ", "EGTESTCD", "EGTEST", "EGCAT",   "ASPID",   "PARAM",    "PARAMCD",  "AVAL",    "AVALC",   "AVALU",
                   "BASE", "BASEC", "BASETYPE", "ABLFL",  "CHG",     "PCHG",    "DTYPE",    "ANRLO",    "ANRHI",   "ANRIND",  "BNRIND",
                   "ADTM", "ADY",   "ATPTN",    "AVISIT", "AVISITN", "ONTRTFL", "WORS01FL", "WORS02FL", "ANL01FL", "ANL03FL", "ANL04FL")

#' @export
#' @rdname adeg_helpers
s_adeg_paramcd <- c("QT", "RR", "HR", "ECGINTP")

#' @export
#' @rdname adeg_helpers
s_adeg_param <- c("QT" = "QT Duration",
                  "RR" = "RR Duration",
                  "HR" = "Heart Rate",
                  "ECGINTP" = "ECG Interpretation")

#' @export
#' @rdname adeg_helpers
s_adeg_paramu <-  c("msec", "msec", "beats/min", "")


#' @export
#' @param visit_format character(1). Format of visit timing
#' @param n_assess numeric(1). Number of assessments.
#' @param n_days numeric(1). Number of days.
#' @rdname adeg_helpers
join_paramcd_adeg <- function(n, .df, .db, visit_format = "WEEK", n_assess = 10, n_days = 5) {
    .dbtab <- .db[["ADSL"]]
    dfout <- expand.grid( #nolint
#        STUDYID = unique(.dbtab$STUDYID),
        USUBJID = .dbtab$USUBJID,
        PARAMCD = lookup_adeg$PARAMCD,
        AVISIT = visit_schedule(visit_format= visit_format, n_assessments = n_assess, n_days= n_days),
        stringsAsFactors = FALSE
    )

    dfout <- merge(.dbtab, dfout, by = "USUBJID", all.y = TRUE)

    dfout <- dfout[dfout$PARAMCD != "ECGINTP" | !grepl("POST-BASELINE", dfout$AVISIT),]
    dfout$DTYPE <- NA_character_
    dtypeinds <- grep("POST-BASELINE", dfout$AVISIT)
    dfout$DTYPE[dtypeinds] <- gsub("POST-BASELINE ", "", dfout$AVISIT[dtypeinds])
    merge(dfout, lookup_adeg, by = "PARAMCD" )
}


#' @export
#' @rdname adeg_helpers
avaldescr_sel <- c("ABNORMAL","NORMAL")


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n ignored.
#' @param .df data frame with required variable `USUBJID`
#' @export
#' @rdname adeg_helpers
#' @examples
#' x <- data.frame(USUBJID = rep(1:10, each = 2))
#'
#' gen_adeg_seq(NULL, x)
#' gen_adeg_seq(NULL, data.frame(USUBJID = c('id1', 'id1', 'id2', 'id3', 'id3', 'id3')))
#'
gen_adeg_seq <- function(n, .df, ...) {
  spl <- split(seq_along(.df$USUBJID), .df$USUBJID)
  rowgroups <- lapply(spl, function(spli) {
    data.frame(rownum = spli,
               ASEQ = seq_along(spli),
               EGSEQ = seq_along(spli))
  })
  retdf <- do.call(rbind, rowgroups)
  o <- order(retdf$rownum)
  retdf[o, c("ASEQ", "EGSEQ")]
}


#' @export
#' @rdname adeg_helpers
gen_adeg_atptn <- function(n, .df, ...) {
  tibble(ATPTN = 1)
}

#' @export
#' @rdname adeg_helpers
gen_adeg_eg <- function(n, .df, ...) {
  tibble(EGTESTCD = .df$PARAMCD,
         EGTEST = .df$PARAM
  )
}

## #' @export
## #' @rdname adeg_helpers
## gen_adeg_u <- function(n, .df, ...) {
##   tibble(USUBJID = .df$USUBJID
##   )
## }



#' @export
#' @rdname adeg_helpers
gen_adeg_aspid <- function(n, .df, ...) {
  tibble(ASPID = sample(1:n))
}

#' @export
#' @rdname adeg_helpers
gen_adeg_aval <- function(n, .df, ...) {
  tibble(AVAL = rnorm(nrow(.df), mean = .df$mean_aval, sd = .df$sd_aval)/365.25,
         AVALU = "YEARS"
  )
}



#' Primary keys: retain values
#'
#' Retain values within primary keys
#'
#' @param df data frame in which to apply the retain.
#' @param value_var variable containing the value to be retained.
#' @param event flag to trigger the retain.
#' @param outside value to.
#'
## #' @examples
## ' ADLB <- radlb(radsl(N = 10, na_percentage = 0), na_vars = list())
## ' ADLB$BASE2 <- random.cdisc.data:::retain(df = ADLB, value_var = ADLB$AVAL,
## '   event = ADLB$ABLFL2 == "Y")
retain <- function(df, value_var, event, outside = NA) {
  indices <- c(1, which(event == TRUE), nrow(df) + 1)
  values <- c(outside, value_var[event == TRUE])
  rep(values, diff(indices))
}



#' @export
#' @rdname adeg_helpers
gen_adeg_base <- function(n, .df, ...) {
  tibble(BASE = ifelse(.df$AVISITN >= 0,
                       retain(.df, .df$AVAL, .df$ABLFL == "Y"),
                       .df$AVAL),
         BASETYPE = "LAST"
  )
}

#' @export
#' @param adesc character. ADESC values to sample from
#' @param ... unused.
#' @rdname adeg_helpers
gen_adeg_adesc <- function(n, .df, adesc = avaldescr_sel, ...) {
  n <- NROW(.df)
  tibble(AVALC = ifelse(.df$PARAMCD == "ECGINTP",
                        as.character(sample_fct(adesc, n, prob = c(0.25, 0.75))),
                        as.character(.df$AVAL))
  )
}


#' @export
#' @rdname adeg_helpers
basec_deps <- c("USUBJID", "PARAMCD", "BASETYPE", "AVALC", "AVISIT", "BASE", "ANRIND", "ABLFL")
#' @export
#' @rdname adeg_helpers
basec_vars <- c("BASEC", "BNRIND")

#' @export
#' @rdname adeg_helpers
gen_adeg_basec <- function(n, .df, ...) {

    .df$row_ord <- 1:NROW(.df)
    spl <- split(.df[,c("row_ord", basec_deps)], as.list(.df[,c("USUBJID", "PARAMCD", "BASETYPE")]))

    rowchunks <- lapply(spl, function(dat) {
        pcd <- dat$PARAMCD[1]
        dat$BASEC <- if(pcd == "ECGINTP") dat$AVALC[dat$AVISIT == "BASELINE"] else as.character(dat$BASE)
        dat$BNRIND <- factor(dat$ANRIND[dat$ABLFL == "Y"], levels = c("LOW", "NORMAL", "HIGH"))
        dat
    })

    fulldat <- do.call(rbind, rowchunks)
    o <- order(fulldat$row_ord)
    fulldat[o, basec_vars]
}

#' @export
#' @rdname adeg_helpers
gen_adeg_chg <- function(n, .df, ...) {
  tibble(CHG = ifelse(.df$AVISITN > 0, .df$AVAL - .df$BASE, NA)
  )
}

#' @export
#' @rdname adeg_helpers
gen_adeg_pchg <- function(n, .df, ...) {
  tibble(PCHG = ifelse(.df$AVISITN > 0, 100 * (.df$CHG / .df$BASE), NA)
  )
}

#' @export
#' @rdname adeg_helpers
gen_adeg_anrind <- function(n, .df, ...) {
  tibble(ANRIND = factor(case_when(
    .df$AVAL < .df$ANRLO ~ "LOW",
    .df$AVAL >= .df$ANRLO & .df$AVAL <= .df$ANRHI ~ "NORMAL",
    .df$AVAL > .df$ANRHI ~ "HIGH"),
    levels = c("LOW", "NORMAL", "HIGH"))
  )
}

#' @export
#' @rdname adeg_helpers
gen_adeg_ablfl <- function(n, .df, ...) {
    ifelse(.df$AVISIT %in% c("BASELINE", "CYCLE 1 DAY 1"), "Y", "")
}

## #' @export
## #' @rdname adeg_helpers
## gen_adeg_anl01fl <- function(n, .df, ...) {
##     factor(ifelse(
##     (.df$ABLFL == "Y" |  (is.na(.df$DTYPE) & .df$WORS01FL == "Y"))
##     & (.df$AVISIT != "SCREENING"),
##     "Y",
##     "")
##   )
## }

## #' @export
## #' @rdname adeg_helpers
## gen_adeg_anl03fl <- function(n, .df, ...) {
##   tibble(ANL03FL = case_when(
##     .df$DTYPE == "MINIMUM" ~ "Y",
##     .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
##     TRUE ~ "")
##   )
## }

## #' @export
## #' @rdname adeg_helpers
## gen_adeg_anl04fl <- function(n, .df, ...) {
##   tibble(ANL04FL = case_when(
##     .df$DTYPE == "MAXIMUM" ~ "Y",
##     .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
##     TRUE ~ "")
##   )
## }

#' @export
#' @rdname adeg_helpers
gen_adeg_onfl <- function(n, .df, ...) {
  tibble(ONTRTFL = factor(case_when(
    is.na(.df$TRTSDTM) ~ "",
    is.na(.df$ADTM) ~ "Y",
    (.df$ADTM < .df$TRTSDTM) ~ "",
    (.df$ADTM > .df$TRTEDTM) ~ "",
    TRUE ~ "Y"))
  )
}

#' @export
#' @rdname adeg_helpers
gen_adeg_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_)
  )
}



#' @export
#' @param data internal detail.
#' @param worst_obs internal detail.
#' @rdname adeg_helpers
flag_variables_raw <- function(data, worst_obs) {

    data_compare <- data %>% # nolint
        dplyr::mutate(row_check = seq_len(nrow(data)))

    data <- data_compare %>%
      { # nolint
          if (worst_obs == FALSE) {
              dplyr::group_by(., .data$USUBJID, .data$PARAMCD, .data$BASETYPE, .data$AVISIT) %>%
                  dplyr::arrange(., .data$ADTM, .data$ASPID, .data$EGSEQ)
          } else {
              dplyr::group_by(., .data$USUBJID, .data$PARAMCD, .data$BASETYPE)
          }
      } %>%
        dplyr::filter(
                   .data$AVISITN > 0 & (.data$ONTRTFL == "Y" | .data$ADTM <= .data$TRTSDTM) &
                   is.na(.data$DTYPE)
               ) %>%
      { # nolint
          if (worst_obs == TRUE) dplyr::arrange(., .data$AVALC) %>% dplyr::filter(., ifelse(
                                                                                         .data$PARAMCD == "ECGINTP",
                                                                                     ifelse(.data$AVALC == "ABNORMAL", .data$AVALC == "ABNORMAL", .data$AVALC == "NORMAL"),
                                                                                     .data$AVAL == min(.data$AVAL)))
          else dplyr::filter(., ifelse(
                                    .data$PARAMCD == "ECGINTP",
                                    .data$AVALC == "ABNORMAL" | .data$AVALC == "NORMAL",
                                    .data$AVAL == min(.data$AVAL)))
      } %>%
      dplyr::slice(1) %>%
      { # nolint
          if (worst_obs == TRUE)
              dplyr::mutate(., new_var = dplyr::case_when(
                                                (.data$AVALC == "ABNORMAL" | .data$AVALC == "NORMAL") ~ "Y",
                                                (!is.na(.data$AVAL) & is.na(.data$DTYPE)) ~ "Y",
            TRUE ~ ""
            ))
          else
              dplyr::mutate(., new_var = dplyr::case_when(
                                                (.data$AVALC == "ABNORMAL" | .data$AVALC == "NORMAL") ~ "Y",
                                                (!is.na(.data$AVAL) & is.na(.data$DTYPE)) ~ "Y",
                                                TRUE ~ ""
                                                ))
      } %>%
      dplyr::ungroup()

    varname <- if(worst_obs) "WORS02FL" else "WORS01FL"
    data_compare[[varname]] <- ifelse(data_compare$row_check %in% data$row_check, "Y", "")

    data_compare <- data_compare[, -which(names(data_compare) %in% c("row_check"))]

    return(data_compare)

}

#' @export
#' @rdname adeg_helpers
adeg_worsvars <- c("WORS01FL", "WORS02FL")
#' @export
#' @rdname adeg_helpers
adeg_worsvar_deps <- c("USUBJID", "PARAMCD", "BASETYPE", "AVISITN", "ONTRTFL",
                       "ADTM", "TRTSDTM", "DTYPE", "AVAL", "AVALC")
#' @export
#' @rdname adeg_helpers
gen_adeg_worsfl <- function(n, .df, ...) {
    .df <- flag_variables_raw(.df, FALSE)
    .df <- flag_variables_raw(.df, TRUE)

    .df[, adeg_worsvars]
}











secs_per_year <- 31557600

#' @export
#' @param study_duration numeric(1). study duration in years.
#' @rdname adeg_helpers
gen_adeg_adtm <- function(n, .df, study_duration = 2, ...) {
    study_duration_secs <- secs_per_year * study_duration
    strt <- .df$TRTSDTM
    ends <- .df$TRTEDTM
    rand_posixct(strt, ends, max_duration_secs = study_duration_secs)
}

#' @export
#' @rdname adeg_helpers
gen_adeg_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}


#' @export
#' @rdname adeg_helpers
anlflagvars <- c("ANL01FL", "ANL03FL", "ANL04FL")
#' @export
#' @rdname adeg_helpers
anlflagdeps <- c("PARAMCD", "AVISIT",  "DTYPE", "ABLFL", "WORS01FL")

#' @export
#' @rdname adeg_helpers
gen_adeg_anlfls <- function(n, .df, ...) {

    .df$ANL01FL <- ""
    .df$ANL03FL <- ""
    .df$ANL04FL <- ""

    ablfl_y <- .df$ABLFL == "Y"
    condlgl <- (.df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP")
    non_scrn <- .df$AVISIT != "SCREENING"
    dtyp <- .df$DTYPE
    fl1inds <- non_scrn & (ablfl_y | (is.na(dtyp) & .df$WORS01FL == "Y"))
    .df$ANL01FL[fl1inds] <- "Y"
    fl3inds <- dtyp == "MINIMUM" | condlgl
    .df$ANL03FL[fl3inds] <- "Y"
    fl4inds <- dtyp == "MAXIMUM" | condlgl
    .df$ANL04FL[fl4inds] <- "Y"

    .df[, anlflagvars]

}


#' Recipes for creating ADEG CDISC Data
#'
#' @rdname cdisc_recs
#' @export
#'

adae_scaff <- rand_per_key("USUBJID", tblnm = "ADSL", mincount = 0, maxcount = 10, prop_present = 1)

#' @rdname cdisc_recs
#' @export
adeg_scaff_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables, ~dependencies, ~func, ~func_args,
  "ADSL", "USUBJID", "PARAMCD", c("PARAM", "PARAMU", "mean_aval", "sd_aval", "ANRLO", "ANRHI"), no_deps, join_paramcd_adeg, NULL) ##adae_scaff, NULL)
#' @rdname cdisc_recs
#' @export
adeg_tbl_recipe <- tribble(
  ~variables,                          ~dependencies,                                         ~func,                 ~func_args,
  c("ASEQ", "EGSEQ"),                  "USUBJID",                                             gen_adeg_seq,          NULL,
  c("EGTESTCD", "EGTEST"),             c("PARAM", "PARAMCD"),                                 gen_adeg_eg,           NULL,
  "ATPTN",                             no_deps,                                               gen_adeg_atptn,        NULL,
  "ASPID",                             no_deps,                                               gen_adeg_aspid,        NULL,
  c("AVAL", "AVALU"),                  c("mean_aval", "sd_aval"),                             gen_adeg_aval,         NULL,
  c("BASE", "BASETYPE"),               c("AVAL", "AVISITN", "ABLFL"),                         gen_adeg_base,         NULL,
  "AVALC",                             c("PARAMCD", "AVAL"),                                  gen_adeg_adesc,        NULL,
  basec_vars,                          basec_deps,                                            gen_adeg_basec,        NULL,
  "CHG",                               c("AVISITN", "AVAL", "BASE"),                          gen_adeg_chg,          NULL,
  "PCHG",                              c("AVISITN", "CHG", "BASE"),                           gen_adeg_pchg,         NULL,
  "ANRIND",                            c("AVAL", "ANRLO", "ANRHI"),                           gen_adeg_anrind,       NULL,
###  "BNRIND",                            c("ANRIND", "ABLFL"),                                  gen_adeg_bnrind,       NULL,
  "ABLFL",                             "AVISIT",                                              gen_adeg_ablfl,        NULL,
  ##"ANL01FL",                           c("ABLFL", "DTYPE", "WORS01FL", "AVISIT"),             gen_adeg_anl01fl,      NULL,
  anlflagvars,                         anlflagdeps,                                           gen_adeg_anlfls,       NULL,
  #                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_adeg_anl04fl,      NULL,
  "ONTRTFL",                           c("TRTSDTM", "TRTEDTM", "ADTM"),                       gen_adeg_onfl,         NULL,
  "AVISITN",                           "AVISIT",                                              gen_adeg_avisitn,      NULL,
  c("WORS01FL", "WORS02FL"),           adeg_worsvar_deps,                                     gen_adeg_worsfl,       NULL,
  "ADTM",                              c("TRTSDTM", "TRTEDTM"),                               gen_adeg_adtm,         list(study_duration = 2),
  "ADY",                               c("ADTM", "TRTSDTM"),                                  gen_adeg_ady,          NULL
)

