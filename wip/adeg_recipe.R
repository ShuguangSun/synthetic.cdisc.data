
lookup_adeg <- tribble(
  ~PARAMCD,     ~PARAM,                ~PARAMU,      ~EGCAT,         ~mean_aval, ~sd_aval, ~ANRLO,   ~ANRHI,
  "QT",         "QT Duration",         "msec",       "INTERVAL",     350,        100,      200,      500,
  "PR",         "RR Duration",         "msec",       "INTERVAL",     1050,       300,      600,      1500,
  "HR",         "Heart Rate",          "beats/min",  "MEASUREMENT",  70,         20,       40,       100,
  "ECGINTP",    "ECG Interpretation",  "",           "FINDING",      NA_real_,   NA_real_, NA_real_, NA_real_
)

#' Helper functions and constants for ADEG
#'

adeg_varnames <- c("ASEQ", "EGSEQ", "EGTESTCD", "EGTEST", "EGCAT",   "ASPID",   "PARAM",    "PARAMCD",  "AVAL",    "AVALC",   "AVALU",
                   "BASE", "BASEC", "BASETYPE", "ABLFL",  "CHG",     "PCHG",    "DTYPE",    "ANRLO",    "ANRHI",   "ANRIND",  "BNRIND",
                   "ADTM", "ADY",   "ATPTN",    "AVISIT", "AVISITN", "ONTRTFL", "WORS01FL", "WORS02FL", "ANL01FL", "ANL03FL", "ANL04FL")

s_adeg_paramcd <- c("QT", "RR", "HR", "ECGINTP")

s_adeg_param <- c("QT" = "QT Duration",
                  "RR" = "RR Duration",
                  "HR" = "Heart Rate",
                  "ECGINTP" = "ECG Interpretation")

s_adeg_paramu <-  c("msec", "msec", "beats/min", "")


join_paramcd_adeg <- function(n, .df, .dbtab) {
  merge(.dbtab, lookup_adeg, by = "USUBJID")
}


avaldescr_sel <- c("ABNORMAL","NORMAL")


#' Generate Sequence Per Number of USUBJID Observation
#'
#' @param n
#' @param .df data frame with required variable `USUBJID`
#'
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

gen_adeg_atptn <- function(n, .df, ...) {
  tibble(ATPTN = 1)
}

gen_adeg_eg <- function(n, .df, ...) {
  tibble(EGTESTCD = .df$PARAMCD,
         EGTEST = .df$PARAM
  )
}

gen_adeg_u <- function(n, .df, ...) {
  tibble(USUBJID = .df$USUBJID
  )
}

# gen_adeg_dtype_avisit <- function(n, minimum, .df, ...) {
#   tibble(DTYPE = NA,
#          AVISIT = random.cdisc.data:::visit_schedule(visit_format = visit_format, n_assessments = n_assessments, n_days = n_days))
#   .df %>%
#     filter(
#       (AVISIT != "BASELINE" & AVISIT != "SCREENING")
#       & (.df$ONTRTFL == "Y" | .df$ADTM <= .df$TRTSDTM)
#     ) %>%
#     {if (minimum == TRUE) filter(., .df$AVAL == min(.df$AVAL)) %>%
#         mutate(., DTYPE = "MINIMUM", AVISIT = "POST-BASELINE MINIMUM")
#      else filter(., .df$AVAL == max(.df$AVAL)) %>%
#         mutate(., DTYPE = "MAXIMUM", AVISIT = "POST-BASELINE MAXIMUM")
#     }
# }


gen_adeg_aspid <- function(n, .df, ...) {
  tibble(ASPID = sample(1:n()))
}

gen_adeg_aval <- function(n, .df, ...) {
  tibble(AVAL = rnorm(nrow(.df), mean = .df$mean_aval, sd = .df$sd_aval)/365.25,
         AVALU = "YEARS"
  )
}

gen_adeg_base <- function(n, .df, ...) {
  tibble(BASE = ifelse(.df$AVISITN >= 0,
                       retain(.df, .df$AVAL, .df$ABLFL == "Y"),
                       .df$AVAL),
         BASETYPE = "LAST"
  )
}

gen_adeg_adesc <- function(n, .df, adesc = avaldescr_sel, ...) {
  n <- NROW(.df)
  tibble(AVALC = ifelse(.df$PARAMCD == "ECGINTP",
                        as.character(sample_fct(adesc, n, prob = c(0.25, 0.75))),
                        as.character(.df$AVAL))
  )
}

gen_adeg_bdesc <- function(n, .df, ...) {
  n <- NROW(.df)
  tibble(BASEC = ifelse(.df$PARAMCD == "ECGINTP",
                        AVALC[.df$AVISIT == "BASELINE"],
                        as.character(.df$BASE))
  )
}

gen_adeg_chg <- function(n, .df, ...) {
  tibble(CHG = ifelse(.df$AVISITN > 0, .df$AVAL - .df$BASE, NA)
  )
}

gen_adeg_pchg <- function(n, .df, ...) {
  tibble(PCHG = ifelse(.df$AVISITN > 0, 100 * (.df$CHG / .df$BASE), NA)
  )
}

gen_adeg_anrind <- function(n, .df, ...) {
  tibble(ANRIND = factor(case_when(
    .df$AVAL < .df$ANRLO ~ "LOW",
    .df$AVAL >= .df$ANRLO & .df$AVAL <= .df$ANRHI ~ "NORMAL",
    .df$AVAL > .df$ANRHI ~ "HIGH"),
    levels = c("LOW", "NORMAL", "HIGH"))
  )
}

gen_adeg_bnrind <- function(n, .df, ...) {
  tibble(BNRIND = factor(.df$ANRIND[.df$ABLFL == "Y"],
                         levels = c("LOW", "NORMAL", "HIGH"))
  )
}

# gen_adeg_ablfl <- function(n, .df, ...) {
#   tibble(ABLFL = ifelse(toupper(visit_format) == "WEEK" & .df$AVISIT == "BASELINE",
#                         "Y",
#                         ifelse(toupper(visit_format) == "CYCLE" & .df$AVISIT == "CYCLE 1 DAY 1",
#                                "Y",
#                                "")
#                         )
#   )
# }

gen_adeg_anl01fl <- function(n, .df, ...) {
  tibble(ANL01FL = factor(ifelse(
    (.df$ABLFL == "Y" |  (is.na(.df$DTYPE) & .df$WORS01FL == "Y"))
    & (.df$AVISIT != "SCREENING"),
    "Y",
    ""))
  )
}

gen_adeg_anl03fl <- function(n, .df, ...) {
  tibble(ANL03FL = case_when(
    .df$DTYPE == "MINIMUM" ~ "Y",
    .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
    TRUE ~ "")
  )
}

gen_adeg_anl04fl <- function(n, .df, ...) {
  tibble(ANL04FL = case_when(
    .df$DTYPE == "MAXIMUM" ~ "Y",
    .df$ABLFL == "Y" & .df$PARAMCD != "ECGINTP" ~ "Y",
    TRUE ~ "")
  )
}

gen_adeg_onfl <- function(n, .df, ...) {
  tibble(ONTRTFL = factor(case_when(
    is.na(.df$TRTSDTM) ~ "",
    is.na(.df$ADTM) ~ "Y",
    (.df$ADTM < .df$TRTSDTM) ~ "",
    (.df$ADTM > .df$TRTEDTM) ~ "",
    TRUE ~ "Y"))
  )
}

gen_adeg_avisitn <- function(n, .df, ...) {
  tibble(AVISITN = case_when(
    .df$AVISIT == "SCREENING" ~ -1,
    .df$AVISIT == "BASELINE" ~ 0,
    (grepl("^WEEK", .df$AVISIT) | grepl("^CYCLE", .df$AVISIT)) ~ as.numeric(.df$AVISIT) - 2,
    TRUE ~ NA_real_)
  )
}

# #Created function to easily match rows which comply to ONTRTFL derivation
# flag_variables <- function(data, worst_obs) {
#
#   data_compare <- data %>% # nolint
#     mutate(row_check = seq_len(nrow(data)))
#
#   data <- data_compare %>%
#     { # nolint
#       if (worst_obs == FALSE) group_by(., .df$USUBJID, .df$PARAMCD, .df$BASETYPE, .df$AVISIT) %>%
#         arrange(., .df$ADTM, .df$ASPID, .df$EGSEQ)
#       else group_by(., .df$USUBJID, .df$PARAMCD, .df$BASETYPE)
#     } %>%
#     filter(.df$AVISITN > 0 & (.df$ONTRTFL == "Y" | .df$ADTM <= .df$TRTSDTM) & is.na(.df$DTYPE)) %>%
#     { # nolint
#       if (worst_obs == TRUE) arrange(., .df$AVALC) %>% filter(., ifelse(
#         .df$PARAMCD == "ECGINTP",
#         ifelse(.df$AVALC == "ABNORMAL", .df$AVALC == "ABNORMAL", .df$AVALC == "NORMAL"),
#         .df$AVAL == min(.df$AVAL)))
#       else filter(., ifelse(
#         .df$PARAMCD == "ECGINTP",
#         .df$AVALC == "ABNORMAL" | .df$AVALC == "NORMAL",
#         .df$AVAL == min(.df$AVAL)))
#     } %>%
#     slice(1) %>%
#     { # nolint
#       if (worst_obs == TRUE)
#         mutate(., new_var = case_when(
#           (.df$AVALC == "ABNORMAL" | .df$AVALC == "NORMAL") ~ "Y",
#           (!is.na(.df$AVAL) & is.na(.df$DTYPE)) ~ "Y",
#           TRUE ~ ""
#         ))
#       else
#         mutate(., new_var = case_when(
#           (.df$AVALC == "ABNORMAL" | .df$AVALC == "NORMAL") ~ "Y",
#           (!is.na(.df$AVAL) & is.na(.df$DTYPE)) ~ "Y",
#           TRUE ~ ""
#         ))
#     } %>%
#     ungroup()
#
#   data_compare$new_var <- ifelse(data_compare$row_check %in% data$row_check, "Y", "")
#   data_compare <- data_compare[, -which(names(data_compare) %in% c("row_check"))]
#
#   return(data_compare)
# }
#
# gen_adeg_wors <- function(n, .df, ...) {
#   tibble(WORS01FL = flag_variables(.df, FALSE)$new_var,
#          WORS02FL = flag_variables(.df, TRUE)$new_var
#   )
# }

secs_per_year <- 31557600

gen_adeg_adtm <- function(n, .df, study_duration = 2, ...) {
  study_duration_secs <- secs_per_year * study_duration
  trtsdt_int <- as.numeric(as.Date(.df$TRTSDTM))
  trtedt_int <- case_when(!is.na(.df$TRTEDTM) ~ as.numeric(as.Date(.df$TRTEDTM)),
                          is.na(.df$TRTEDTM) ~ floor(.df$trtsdt_int + (study_duration_secs) / 86400))
  tibble(ADTM = as.POSIXct((sample(.df$trtsdt_int:.df$trtedt_int, size = 1) * 86400), origin = "1970-01-01"))
}

gen_adeg_ady <- function(n, .df, ...) {
  tibble(ADY = ceiling(as.numeric(difftime(.df$ADTM, .df$TRTSDTM, units = "days"))))
}

#' Recipes for creating ADEG CDISC Data
#'
#' @rdname adeg_recipes
#' @export
#'

adae_scaff <- rand_per_key("USUBJID", mincount = 0, maxcount = 10, prop_present = 1)

adeg_rel_join_recipe <- tribble(
  ~foreign_tbl, ~foreign_key, ~foreign_deps, ~variables, ~dependencies, ~func, ~func_args,
  "ADSL", "USUBJID", "PARAMCD", c("PARAM", "PARAMU", "mean_aval", "sd_aval", "ANRLO", "ANRHI"), no_deps, adae_scaff, NULL)

adeg_table_recipe <- tribble(
  ~variables,                          ~dependencies,                                         ~func,                 ~func_args,
  c("ASEQ", "EGSEQ"),                  no_deps,                                               gen_adeg_seq,          NULL,
  c("EGTESTCD", "EGTEST"),             no_deps,                                               gen_adeg_eq,           NULL,
  "ATPTN",                             no_deps,                                               gen_adeg_atptn,        NULL,
  # c("DTYPE", "AVISIT"),                c("ONTRTFL", "ADTM"),                                  gen_adeg_dtype_avisit, list(minimum = TRUE),
  "ASPID",                             no_deps,                                               gen_adeg_aspid,        NULL,
  c("AVAL", "AVALU"),                  c("mean_aval", "sd_aval"),                             gen_adeg_aval,         NULL,
  c("BASE", "BASETYPE"),               c("AVAL", "AVISITN", "ABLFL"),                         gen_adeg_base,         NULL,
  "AVALC",                             c("PARAMCD", "AVAL"),                                  gen_adeg_adesc,        NULL,
  "BASEC",                             c("PARAMCD", "AVISIT", "BASE"),                        gen_adeg_bdesc,        NULL,
  "CHG",                               c("AVISITN", "AVAL", "BASE"),                          gen_adeg_chg,          NULL,
  "PCHG",                              c("AVISITN", "CHG", "BASE"),                           gen_adeg_pchg,         NULL,
  "ANRIND",                            c("AVAL", "ANRLO", "ANRHI"),                           gen_adeg_anrind,       NULL,
  "BNRIND",                            c("ANRIND", "ABLFL"),                                  gen_adeg_bnrind,       NULL,
  # "ABLFL",                             "AVISIT",                                              gen_adeg_ablfl,        NULL,
  "ANL01FL",                           c("ABLFL", "DTYPE", "WORS01FL", "AVISIT"),             gen_adeg_anl01fl,      NULL,
  "ANL03FL",                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_adeg_anl03fl,      NULL,
  "ANL04FL",                           c("ABLFL", "DTYPE", "PARAMCD"),                        gen_adeg_anl04fl,      NULL,
  "ONTRTFL",                           c("TRTSDTM", "TRTEDTM", "ADTM"),                       gen_adeg_onfl,         NULL,
  "AVISITN",                           "AVISIT",                                              gen_adeg_avisitn,      NULL,
  # c("WORS01FL", "WORS02FL"),           no_deps,                                               gen_adeg_worsfl,       NULL,
  "ADTM",                              c("TRTSDTM", "TRTEDTM"),                               gen_adeg_adtm,         list(study_duration = 2),
  "ADY",                               c("ADTM", "TRTSDTM"),                                  gen_adeg_ady,          NULL
)

# ADSL <- gen_table_data(N = 10, recipe = adsl_recipe)
# ADEG <- gen_reljoin_table(adeg_rel_join_recipe, adeg_table_recipe, db = list(ADSL = ADSL))

