lookup_cm <- tribble(
      ~CMCLAS, ~CMDECOD, ~ATIREL,
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




adcm_sjf <- rand_per_key("USUBJID", tblnm= "ADSL", mincount = 0, maxcount = 10, prop_present = 1)



adcm_merge_lu <- function(n, .df, lookup = lookup_cm) {
    n <- NROW(.df)
    rows <- sample(1:NROW(lookup), n, replace = TRUE)
    lookup[rows,]
}

#' @rdname cdisc_recs
#' @export
adcm_scaff_recipe <- tribble(
    ~foreign_tbls, ~foreign_key, ~func, ~func_args,
    "ADSL", "USUBJID", adcm_sjf, NULL)

.pad_atc <- function(dat, inds, atcnum, suffix) {
    colnm <- paste0("ATC", atcnum)
    dat[inds, colnm] <- paste(dat[[colnm]][inds], suffix)
    dat
}

adcm_gen_atc <- function(n, .df, who_coding = FALSE, lookup = lookup_cm) {
    ret <- data.frame(stringsAsFactors = FALSE,
                      ATC1 = paste("ATCCLAS1", substr(.df$CMDECOD, 9, 9)),
                      ATC2 = paste("ATCCLAS2", substr(.df$CMDECOD, 9, 9)),
                      ATC3 = paste("ATCCLAS3", substr(.df$CMDECOD, 9, 9)),
                      ATC4 = paste("ATCCLAS4", substr(.df$CMDECOD, 9, 9)))

    if(!who_coding) {
        p2_i <- sample(1:NROW(lookup), ceiling(NROW(lookup)/2))
        p2cmods <- lookup[p2_i, "CMDECOD", drop = TRUE]
        p2chng <- which(ret$CMDECOD %in% p2cmods)
        for(i in 1:4)
            ret <- .pad_atc(ret, p2chng, i, "p2")

        p3_i <- sample(p2_i, ceiling(length(p2_i)/2))
        p3cmods <- lookup[p3_i, "CMDECOD", drop = TRUE]
        p3chng <- which(ret$CMDECOD %in% p3cmods)
        for(i in 1:4)
            ret <- .pad_atc(ret, p3chng, i, "p3")

    }

    for(i in 1:4) {
        ret[[paste0("ATC", i, "CD")]] <- ret[[paste0("ATC", i)]]
    }
    ret
}

atcvars <- c(paste0("ATC", 1:4),
             paste0("ATC", 1:4, "CD"))

#' @rdname cdisc_recs
#' @export
adcm_tbl_recipe <- tribble(
    ~variables, ~dependencies, ~func, ~func_args,
    names(lookup_cm), "USUBJID",  adcm_merge_lu, list(lookup = lookup_cm),
    atcvars, "CMDECOD", adcm_gen_atc, list(lookup = lookup_cm, who_coding = FALSE),
    "CMCAT", "CMCLAS", function(n, .df) .df$CMCLAS, NULL)
