% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adae_recipe.R
\name{gen_ase_vars}
\alias{gen_ase_vars}
\title{Add Random date and day columns}
\usage{
gen_ase_vars(n, .df, study_duration_secs = 2 * secs_per_year)
}
\arguments{
\item{n}{numeric(1). Number of observations to generate. Ignored if \code{.df} is specified}

\item{.df}{data.frame. Partially generated data table to build upon}

\item{study_duration_secs}{numeric(1). Study duration in seconds}
}
\description{
This function generates random values for the \code{ASTDM} \code{AENDTM} \code{ASTDY} and \code{AENDY} columns and returns a data.frame containing those columns.
}
