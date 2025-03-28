% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_tricot.R
\name{pivot_tricot}
\alias{pivot_tricot}
\title{Pivot Tricot Data}
\usage{
pivot_tricot(
  data,
  id_var = "id",
  option_cols = c("option_a", "option_b", "option_c"),
  possible_ranks = c("A", "B", "C"),
  trait_good = "_pos",
  trait_bad = "_neg",
  na_value = "Not observed"
)
}
\arguments{
\item{data}{A data frame containing Tricot rankings and trait evaluations.}

\item{id_var}{Character string specifying the ID column name. Default is \code{"id"}.}

\item{option_cols}{A character vector of column names representing the options evaluated.
Default is \code{c("option_a", "option_b", "option_c")}.}

\item{possible_ranks}{A character vector specifying the possible rank values.
Default is \code{c("A", "B", "C")}.}

\item{trait_good}{Character string indicating the suffix for positive traits. Default is \code{"_pos"}.}

\item{trait_bad}{Character string indicating the suffix for negative traits. Default is \code{"_neg"}.}

\item{na_value}{The value used to indicate missing observations. Default is \code{"Not observed"}.}
}
\value{
A tibble with the transformed Tricot data, containing columns:
\itemize{
\item \code{id}: Identifier for each observation.
\item \code{trait}: Trait being evaluated.
\item \code{rank}: The rank assigned to each variety.
\item \code{variety}: The variety name.
}
}
\description{
This function transforms Tricot (Triadic Comparison of Technologies) data
into a tidy format, assigning ranks based on trait evaluations.
}
\examples{
# Read in tricot data from the `gosset` library
cassava_pivot <- pivot_tricot(gosset::cassava)
}
