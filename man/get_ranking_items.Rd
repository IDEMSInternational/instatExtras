% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ranking_items.R
\name{get_ranking_items}
\alias{get_ranking_items}
\title{Get Ranking Items from a Rankings Object}
\usage{
get_ranking_items(
  data = get_index_names,
  vars_to_get,
  index = "rankings_index",
  rankings_object
)
}
\arguments{
\item{data}{A data frame (default is \code{get_index_names}) containing variable metadata,
including a column that matches the \code{index} argument.}

\item{vars_to_get}{A character vector of variable names to extract from the \code{rankings_object}.}

\item{index}{The name of the column in \code{data} used to index or filter (default is \code{rankings_index}).}

\item{rankings_object}{A named list or object where each element corresponds to a ranking variable.}
}
\value{
A list containing the retrieved items from \code{rankings_object} corresponding to \code{vars_to_get}.
}
\description{
Extracts specified ranking variables from a rankings object.
}
\examples{
# Example data setup:
data <- data.frame(
  Name = c("id", "variety", "lastassessment_grainquality", "lastassessment_yield"),
  label = c("id", "variety", NA, NA),
  class = c("character", "character", "numeric", "numeric"),
  Dependent_Columns = c("count_all", "count_all", NA, NA),
  Has_Dependants = c(TRUE, TRUE, NA, NA),
  Is_Hidden = c(FALSE, FALSE, FALSE, FALSE),
  Is_Key = c(TRUE, TRUE, FALSE, FALSE),
  rankings_index = c(NA, NA, 2, 4),
  Scientific = c(FALSE, FALSE, FALSE, FALSE),
  Signif_Figures = c(NA, NA, 7, 7),
  Tricot_Type = c("id", "variety", "traits", "traits")
)

rankings_object <- list(
  lastassessment_grainquality = c(
    "CSW18 > PBW502 > HW2045", "CSW18 > HD2985 > PBW502",
    "DBW17 > RAJ4120 > HW2045", "CSW18 > PBW343 > RAJ4120",
    "PBW343 > HI1563 > HW2045", "K9107 > HD2824 > PBW502"
  ),
  lastassessment_yield = c(
    "CSW18 > PBW502 > HW2045", "CSW18 > HD2985 > PBW502",
    "DBW17 > RAJ4120 > HW2045", "CSW18 > PBW343 > RAJ4120",
    "PBW343 > HI1563 > HW2045", "K9107 > HD2824 > PBW502"
  )
)

# Example 1: Get rankings for 'lastassessment_grainquality' and 'lastassessment_yield'
vars_to_get <- c("lastassessment_grainquality", "lastassessment_yield")
result1 <- get_ranking_items(data, vars_to_get, "rankings_index", rankings_object)
print(result1)

# Example 2: Get rankings for just 'lastassessment_grainquality'
vars_to_get2 <- c("lastassessment_grainquality")
result2 <- get_ranking_items(data, vars_to_get2, "rankings_index", rankings_object)
print(result2)

# Example 3: using the default data parameter.

get_index_names <- data # setting up get_index_names for example 3
rankings_index <- "rankings_index" # setting rankings_index for example 3
vars_to_get3 <- c("lastassessment_yield")
result3 <- get_ranking_items(vars_to_get = vars_to_get3, rankings_object = rankings_object)
print(result3)
}
