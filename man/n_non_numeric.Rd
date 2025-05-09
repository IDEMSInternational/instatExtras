% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_non_numeric.R
\name{n_non_numeric}
\alias{n_non_numeric}
\title{Count Non-Numeric Elements in a Vector}
\usage{
n_non_numeric(x)
}
\arguments{
\item{x}{A vector of any type.}
}
\value{
An integer indicating the number of non-numeric elements in the vector.
}
\description{
This function counts the number of elements in a vector that are not purely numeric.
}
\details{
The function first converts the input to a character vector. It then
compares the presence of \code{NA} values before and after attempting numeric conversion.
Elements that fail numeric conversion are considered non-numeric.
}
\examples{
x <- c("10", "abc", "5.5", "NaN", "NA", "42", "3e2")
n_non_numeric(x)  # Should count the non-numeric values

}
