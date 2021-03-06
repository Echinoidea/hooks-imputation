% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compareDistribution.R
\name{compareDistribution}
\alias{compareDistribution}
\title{Compare Distributions of Original vs. Imputed Data}
\usage{
compareDistribution(original, imputed, by)
}
\arguments{
\item{original}{The original, unimputed data.}

\item{imputed}{The data with missing values imputed.}

\item{by}{The variable to plot the distributions of that both
\code{original} and \code{imputed} have.}
}
\value{
Returns a ggplot density plot containing a line for each data object.
}
\description{
Create a plot with two density layers representing the distribution of the
test variable for both the original and imputed data.
}
\examples{
compareDistribution(data, data_imputed, "values")

}
