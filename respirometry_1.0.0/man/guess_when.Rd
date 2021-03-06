% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess_when.R
\encoding{UTF-8}
\name{guess_when}
\alias{guess_when}
\title{Estimate when the O2 level will reach a defined level}
\usage{
guess_when(past_o2, past_time, goal_o2, plot = TRUE)
}
\arguments{
\item{past_o2}{a numeric vector of at least two oxygen measurements previously during the trial.}

\item{past_time}{a vector of timepoints corresponding to when \code{past_o2} values were recorded. Can be a numeric vector for duration since trial began or a POSIX vector of time values.}

\item{goal_o2}{a numeric vector or single value describing the O2 level of interest.}

\item{plot}{logical. Do you want to see a plot to visualize this prediction?}
}
\value{
A prediction of the time when O2 will reach \code{goal_o2}. If \code{past_time} is numeric, then a numeric value(s) will be returned. If POSIX, then POSIX will be returned.
}
\description{
Estimates the time at which O2 will reach a defined level assuming a linear change in O2 over time.
}
\note{
Viewing the plot can be valuable if the O2 consumption or production is not linear.
}
\examples{
guess_when(past_o2 = rnorm(n = 10, mean = 100:91), past_time = 1:10, goal_o2 = 75, plot = FALSE)
guess_when(past_o2 = rnorm(n = 10, mean = 100:91, sd = 5), past_time = 1:10, goal_o2 = 75)
# Viewing the plot can be helpful to see how trustworthy the prediction is
# when signal:noise is low.

}
\seealso{
\code{\link{predict_pH}}, \code{\link{predict_nh3}}
}
\author{
Matthew A. Birk, \email{matthewabirk@gmail.com}
}
