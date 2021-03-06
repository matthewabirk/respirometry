% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_alpha.R
\encoding{UTF-8}
\name{calc_alpha}
\alias{calc_alpha}
\title{Calculate the oxygen supply capacity (alpha)}
\usage{
calc_alpha(po2, mo2, MR = NULL, mo2_threshold = Inf)
}
\arguments{
\item{po2}{a vector of PO2 values.}

\item{mo2}{a vector of metabolic rate values. Must be the same length and corresponding to \code{po2}.}

\item{MR}{a vector of values for the metabolic rate at which \code{pcrit_alpha} should be returned. Default is \code{NULL}. If not specified, then \code{pcrit_alpha} is not returned and a message is added to the top of the return.}

\item{mo2_threshold}{a single numeric value above which \code{mo2} values are ignored. Useful to removing obviously erroneous values. Default is \code{Inf}.}
}
\value{
Returns a list of 1) alpha, 2) the PO2 and MO2 coordinates of the observation where alpha was reached, and 3) the Pcrit at a metabolic rate of \code{MR}.
}
\description{
The oxygen supply capacity (\eqn{\alpha}) is a species- and temperature-specific value quantifying an animal's ability to extract oxygen from the ambient medium to support its metabolism (e.g. umol O2 / g / hr / kPa).
}
\examples{
mo2_data <- read.csv(system.file('extdata', 'mo2_v_po2.csv', package = 'respirometry'))
calc_alpha(po2 = mo2_data$po2, mo2 = mo2_data$mo2, MR = 1.5) # MR set to 1.5 to capture the 
# Pcrit corresponding to some of the lowest MO2 values recorded (something close to SMR).

}
\references{
Seibel et al. 2020 (in prep).
}
\seealso{
\code{\link{calc_pcrit}}, \code{\link{plot_pcrit}}
}
\author{
Matthew A. Birk, \email{matthewabirk@gmail.com}
}
