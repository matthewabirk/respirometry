% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Q10.R
\encoding{UTF-8}
\name{Q10}
\alias{Q10}
\title{Parameters of Q10 Temperature Coefficient}
\usage{
Q10(Q10, R2, R1, T2, T1)
}
\arguments{
\item{Q10}{factor by which rate changes due to 10 °C increase in temperature.}

\item{R2}{rate 2.}

\item{R1}{rate 1.}

\item{T2}{temperature 2 (in °C).}

\item{T1}{temperature 1 (in °C).}
}
\description{
Returns the unknown parameter given 4 of 5 parameters from Q10 temperature coefficient calculation for chemical or biological systems.
}
\details{
Given four parameters, the fifth parameter will be returned.
}
\examples{
Q10(R2 = 10, R1 = 5, T2 = 20, T1 = 10) # Returns Q10; = 2
Q10(Q10 = 2.66, R1 = 5, T2 = 20, T1 = 10) # Returns R2; = 13.3

}
\author{
Matthew A. Birk, \email{matthewabirk@gmail.com}
}

