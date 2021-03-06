% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goal_flush_pH.R
\encoding{UTF-8}
\name{goal_flush_pH}
\alias{goal_flush_pH}
\title{Calculate goal pH of a flush reservoir to achieve the post-flush goal pCO2}
\usage{
goal_flush_pH(
  goal_pco2,
  resp_pH,
  resp_vol,
  flush_vol,
  flush_remain = 0,
  temp = 25,
  sal = 35,
  TA = NULL,
  atm_pres = 1013.25
)
}
\arguments{
\item{goal_pco2}{the desired pCO2 in the respirometer after the flush (uatm).}

\item{resp_pH}{pH inside the respirometer before the flush (total scale).}

\item{resp_vol}{volume of the respirometer (liter).}

\item{flush_vol}{volume of the flush reservoir (liter).}

\item{flush_remain}{volume of the flush reservoir that will remain after the flush (liter).}

\item{temp}{temperature (°C). Default is 25 °C.}

\item{sal}{salinity (psu). Default is 35 psu. If \code{sal} < 26 psu, then \code{TA} must be provided.}

\item{TA}{(optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.}

\item{atm_pres}{atmospheric pressure (mbar). Default is 1013.25 mbar.}
}
\value{
pH needed in the flush reservoir to achieve the goal pCO2 post-flush (total scale).
}
\description{
Calculates the pH of a flush reservoir that is needed to achieve the goal pCO2 after the flush reservoir has been drained into the respirometer.
}
\examples{
# I want the respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.6 and is 90 L.
# If I have a 200 L reservoir which will be drained completely, what do I want
# the pH of the reservoir to be?
goal_flush_pH(goal_pco2 = 1000, resp_pH = 7.6, resp_vol = 90, flush_vol = 200)

}
\seealso{
\code{\link{co2_rate}}, \code{\link{flush_carb}}, \code{\link[seacarb]{carb}}, \code{\link{peri_pump}}
}
\author{
Matthew A. Birk, \email{matthewabirk@gmail.com}
}
