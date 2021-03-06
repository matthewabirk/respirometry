% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flush_water.R
\encoding{UTF-8}
\name{flush_water}
\alias{flush_water}
\title{Find percent of water exchanged after a flush}
\usage{
flush_water(vol, flow_rate, duration, perc_fresh, plot = FALSE)
}
\arguments{
\item{vol}{volume of the respirometer (liter).}

\item{flow_rate}{rate of water flow into the respirometer (liters / minute).}

\item{duration}{duration of the flush (minutes).}

\item{perc_fresh}{percent of the respirometer volume that is new flushed water.}

\item{plot}{logical. Plot the percent exchanged as a function of flow rate and duration to see what effect would result if the rate or duration are changed. All parameters must only have a single value.}
}
\description{
Calculate the proportion of water in a respirometer that is new after a flush. Useful for intermittent respirometry. Given 3 of the first 4 parameters, the 4th parameter is calculated.
}
\examples{
# What proportion of a 90 L respirometer is exchanged after 20 minutes of flow at 2 LPM?
flush_water(vol = 90, flow_rate = 2, duration = 20)

# Would it be worth it to extend the flush another five minutes? How much would that
# improve the exchange?
flush_water(vol = 90, flow_rate = 2, duration = 20, plot = TRUE)
# Another five minutes would increase exchange by nearly 10\%.
# Perhaps that's worth the extra time...

# Visualize flushing
vol = 150
flow_rate = seq(0, 10, by = 0.5)
duration = 0:60
perc_fresh = outer(flow_rate, duration, function(flow_rate, duration){
	flush_water(vol = vol, flow_rate = flow_rate, duration = duration)
})
persp(flow_rate, duration, perc_fresh, xlab = 'Flow rate (LPM)', ylab = 'Duration (min)',
zlab = '\% exchange', theta = 45, phi = 15, expand = 0.5, ticktype = 'detailed', nticks = 10)

}
\references{
Steffensen JF. 1989. Some errors in respirometry of aquatic breathers: How to avoid and correct for them. Fish Physiol Biochem. 6:49–59. Equation 5.
}
\seealso{
\code{\link{flush_carb}}, \code{\link{min_flow}}
}
\author{
Matthew A. Birk, \email{matthewabirk@gmail.com}
}
