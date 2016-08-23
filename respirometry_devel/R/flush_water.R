#' @title Find percent of water exchanged after a flush
#' 
#' @description Calculate the proportion of water in a respirometer that is new after a flush. Useful for intermittent respirometry.
#' 
#' @param vol volume of the respirometer (liter).
#' @param flow_rate rate of water flow into the respirometer (liters / minute).
#' @param duration duration of the flush (minutes).
#' @param perc_fresh percent of the respirometer volume that is new flushed water.
#' @param plot logical. Plot the percent exchanged as a function of flow rate and duration to see what effect would result if these rate or duration are changed.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Steffensen JF. 1989. Some errors in respirometry of aquatic breathers: How to avoid and correct for them. Fish Physiol Biochem. 6:49–59. Equation 5.
#' @seealso \code{\link{flush_carb}}, \code{\link{min_flow}}
#' 
#' @examples
#' # What proportion of a 90 L respirometer is exchanged after 20 minutes of flow at 2 LPM?
#' flush_water(vol = 90, flow_rate = 2, duration = 20)
#' 
#' # Would it be worth it to extend the flush another five minutes? How much would that
#' # improve the exchange?
#' flush_water(vol = 90, flow_rate = 2, duration = 20, plot = TRUE)
#' # Another five minutes would increase exchange by nearly 10%.
#' # Perhaps that's worth the extra time...
#' 
#' @encoding UTF-8
#' @export

flush_water = function(vol, flow_rate, duration, perc_fresh, plot = FALSE){
  if(sum(missing(vol), missing(flow_rate), missing(duration), missing(perc_fresh)) != 1) stop('3 of 4 of the parameters must be provided.')
  if(missing(vol)){
  	vol = flow_rate / (-log(-perc_fresh + 1) / duration)
  	result = list(vol = vol)
  }
  if(missing(flow_rate)){
  	flow_rate = -log(-perc_fresh + 1) / duration * vol
  	result = list(flow_rate = flow_rate)
  }
  if(missing(duration)){
  	D = flow_rate / vol
  	duration = log(-perc_fresh + 1) / -D
  	result = list(duration = duration)
  }
  if(missing(perc_fresh)){
    D = flow_rate / vol
    perc_fresh = 1 - exp(-D * duration)
    result = list(perc_fresh = perc_fresh)
  }
	if(plot == TRUE){
		graphics::layout(mat = matrix(1:2, ncol = 2))
		FR_range = seq(flow_rate - flow_rate / 2, flow_rate + flow_rate / 2, length.out = 100)
		D_range = FR_range / vol
		perc_range = 1 - exp(-D_range * duration)
		plot(FR_range, perc_range, type = 'l', xlab = 'Flow rate (LPM)', ylab = 'Proportion exchanged', main = paste('Volume =', vol, 'L\nDuration =', duration, 'minutes'))
		graphics::abline(v = flow_rate, lty = 2)
		
		dur_range = seq(duration - duration / 2, duration + duration / 2, length.out = 100)
		D = flow_rate / vol
		perc_range = 1 - exp(-D * dur_range)
		plot(dur_range, perc_range, type = 'l', xlab = 'Duration (min)', ylab = 'Proportion exchanged', main = paste('Volume =', vol, 'L\nFlow rate =', flow_rate, 'LPM'))
		graphics::abline(v = duration, lty = 2)
		graphics::layout(1)
	}
	return(result)
}