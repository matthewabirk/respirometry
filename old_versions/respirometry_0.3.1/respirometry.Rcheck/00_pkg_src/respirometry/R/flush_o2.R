#' @title Estimate dissolved O2 after a flush
#' 
#' @description Calculate the pO2 or [O2] in a respirometer after a flush. Given 5 of the 6 parameters, the 6th parameter is calculated.
#' 
#' @param resp_vol volume of the respirometer (liter).
#' @param flow_rate rate of water flow into the respirometer (liters / minute).
#' @param duration duration of the flush (minutes).
#' @param resp_o2 O2 inside the respirometer before the flush (units do not matter as long as it is consistant with \code{flush_o2} and \code{final_o2}).
#' @param flush_o2 O2 of the water used for flushing the respirometer (units do not matter as long as it is consistant with \code{resp_o2} and \code{final_o2}).
#' @param final_o2 O2 of the water in the respirometer at the end of the flush (units do not matter as long as it is consistant with \code{resp_o2} and \code{flush_o2}).
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{flush_water}}, \code{\link{flush_carb}}
#' 
#' @examples
#' # What will be the pO2 in the respirometer after this flush?
#' flush_o2(resp_vol = 90, flow_rate = 10, duration = 3, resp_o2 = 15, flush_o2 = 21)
#' 
#' # I want to bring the pO2 back up to 95% air saturation. How long do I need to flush?
#' flush_o2(resp_vol = 20, flow_rate = 2, resp_o2 = 75, flush_o2 = 99, final_o2 = 95)
#' 
#' @encoding UTF-8
#' @export

flush_o2 = function(resp_vol, flow_rate, duration, resp_o2, flush_o2, final_o2){
	if(sum(missing(resp_vol), missing(flow_rate), missing(duration), missing(resp_o2), missing(flush_o2), missing(final_o2)) != 1) stop('5 of 6 of the parameters must be provided.')
	if(missing(resp_vol)){
		resp_vol = flush_water(flow_rate = flow_rate, duration = duration, perc_fresh = (final_o2 - resp_o2) / (flush_o2 - resp_o2))
		return(resp_vol)
	}
	if(missing(flow_rate)){
		flow_rate = flush_water(vol = resp_vol, duration = duration, perc_fresh = (final_o2 - resp_o2) / (flush_o2 - resp_o2))
		return(flow_rate)
	}
	if(missing(duration)){
		duration = flush_water(vol = resp_vol, flow_rate = flow_rate, perc_fresh = (final_o2 - resp_o2) / (flush_o2 - resp_o2))
		return(duration)
	}
	if(missing(resp_o2)){
		perc_fresh = flush_water(vol = resp_vol, flow_rate = flow_rate, duration = duration)
		resp_o2 = (perc_fresh * flush_o2 - final_o2) / (perc_fresh - 1)
		return(resp_o2)
	}
	if(missing(flush_o2)){
		perc_fresh = flush_water(vol = resp_vol, flow_rate = flow_rate, duration = duration)
		flush_o2 = 1 / perc_fresh * (final_o2 - resp_o2) + resp_o2
		return(flush_o2)
	}
	if(missing(final_o2)){
		perc_flush = flush_water(vol = resp_vol, flow_rate = flow_rate, duration = duration)
		final_o2 = resp_o2 * (1 - perc_flush) + flush_o2 * perc_flush
		return(final_o2)
	}
}