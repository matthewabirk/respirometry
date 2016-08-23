#' @title Estimate dissolved O2 after a flush
#' 
#' @description Given the dissolved O2 inside the respirometer and in the flush reservoir, the new pO2 or [O2] in the respirometer after the flush is estimated.
#' 
#' @param resp_vol volume of the respirometer (liter).
#' @param flow_rate rate of water flow into the respirometer (liters / minute).
#' @param duration duration of the flush (minutes).
#' @param resp_o2 O2 inside the respirometer before the flush (units do not matter as long as it is consistant with \code{flush_o2}).
#' @param flush_o2 O2 of the water used for flushing the respirometer (units do not matter as long as it is consistant with \code{resp_o2}).
#' 
#' @return The dissolved O2 in the respirometer after the flush.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{flush_water}}, \code{\link{flush_carb}}
#' 
#' @examples
#' # What will be the pO2 in the respirometer after this flush?
#' flush_o2(resp_vol = 90, flow_rate = 10, duration = 3, resp_o2 = 15, flush_o2 = 21)
#' 
#' @encoding UTF-8
#' @export

flush_o2 = function(resp_vol, flow_rate, duration, resp_o2, flush_o2){
	perc_flush = unlist(flush_water(vol = resp_vol, flow_rate = flow_rate, duration = duration))
	mix_o2 = resp_o2 * (1 - perc_flush) + flush_o2 * perc_flush
	return(mix_o2)
}