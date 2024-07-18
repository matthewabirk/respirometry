#' @title Estimate carbonate chemistry after a flush
#' 
#' @description Given the seawater pH inside the respirometer and in the flush reservoir, the new carbonate parameters (including pH) in the respirometer after the flush are estimated.
#' 
#' @param resp_vol volume of the respirometer (liter).
#' @param flow_rate rate of water flow into the respirometer (liters / minute).
#' @param duration duration of the flush (minutes).
#' @param resp_pH pH inside the respirometer before the flush (total scale).
#' @param flush_pH pH of the water used for flushing the respirometer (total scale).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return A data frame returned by \code{\link[seacarb]{carb}}.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link[seacarb]{carb}}, \code{\link{flush_water}}
#' 
#' @examples
#' flush_carb(resp_vol = 90, flow_rate = 10, duration = 3, resp_pH = 7.8, flush_pH = 8.1)
#' 
#' # What will be the pH in the respirometer after this flush?
#' flush_carb(resp_vol = 90, flow_rate = 10, duration = 3, resp_pH = 7.8, flush_pH = 8.1)$pH
#' 
#' @encoding UTF-8
#' @export

flush_carb = function(resp_vol, flow_rate, duration, resp_pH, flush_pH, temp = 25, sal = 35, TA = NULL, atm_pres = 1013.25){
	amb_pres = measurements::conv_unit(atm_pres, 'mbar', 'atm')
	if(is.null(TA)) TA = guess_TA(temp = temp, sal = sal)
	TA = measurements::conv_unit(TA, 'umol', 'mol')
	resp_conditions = seacarb::carb(flag = 8, var1 = resp_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)
	flush_conditions = seacarb::carb(flag = 8, var1 = flush_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)
	perc_flush = unlist(flush_water(vol = resp_vol, flow_rate = flow_rate, duration = duration))
	mix_DIC = resp_conditions$DIC * (1 - perc_flush) + flush_conditions$DIC * perc_flush
	end_conditions = seacarb::carb(flag = 15, var1 = TA, var2 = mix_DIC, S = sal, T = temp, Patm = amb_pres)
	return(end_conditions)
}