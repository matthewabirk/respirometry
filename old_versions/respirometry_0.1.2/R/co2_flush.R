#' @title Calculate CO2 to add to flush reservoir
#' 
#' @description Calculates the moles of CO2 gas to be added to a seawater reservoir before flushing a respirometer to achieve the desired pCO2 in the respirometer after the flush. Useful for ocean acidification experiments where CO2 treatments are desired.
#' 
#' @param goal_pco2 the desired pCO2 in the respirometer after the flush (uatm).
#' @param resp_pH pH inside the respirometer before the flush (total scale).
#' @param resp_vol volume of the respirometer (liter).
#' @param flush_pH pH of the reservoir water used for flushing before CO2 is added (total scale).
#' @param flush_vol volume of the flush reservoir (liter).
#' @param flush_remain volume of the flush reservoir that will remain after the flush (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu. If \code{sal} < 26 psu, then \code{TA} must be provided.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return moles of CO2 gas to be added to the flush reservoir.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note It is assummed that the entire reservoir is drained into the respirometer during the flush. It is also assumed that all of the CO2 added dissolves and remains in solution.
#' @seealso \code{\link{co2_rate}}, \code{\link{flush_carb}}, \code{\link[seacarb]{carb}}, \code{\link{peri_pump}}
#' 
#' @examples
#' # I want the respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.6 and is 90 L.
#' # If I have a 200 L reservoir with pH = 7.9 seawater, how much CO2 do I need
#' # to add to the flush reservoir?
#' co2_flush(goal_pco2 = 1000, resp_pH = 7.6, resp_vol = 90, flush_pH = 7.9, flush_vol = 200)
#' 
#' @encoding UTF-8
#' @export

co2_flush = function(goal_pco2, resp_pH, resp_vol, flush_pH, flush_vol, flush_remain = 0, temp = 25, sal = 35, TA = NULL, atm_pres = 1013.25){
	amb_pres = measurements::conv_unit(atm_pres, 'mbar', 'atm')
	if(is.null(TA)) TA = guess_TA(temp = temp, sal = sal)
	TA = measurements::conv_unit(TA, 'umol', 'mol')
	resp_DIC = seacarb::carb(flag = 8, var1 = resp_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
	goal_DIC = seacarb::carb(flag = 24, var1 = goal_pco2, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
  flush_perc = as.numeric(flush_water(vol = resp_vol, flow_rate = flush_vol - flush_remain, duration = 1))
  flush_DIC = (goal_DIC - resp_DIC * (1 - flush_perc)) / flush_perc
  init_DIC = seacarb::carb(flag = 8, var1 = flush_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
  C_to_add = (flush_DIC - init_DIC) * measurements::conv_dim(x = flush_vol, x_unit = 'l', trans = as.numeric(seacarb::rho(S = sal, T = temp)), trans_unit = 'kg_per_m3', y_unit = 'kg')
  return(C_to_add)
}