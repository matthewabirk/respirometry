#' @title Calculate CO2 to add to flush reservoir
#' 
#' @description Calculates the moles of CO2 gas to be added to a seawater reservoir before flushing a respirometer to achieve the desired pCO2 in the respirometer after the flush. Useful for ocean acidification experiments where CO2 treatments are desired.
#' 
#' @param goal_pco2 the desired pCO2 in the respirometer after the flush (Pa).
#' @param resp_pH pH inside the respirometer before the flush (total scale).
#' @param resp_vol volume of the respirometer (liter).
#' @param flush_pH pH of the reservoir water used for flushing before CO2 is added (total scale).
#' @param flush_vol volume of the flush reservoir (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return moles of CO2 gas to be added to the flush reservoir.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note It is assummed that the entire reservoir is drained into the respirometer during the flush.
#' @seealso \code{\link{co2_rate}}, \code{\link{flush_carb}}, \code{\link{seacarb::carb}}
#' 
#' @examples
#' # I want the respirometer to have a pCO2 = 100 Pa. It currently has a pH of 7.6 and is 90 L. If I have a 200 L reservoir with pH = 7.9 seawater, how much CO2 do I need to add to the flush reservoir?
#' co2_add(goal_pco2 = 100, resp_pH = 7.6, resp_vol = 90, flush_pH = 7.9, flush_vol = 200)
#' 
#' @encoding UTF-8
#' @export

co2_add = function(goal_pco2, resp_pH, resp_vol, flush_pH, flush_vol, temp = 25, sal = 35, TA = NULL, atm_pres = 1013.25){
	amb_pres = birk::conv_unit(atm_pres, 'mbar', 'atm')
	if(is.null(TA)) TA = guess_TA(temp = temp, sal = sal)
	TA = birk::conv_unit(TA, 'umol', 'mol')
	resp_DIC = seacarb::carb(flag = 8, var1 = resp_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
	goal_DIC = seacarb::carb(flag = 24, var1 = birk::conv_unit(goal_pco2, 'Pa', 'uatm'), var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
  flush_perc = as.numeric(flush(vol = resp_vol, flow_rate = flush_vol, duration = 1))
  flush_DIC = (goal_DIC - resp_DIC * (1 - flush_perc)) / flush_perc
  init_DIC = seacarb::carb(flag = 8, var1 = flush_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
  C_to_add = (flush_DIC - init_DIC) * birk::conv_dim(x = flush_vol, x_unit = 'l', trans = as.numeric(seacarb::rho(S = sal, T = temp)), trans_unit = 'kg_per_m3', y_unit = 'kg')
  return(C_to_add)
}