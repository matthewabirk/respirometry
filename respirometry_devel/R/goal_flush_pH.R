#' @title Calculate goal pH of a flush reservoir to achieve the post-flush goal pCO2
#' 
#' @description Calculates the pH of a flush reservoir that is needed to achieve the goal pCO2 after the flush reservoir has been drained into the respirometer.
#' 
#' @param goal_pco2 the desired pCO2 in the respirometer after the flush (uatm).
#' @param resp_pH pH inside the respirometer before the flush (total scale).
#' @param resp_vol volume of the respirometer (liter).
#' @param flush_vol volume of the flush reservoir (liter).
#' @param flush_remain volume of the flush reservoir that will remain after the flush (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return pH needed in the flush reservoir to achieve the goal pCO2 post-flush (total scale).
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{co2_rate}}, \code{\link{flush_carb}}, \code{\link[seacarb]{carb}}, \code{\link{peri_pump}}
#' 
#' @examples
#' # I want the respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.6 and is 90 L.
#' # If I have a 200 L reservoir which will be drained completely, what do I want
#' # the pH of the reservoir to be?
#' goal_flush_pH(goal_pco2 = 1000, resp_pH = 7.6, resp_vol = 90, flush_vol = 200)
#' 
#' @encoding UTF-8
#' @export

goal_flush_pH = function(goal_pco2, resp_pH, resp_vol, flush_vol, flush_remain = 0, temp = 25, sal = 35, TA = NULL, atm_pres = 1013.25){
  amb_pres = measurements::conv_unit(atm_pres, 'mbar', 'atm')
  if(is.null(TA)) TA = guess_TA(temp = temp, sal = sal)
  TA = measurements::conv_unit(TA, 'umol', 'mol')
  resp_DIC = seacarb::carb(flag = 8, var1 = resp_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
  goal_DIC = seacarb::carb(flag = 24, var1 = goal_pco2, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
  flush_perc = as.numeric(flush_water(vol = resp_vol, flow_rate = flush_vol - flush_remain, duration = 1))
  flush_DIC = (goal_DIC - resp_DIC * (1 - flush_perc)) / flush_perc
  flush_pH_goal = seacarb::carb(flag = 15, var1 = TA, var2 = flush_DIC, S = sal, T = temp, Patm = amb_pres)$pH
  return(flush_pH_goal)
}