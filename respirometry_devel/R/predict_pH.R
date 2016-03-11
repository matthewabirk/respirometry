#' @title Predict pH post-respiration
#' 
#' @description Predicts the pH of seawater after a defined amount of oxygen consumption.
#' 
#' @details
#' Given a known amount of oxygen consumed and an estimated respiratory quotient (see \code{\link{Q10}}), the amount of CO2 produced can be estimated. From this CO2 production estimate, the carbonate chemistry of the seawater can be estimated. Atmospheric pressure is assumed.
#' 
#' @param start_o2 pO2 at the start of the measurement (% air saturation). Default is 100% air saturation.
#' @param end_o2 pO2 at the end of the measurment (% air saturation).
#' @param start_pH seawater pH (total scale) at the start of the measurement.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param RQ respiratory quotient: ratio of CO2 produced / O2 consumed. Default is 1.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' 
#' @return A list of the predicted pH (total scale) at the end of the measurement and the predicted pCO2 (Pa).
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{seacarb::carb}}, \code{\link{guess_TA}}
#' 
#' @examples
#' predict_pH(end_o2 = 75, start_pH = 8.1)
#' predict_pH(start_o2 = 75, end_o2 = 50, start_pH = 7.96, temp = 15, sal = 33, RQ = .88)
#' 
#' @encoding UTF-8
#' @export

predict_pH = function(start_o2 = 100, end_o2, start_pH, temp = 25, sal = 35, RQ = 1, TA = NULL){
  o2_consumed_l = presens::o2_unit_conv(perc_a.s. = start_o2 - end_o2, salinity = sal, temp = temp)$umol_per_l
  o2_consumed = o2_consumed_l / (seacarb::rho(S = sal, T = temp)/1000)
  co2_produced = o2_consumed * RQ * 1e-6
  
  # estimate TA
  TA = if(is.null(TA)) guess_TA(temp = temp, sal = sal)
  TA = birk::conv_unit(TA, 'umol', 'mol')
  
  start = seacarb::carb(flag = 8, var1 = start_pH, var2 = TA, S = sal, T = temp)
  end = as.list(seacarb::carb(flag = 15, var1 = TA, var2 = start$DIC + co2_produced, S = sal, T = temp)[,c('pH', 'pCO2')])
  end$pCO2 = birk::conv_unit(end$pCO2, 'uatm', 'Pa')
  return(end)
}