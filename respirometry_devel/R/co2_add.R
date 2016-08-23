#' @title Calculate CO2 to add to water
#' 
#' @description Calculates the moles of CO2 gas to be added to a volume of seawater to achieve the desired pCO2. Useful for ocean acidification experiments where CO2 treatments are desired.
#' 
#' @param goal_pco2 the desired pCO2 in the water (uatm).
#' @param start_pH pH of the water before CO2 is added (total scale).
#' @param vol volume of the water (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return moles of CO2 gas to be added to the seawater.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note It is assumed that all of the CO2 added dissolves and remains in solution.
#' @seealso \code{\link{co2_rate}}, \code{\link{flush_carb}}, \code{\link[seacarb]{carb}}, \code{\link{peri_pump}}
#' 
#' @examples
#' # I want the 50 L respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.88.
#' # How many moles of CO2 gas should be added to the water to reach my desired pCO2?
#' co2_add(goal_pco2 = 1000, start_pH = 7.88, vol = 50)
#' 
#' @encoding UTF-8
#' @export

co2_add = function(goal_pco2, start_pH, vol, temp = 25, sal = 35, TA = NULL, atm_pres = 1013.25){
	amb_pres = measurements::conv_unit(atm_pres, 'mbar', 'atm')
	if(is.null(TA)) TA = guess_TA(temp = temp, sal = sal)
	TA = measurements::conv_unit(TA, 'umol', 'mol')
	init_DIC = seacarb::carb(flag = 8, var1 = start_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
	goal_DIC = seacarb::carb(flag = 24, var1 = goal_pco2, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
	C_to_add = (goal_DIC - init_DIC) * measurements::conv_dim(x = vol, x_unit = 'l', trans = as.numeric(seacarb::rho(S = sal, T = temp)), trans_unit = 'kg_per_m3', y_unit = 'kg')
	return(C_to_add)
}