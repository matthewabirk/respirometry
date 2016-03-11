#' @title Calculate CO2 to add to a respirometer intake flow
#' 
#' @description Calculates the moles of CO2 gas to be added to a respirometer intake seawater flow to achieve the desired pCO2 in the respirometer. Useful for ocean acidification experiments where CO2 treatments are desired. Can be used for acclimation before a trial begins or for use with flow-through respirometry.
#' 
#' @param goal_pco2 the desired pCO2 in the respirometer (Pa).
#' @param init_pH ambient pH of the intake flow (total scale).
#' @param flow_rate rate of water flow into the respirometer (liters / minute).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param TA (optional) total alkalinity (umol / kg). If undefined TA is estimated from salinity using \code{\link{guess_TA}}.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' @param MO2 (optional) oxygen consumption rate (umol / hr). If defined, the CO2 to be added is reduced to compensate for the CO2 produced by the organism.
#' @param RQ (optional) respiratory quotient: ratio of CO2 produced / O2 consumed. Only used if \code{MO2} is defined. Default is 1.
#' 
#' @return moles of CO2 gas to be added to the intake flow per minute.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Jokiel PL, Bahr KD, Rodgers KS. 2014. Low-cost, high-flow mesocosm system for simulating ocean acidification with CO2 gas. Limnol Oceanogr Methods. 12:313–322.
#' @seealso \code{\link{co2_add}}, \code{\link{flush_carb}}, \code{\link{seacarb::carb}}
#' 
#' @examples
#' # I want the respirometer to have a pCO2 = 100 Pa. How much CO2 per minute do I need to add to the intake flow if the ambient pH is 8.1 and it is flowing at 3 LPM?
#' co2_rate(goal_pco2 = 100, init_pH = 8.1, flow_rate = 3)
#' 
#' @encoding UTF-8
#' @export

co2_rate = function(goal_pco2, init_pH, flow_rate, temp = 25, sal = 35, TA = NULL, atm_pres = 1013.25, MO2 = NULL, RQ = 1){
	amb_pres = birk::conv_unit(atm_pres, 'mbar', 'atm')
	if(is.null(TA)) TA = guess_TA(temp = temp, sal = sal)
	TA = birk::conv_unit(TA, 'umol', 'mol')
	init_DIC = seacarb::carb(flag = 8, var1 = init_pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
	goal_DIC = seacarb::carb(flag = 24, var1 = birk::conv_unit(goal_pco2, 'Pa', 'uatm'), var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
	C_add_per_kg = goal_DIC - init_DIC
	C_add_per_l = C_add_per_kg / birk::conv_dim(x = 1, x_unit = 'kg', trans = as.numeric(seacarb::rho(S = sal, T = temp)), trans_unit = 'kg_per_m3', y_unit = 'l')
	C_add_per_min = C_add_per_l * flow_rate
	if(!is.null(MO2)) C_add_per_min = C_add_per_min - birk::conv_unit(MO2 / 60 * RQ, 'umol', 'mol')
	return(C_add_per_min)
}