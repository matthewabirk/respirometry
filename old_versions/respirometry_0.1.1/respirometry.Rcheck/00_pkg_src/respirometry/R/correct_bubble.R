#' @title Adjust respirometer volume for bubbles
#' 
#' @description Given the volume of the respirometer and the volume of bubbles or air space, the moles of O2 in the system are calculated, and the volume of a respirometer holding the same quantity of O2 with only water is returned.
#' 
#' @details
#' Depending on temperature and salinity, air holds 20,000x as much O2 as water per unit volume, thus small air bubbles in a respirometer can dramatically increase the amount of O2 an organism has to consume to lower the pO2 or aqueous [O2]. Thus air bubbles lead to underestimations of MO[2]. To correct for this in MO2 calculations after measurement, the volume of the respirometer can be increased. This function calculates the volume needed for MO2 calculations as a function of the volume of air space.
#' 
#' @param resp_vol volume of the respirometer (liter).
#' @param bubble_vol volume of the gas bubbles or headspace (mL).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return The volume of a respirometer holding an equivalent quantity of O2 filled only with water.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Due to the high concentration of O2 in air, very small errors in bubble volume estimates can lead to very large differences in the volume returned. Only trust the returned value if you are very confident of the accuracy of your bubble volume estimate.
#' @seealso \code{\link[marelac]{molvol}}
#' 
#' @examples
#' correct_bubble(resp_vol = 50, bubble_vol = 10) # a 10 mL bubble makes a huge difference!
#' 
#' correct_bubble(resp_vol = 50, bubble_vol = 1, temp = 10, sal = 0) 
#' # in calculating MO2, a volume of 63.8 L should be used rather than the true 50 L.
#'
#' @encoding UTF-8
#' @export

correct_bubble = function(resp_vol, bubble_vol, temp = 25, sal = 35, atm_pres = 1013.25){
	air_pres = measurements::conv_unit(atm_pres, 'mbar', 'bar')
	bubble_vol = measurements::conv_unit(bubble_vol, 'ml', 'l')
	vol_water = resp_vol - bubble_vol
	mol_o2 = bubble_vol * marelac::atmComp(species = 'O2') * marelac::molvol(t = temp, P = air_pres, species = 'O2')
	water_equiv = measurements::conv_unit(mol_o2, 'mol', 'umol') / marelac::gas_satconc(S = sal, t = temp, P = air_pres, species = 'O2')
	corr_resp_vol = as.numeric(vol_water + water_equiv)
	return(corr_resp_vol)
}