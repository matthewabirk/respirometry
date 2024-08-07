#' @title Maximum MO2 supported by flow rate
#' 
#' @description Calculates the maximum oxygen consumption rate (MO2) supported by a respirometer with a given flow rate. Useful for ensuring an acclimating animal maintains a normoxic environment.
#' 
#' @param flow_rate water flow rate into respirometer (liters / min).
#' @param min_pO2 minimum pO2 acceptable in respirometer (\% air saturation). Default is 90\% air saturation.
#' @param pO2_in pO2 of water entering respirometer (\% air saturation). Default is 100\% air saturation.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return The maximum whole-animal oxygen consumption rate (umol / hr) that can be sustained.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Keep in mind that most organisms are very stressed upon being placed in a respirometer and their MO2 may be much higher than basal MO2.
#' @references Steffensen JF. 1989. Some errors in respirometry of aquatic breathers: How to avoid and correct for them. Fish Physiol Biochem. 6:49–59. Equation 8.
#' @seealso \code{\link{min_flow}}, \code{\link{flush_water}}
#' 
#' @examples
#' max_MO2(flow_rate = 1)
#' 
#' # What is the maximum MO2 organism I can place in my respirometer and still maintain at
#' # least 75% air saturation when the intake fresh water is 1.5 LPM, 10 °C and 90% air saturated?
#' (max_mo2 <- max_MO2(flow_rate = 1.5, min_pO2 = 75, pO2_in = 90, temp = 10, sal = 0))
#' 
#' # If a 300 g individual has an MO2 of 2000 umol/hr, how big of an animal can I use?
#' scale_MO2(mass_1 = 300, MO2_1 = 2000, MO2_2 = max_mo2) # I can almost support a 1 kg individual!
#' 
#' @encoding UTF-8
#' @export

max_MO2 = function(flow_rate, min_pO2 = 90, pO2_in = 100, temp = 25, sal = 35, atm_pres = 1013.25){
	o2_conc = as.numeric(marelac::gas_satconc(S = sal, t = temp, P = measurements::conv_unit(atm_pres, 'mbar', 'bar'), species = 'O2'))
	cO2_in = (pO2_in / 100) * o2_conc
	return(flow_rate * (cO2_in - o2_conc * (min_pO2 / 100)) * 60)
}


#' @rdname max_MO2
#' @export
max_mo2 = max_MO2