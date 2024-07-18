#' @title Minimum flow rate to support MO2
#' 
#' @description Calculates the minimum flow rate into a respirometer required to maintain a high pO2. Useful for ensuring an acclimating animal maintains a normoxic environment. It can also be used to estimate the flow rate needed for a given pO2 decrease desired for flow-through respirometry.
#' 
#' @param MO2 whole-animal oxygen consumption rate (umol / hour).
#' @param min_pO2 minimum pO2 acceptable in respirometer (\% air saturation). Default is 90\% air saturation.
#' @param pO2_in pO2 of water entering respirometer (\% air saturation). Default is 100\% air saturation.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return The flow rate (liters / min) into the respirometer required for the steady state pO2 to be \code{min_pO2}.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Keep in mind that most organisms are very stressed upon being placed in a respirometer and their MO2 may be much higher than basal MO2.
#' @references Steffensen JF. 1989. Some errors in respirometry of aquatic breathers: How to avoid and correct for them. Fish Physiol Biochem. 6:49–59. Equation 8.
#' @seealso \code{\link{max_MO2}}, \code{\link{flush_water}}
#' 
#' @examples
#' min_flow(MO2 = 1000)
#' 
#' # What is the minimum flow rate required to maintain at least 75% air saturation in a
#' # respirometer with an organism(s) with an oxygen consumption rate of 1000 umol/h
#' # when the intake fresh water is 10 °C and 90% air saturated?
#' min_flow(MO2 = 1000, min_pO2 = 75, pO2_in = 90, temp = 10, sal = 0)
#' 
#' @encoding UTF-8
#' @export

min_flow = function(MO2, min_pO2 = 90, pO2_in = 100, temp = 25, sal = 35, atm_pres = 1013.25){
	o2_conc = as.numeric(marelac::gas_satconc(S = sal, t = temp, P = measurements::conv_unit(atm_pres, 'mbar', 'bar'), species = 'O2'))
	cO2_in = (pO2_in / 100) * o2_conc
	return((MO2 / 60) / (cO2_in - o2_conc * (min_pO2 / 100)))
}