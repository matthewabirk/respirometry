#' @title Calculate respiratory quotient
#' 
#' @description Calculates the respiratory quotient (RQ), or ratio of CO2 produced to O2 consumed between observations. To calculate CO2 produced, either DIC or both pH and TA must be provided.
#' 
#' @param o2 a numeric vector of O2 values with a length of at least 2.
#' @param o2_unit a string describing the unit used to measure \code{o2}. Default is "percent_a.s." Options are from \code{\link{conv_o2}}.
#' @param pH pH (total scale). Elements must align with \code{o2} vector.
#' @param TA total alkalinity (umol / kg). May be either a vector with length equal to \code{o2} or a single numeric value.
#' @param DIC dissolved inorganic carbon (umol / kg). Elements must align with \code{o2} vector.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return ratio of CO2 produced to O2 consumed.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note If you want a rough estimate of RQ, but only have pH measurements, TA can be estimated from salinity using \code{\link{guess_TA}}.
#' @seealso \code{\link{conv_o2}}, \code{\link{guess_TA}}
#' 
#' @examples
#' o2_observations = c(21, 18, 14.5, 7)
#' pH_observations = c(8.05, 7.98, 7.86, 7.65)
#' TA_observations = c(2222, 2219, 2208, 2214)
#' 
#' RQ(o2 = o2_observations, o2_unit = 'kPa', pH = pH_observations,
#' TA = TA_observations, temp = 20, sal = 33)
#' 
#' DIC_observations = c(2222, 2250, 2284, 2355)
#' RQ(o2 = o2_observations, o2_unit = 'kPa', DIC = DIC_observations)
#' 
#' RQ(o2 = o2_observations, o2_unit = 'kPa', pH = pH_observations, TA = 2032)
#' @encoding UTF-8
#' @export

RQ = function(o2, o2_unit = "percent_a.s.", pH = NULL, TA = NULL, DIC = NULL, temp = 25, sal = 35, atm_pres = 1013.25){
	if(sum(is.null(pH), is.null(TA)) >= 1 & is.null(DIC)) stop('Either DIC or both pH and TA must be provided.')
	if(length(o2) <= 1) stop('o2 must have a length of at least 2.')
	
	amb_pres = measurements::conv_unit(atm_pres, 'mbar', 'atm')
	if(is.null(DIC)){
		TA = TA * 1e-6
		DIC = seacarb::carb(flag = 8, var1 = pH, var2 = TA, S = sal, T = temp, Patm = amb_pres)$DIC
		DIC = DIC * 1e6
	}
	o2 = conv_o2(o2 = o2, from = o2_unit, to = 'umol_per_kg', temp = temp, sal = sal, atm_pres = atm_pres)
	RQ = diff(DIC) / -diff(o2)
	return(RQ)
}