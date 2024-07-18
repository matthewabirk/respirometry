#' @title Closed respirometry
#' 
#' @description Returns the unknown parameter given 3 of 4 parameters to calculate respiration rate in a closed respirometer. This is useful both for basic closed respirometry setups, and also for the closed measurement phase of intermittent respirometry.
#' 
#' @param MO2 whole-animal oxygen consumption rate (umol O2 / hour).
#' @param delta_pO2 desired change in pO2 (\% air saturation).
#' @param duration desired duration to reach \code{delta_pO2} (minutes).
#' @param vol volume of the respirometer (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @note If there are more than two O2 observations, consider using \code{\link{calc_MO2}}.
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{flush_water}}, \code{\link{calc_MO2}}
#' 
#' @examples
#' # I've read in the literature that my animal has an SMR of 200 umol/h. How large of a
#' # respirometer do I want if I want it to breathe down to 80% air saturation in 30 minutes?
#' closed(MO2 = 200, delta_pO2 = 100 - 80, duration = 30) # returns respirometer volume
#' 
#' # I've read in the literature that my animal has an SMR of 1000 umol/h. How long will it take to
#' # breathe down a 50 L respirometer by 10% air saturation?
#' closed(MO2 = 1000, delta_pO2 = 10, vol = 50) # returns the duration to breathe down the O2
#'
#' # How does animal size affect how long my measurement periods last?
#' mass_range <- seq(100, 400, 50)
#' dur_range <- (closed(MO2 = scale_MO2(mass_1 = 100, MO2_1 = 400, mass_2 = mass_range),
#'  delta_pO2 = 20, vol = 10))
#' plot(mass_range, dur_range, type = 'b')
#' 
#' # What is the MO2 if O2 drops 0.44 mg/l in 33 minutes when the respirometer volume is 30 L?
#' closed(delta_pO2 = conv_o2(o2 = 0.44, from = 'mg_per_l', to = 'percent_a.s.'), duration = 33,
#'  vol = 30)
#' 
#' @encoding UTF-8
#' @export

closed = function(MO2, delta_pO2, duration, vol, temp = 25, sal = 35, atm_pres = 1013.25){
	if(sum(missing(MO2), missing(delta_pO2), missing(duration), missing(vol)) != 1) stop('3 of the first 4 parameters must be provided.')
	if(missing(MO2)){
		MO2 = conv_o2(o2 = delta_pO2, from = 'percent_a.s.', to = 'umol_per_l', temp = temp, sal = sal, atm_pres = atm_pres) * vol / measurements::conv_unit(duration, 'min', 'hr')
		return(MO2)
	}
	pMO2 = conv_o2(o2 = MO2, from = 'umol_per_l', to = 'percent_a.s.', temp = temp, sal = sal, atm_pres = atm_pres) / measurements::conv_unit(1, 'hr', 'min') # % a.s.
	if(missing(delta_pO2)){
		delta_pO2 = pMO2 / vol * duration
		return(delta_pO2)
	}
	if(missing(duration)){
		duration = delta_pO2 / (pMO2 / vol)
		return(duration)
	}
	if(missing(vol)){
		vol = duration / delta_pO2 * pMO2
		return(vol)
	}
}