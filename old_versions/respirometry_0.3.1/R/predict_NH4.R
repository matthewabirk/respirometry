#' @title Predict NH4 concentration post-respiration
#' 
#' @description Predicts the [NH4+] of seawater after a defined amount of oxygen consumption.
#' 
#' @details
#' Given a known amount of oxygen consumed and an estimated O:N ratio, the amount of NH4 produced can be estimated. Production or consumption of ammonium by "background" microbes or conversion of ammonium to nitrite and nitrate is ignored since bacteria in the respirometer are typically sought to be in low levels.
#' 
#' @param o2_drop a numeric value or vector describing the change in O2. Default is 10.
#' @param o2_unit a string describing the unit used to measure \code{o2_drop}. Default is "percent_a.s." Options are from \code{\link{conv_o2}}.
#' @param o2_nh4_ratio molar ratio of O2 consumed to NH4+ produced.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return The predicted NH4+ produced in mg/l.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{conv_o2}}, \code{\link{conv_NH4}}
#' 
#' @examples
#' predict_NH4(o2_drop = 25, o2_nh4_ratio = 10)
#' 
#' @encoding UTF-8
#' @export


predict_NH4 = function(o2_drop = 10, o2_unit = "percent_a.s.", o2_nh4_ratio, temp = 25, sal = 35, atm_pres = 1013.25){
	umol_o2 = conv_o2(o2 = o2_drop, from = o2_unit, to = 'umol_per_l', temp = temp, sal = sal, atm_pres = atm_pres)
	umol_nh4 = as.numeric(umol_o2) / o2_nh4_ratio
	mg_nh4 = conv_NH4(n_waste = umol_nh4, from = 'umol_NH4', to = 'mg_NH4')
	return(mg_nh4)
}