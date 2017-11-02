#' @title Predict NH3 / NH4+ concentration post-respiration
#' 
#' @description Predicts the [NH3] and [NH4+] of seawater after a defined amount of oxygen consumption. Ammonotelic animals excrete the ionized form NH4+ (ammonium) but some of these ions dissociate into unionized NH3 (ammonia) which is toxic for most fishes and crustaceans around 0.4-2.0 mg/L (Boyd 2012).
#' 
#' @details
#' Given a known amount of oxygen consumed and an estimated O2:N ratio, the amount of NH4 produced can be estimated. Production or consumption of ammonium by "background" microbes or conversion of ammonium to nitrite and nitrate is ignored since bacteria in the respirometer are typically sought to be in low levels. The amount of dissociation to produce ammonia is calculated by \code{\link[seacarb]{Kn}}.
#' 
#' @param o2_drop a numeric value or vector describing the change in O2. Default is 10.
#' @param o2_unit a string describing the unit used to measure \code{o2_drop}. Default is "percent_a.s." Options are from \code{\link{conv_o2}}.
#' @param o2_nh4_ratio molar ratio of O2 consumed to NH4+ produced.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param pH seawater pH (total scale). Default is 8.1.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @return A list containing the predicted NH3, NH4+, and TAN produced in mg/l.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Boyd C. 2012. Water Quality. In "Aquaculture: Farming Aquatic Animals and Plants". Blackwell Publishing, Ltd.
#' @seealso \code{\link{conv_o2}}, \code{\link{conv_nh4}}, \code{\link[seacarb]{Kn}}
#' 
#' @examples
#' predict_nh3(o2_drop = 25, o2_nh4_ratio = 10)
#' 
#' @encoding UTF-8
#' @export


predict_nh3 = function(o2_drop = 10, o2_unit = "percent_a.s.", o2_nh4_ratio, temp = 25, sal = 35, pH = 8.1, atm_pres = 1013.25){
	umol_o2 = conv_o2(o2 = o2_drop, from = o2_unit, to = 'umol_per_l', temp = temp, sal = sal, atm_pres = atm_pres)
	umol_tan = as.numeric(umol_o2) / o2_nh4_ratio
	kn = seacarb::Kn(S = sal, T = temp, pHscale = 'T')
	umol_nh3 = umol_tan * kn / (10^(-pH) + kn)
	umol_nh4 = umol_tan * 10^(-pH) / (kn + 10^(-pH))
	mg_nh3 = conv_nh4(n_waste = umol_nh3, from = 'umol_NH3', to = 'mg_NH3')
	mg_nh4 = conv_nh4(n_waste = umol_nh4, from = 'umol_NH4', to = 'mg_NH4')
	mg_n = list(mg_NH3 = mg_nh3, mg_NH4 = mg_nh4, mg_TAN = mg_nh3 + mg_nh4, umol_TAN = umol_tan)
	return(mg_n)
}