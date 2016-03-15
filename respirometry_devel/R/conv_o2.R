# Created by Matthew A. Birk
# Dependencies: birk, marelac
# Converts % air saturation to other O2 units
# Last updated: Mar 2016

#' @title Convert between units of oxygen partial pressure and concentration
#'
#' @description Unfortunately, a consensus on the best way to express how much oxygen is in water has not been formed to date. Until then, this function converts between all commonly used forms of dissolved O2 measurement.
#'
#' @details
#' Conversions are based on relationships and values from the package \code{\link[marelac]{marelac}}.
#'
#' @param o2 a numeric vector of the O2 value(s). Default is 100.
#' @param from a string describing the unit used to measure \code{o2}. Default is "percent_a.s." Options are:\describe{
#' \item{percent_a.s. (percent air saturation)}{}
#' \item{percent_o2}{}
#' \item{hPa}{}
#' \item{kPa}{}
#' \item{torr}{}
#' \item{mmHg}{}
#' \item{inHg}{}
#' \item{mg_per_l}{}
#' \item{umol_per_l}{}
#' \item{ml_per_l}{}
#' }
#' @param to a single string either describing the unit to which the conversion should be conducted (options are the same as in \code{from}), or the string "all" to return all units.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#'
#' @examples
#' o2_unit_conv(o2 = 50)
#' o2_unit_conv(o2 = 1:50, from = "umol_per_l", to = "ml_per_l", temp = 10, sal = 0,
#' 	atm_pres = 1100)
#' o2_unit_conv()[c('mmHg','kPa')]
#'
#' @encoding UTF-8
#' @export
#' @import marelac
#' @import birk

conv_o2 = function(o2 = 100, from = 'percent_a.s.', to = 'all', temp = 25, sal = 35, atm_pres = 1013.25){
	air_pres = birk::conv_unit(atm_pres, 'mbar', 'bar')
	if(from == 'percent_a.s.') perc_a.s. = o2
	if(from == 'percent_o2') perc_a.s. = o2 / marelac::atmComp('O2')
	if(from == 'hPa') perc_a.s. = birk::conv_unit(o2, 'hPa', 'atm') * 100 / (air_pres - marelac::vapor(S = salinity, t = temp)) / marelac::atmComp('O2')
	if(from == 'kPa') perc_a.s. = birk::conv_unit(o2, 'kPa', 'atm') * 100 / (air_pres - marelac::vapor(S = salinity, t = temp)) / marelac::atmComp('O2')
	if(from == 'torr') perc_a.s. = birk::conv_unit(o2, 'torr', 'atm') * 100 / (air_pres - marelac::vapor(S = salinity, t = temp)) / marelac::atmComp('O2')
	if(from == 'mmHg') perc_a.s. = birk::conv_unit(o2, 'mmHg', 'atm') * 100 / (air_pres - marelac::vapor(S = salinity, t = temp)) / marelac::atmComp('O2')
	if(from == 'inHg') perc_a.s. = birk::conv_unit(o2, 'inHg', 'atm') * 100 / (air_pres - marelac::vapor(S = salinity, t = temp)) / marelac::atmComp('O2')
	if(from == 'mg_per_l') perc_a.s. = o2 * 100 / marelac::gas_satconc(S = salinity, t = temp, P = air_pres, species = 'O2') / 1e-6 / marelac::molweight('O2') / 1e3
	if(from == 'umol_per_l') perc_a.s. = o2 * 100 / marelac::gas_satconc(S = salinity, t = temp, P = air_pres, species = 'O2')
	if(from == 'ml_per_l') perc_a.s. = birk::conv_unit(o2, 'ml', 'l') / marelac::molvol(t = temp, P = air_pres, species = 'O2', quantity = 1 / birk::conv_unit(100 / marelac::gas_satconc(S = salinity, t = temp, P = air_pres, species = 'O2'), 'mol', 'umol'))
	
	x=list()
	if(to == 'percent_a.s.' | to == 'all') x$percent_a.s. = perc_a.s.
	if(to == 'percent_o2' | to == 'all') x$percent_o2 = marelac::atmComp('O2') * perc_a.s.
	if(to == 'hPa' | to == 'all') x$hPa = birk::conv_unit((air_pres - marelac::vapor(S = salinity, t = temp)) * marelac::atmComp('O2') * perc_a.s. / 100, 'atm', 'hPa')
	if(to == 'kPa' | to == 'all') x$kPa = birk::conv_unit((air_pres - marelac::vapor(S = salinity, t = temp)) * marelac::atmComp('O2') * perc_a.s. / 100, 'atm', 'kPa')
	if(to == 'torr' | to == 'all') x$torr = birk::conv_unit((air_pres - marelac::vapor(S = salinity, t = temp)) * marelac::atmComp('O2') * perc_a.s. / 100, 'atm', 'torr')
	if(to == 'mmHg' | to == 'all') x$mmHg = birk::conv_unit((air_pres - marelac::vapor(S = salinity, t = temp)) * marelac::atmComp('O2') * perc_a.s. / 100, 'atm', 'mmHg')
	if(to == 'inHg' | to == 'all') x$inHg = birk::conv_unit((air_pres - marelac::vapor(S = salinity, t = temp)) * marelac::atmComp('O2') * perc_a.s. / 100, 'atm', 'inHg')
	if(to == 'mg_per_l' | to == 'all') x$mg_per_l = marelac::gas_satconc(S = salinity, t = temp, P = air_pres, species = 'O2') * 1e-6 * marelac::molweight('O2') * 1e3 * perc_a.s. / 100
	if(to == 'umol_per_l' | to == 'all') x$umol_per_l = marelac::gas_satconc(S = salinity, t = temp, P = air_pres, species = 'O2') * perc_a.s. / 100
	if(to == 'ml_per_l' | to == 'all') x$ml_per_l = birk::conv_unit(as.numeric(marelac::molvol(t = temp, P = air_pres, species = 'O2', quantity = birk::conv_unit(marelac::gas_satconc(S = salinity, t = temp, P = air_pres, species = 'O2') * perc_a.s. / 100, 'umol', 'mol'))), 'l', 'ml')
	attr(x$percent_o2, 'names') = NULL
	attr(x$hPa, 'names') = NULL
	attr(x$kPa, 'names') = NULL
	attr(x$torr, 'names') = NULL
	attr(x$mmHg, 'names') = NULL
	attr(x$inHg, 'names') = NULL
	attr(x$mg_per_l, 'names') = NULL
	attr(x$umol_per_l, 'names') = NULL
	attr(x$ml_per_l, 'names') = NULL
	return(x)
}