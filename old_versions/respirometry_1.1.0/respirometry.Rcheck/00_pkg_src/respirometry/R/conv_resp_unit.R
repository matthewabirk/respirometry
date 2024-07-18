#' Convert units related to respirometry
#'
#' @description Converts units of measurement that are joined by " / " or " * ". This function expands upon \code{\link[measurements]{conv_multiunit}} to incorporate O2 unit conversion and seawater volume-mass conversions.
#' 
#' @details
#' The O2 units supported by \code{\link{conv_o2}} should be appended with "_O2" (e.g. "kPa_O2"; even "percent_o2_O2") and O2 unit concentrations should drop "per_l" or "per_kg" (e.g. "umol_O2"). To designate seawater mass-volume conversion, append the unit with "_seawater" (e.g. "kg_seawater").
#'
#' @param value a numeric vector giving the measurement value in its original units.
#' @param from,to a string defining the unit with subunits separated by " / " or " * ". See Details for proper notation regarding O2 and seawater mass/volume.
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' @param o2_conc_base (optional) if converting between pO2 and [O2], should concentrations be "per_l" or "per_kg"? Default is "per_l".
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link[measurements]{conv_multiunit}}, \code{\link{conv_o2}}, \code{\link[seacarb]{rho}}
#'
#' @examples
#' # I read that an animal's MO2 is 1.92 ml O2/kg/min. What is this MO2 in umol O2/g/h?
#' conv_resp_unit(value = 1.92, from = "ml_O2 / kg / min", to = "umol_O2 / g / hr")
#' 
#' # Krogh's diffusion coefficient for oxygen through gills can be expressed as ml O2 / mm2 (gill
#' # surface area) / um (gill thickness) / torr (seawater pO2 - blood pO2) / minute at a given
#' # temperature.
#' # To convert to another unit:
#' conv_resp_unit(value = 1e-6, from = "ml_O2 / mm2 / um / torr / min",
#' to = "umol_O2 / cm2 / um / kPa / hr", temp = 20)
#' 
#' # Now, with a knowledge of gill morphometrics, seawater pO2, and blood pO2, I can compare
#' # gill diffusion with whole animal MO2.
#'
#' @encoding UTF-8
#' @export

conv_resp_unit = function(value, from, to, temp = 25, sal = 35, atm_pres = 1013.25, o2_conc_base = 'per_l'){
	if(!o2_conc_base %in% c('per_l', 'per_kg')) stop('"o2_conc_base" must be either "per_l" or "per_kg"')
	ops_f = gsub(pattern = '[^/\\*]', replacement = '', from)
	ops_t = gsub(pattern = '[^/\\*]', replacement = '', to)
	if(ops_f != ops_t) stop('The order the units in "from" and "to" must be equivalent.')
	ops = c(unlist(strsplit(ops_f, split = '')), '')
	froms = unlist(strsplit(from, split = ' / | \\* '))
	froms_bad = froms[!froms %in% c(unlist(measurements::conv_unit_options), grep('_O2$|_seawater$', froms, value = TRUE))]
	if(length(froms_bad) > 0) stop(paste(froms_bad, 'is not supported in the "from" argument. See conv_unit_options for supported units.'))
	tos = unlist(strsplit(to, split = ' / | \\* '))
	tos_bad = tos[!tos %in% c(unlist(measurements::conv_unit_options), grep('_O2$|_seawater$', tos, value = TRUE))]
	if(length(tos_bad) > 0) stop(paste(tos_bad, 'is not supported in the "to" argument. See conv_unit_options for supported units.'))
	o2_options = names(conv_o2())
	convs = sapply(1:length(froms), function(i){
		if(all(grepl('_O2$', c(froms[i], tos[i])))){
			inter1 = conv_o2(1, from = o2_options[which(paste0(gsub(paste0('_', o2_conc_base), '', o2_options), '_O2') == froms[i])], to = o2_options[which(paste0(gsub(paste0('_', o2_conc_base), '', o2_options), '_O2') == tos[i])], temp = temp, sal = sal, atm_pres = atm_pres)
		}
		if(all(grepl('_seawater$', c(froms[i], tos[i])))){
			inter1 = as.numeric(measurements::conv_dim(x = 1, x_unit = gsub('_seawater$', '', froms[i]), trans = seacarb::rho(S = sal, T = temp), trans_unit = 'kg_per_m3', y_unit = gsub('_seawater$', '', tos[i])))
		}
		if(all(!grepl('_O2$|_seawater$', c(froms[i], tos[i])))){
			inter1 = measurements::conv_unit(1, froms[i], tos[i])
		}
		inter1
	})
	conv_val = paste(c(rbind(convs, ops)), collapse = ' ')
	conv_val = eval(parse(text = conv_val))
	return(value * conv_val)
}