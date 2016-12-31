#' @title Convert between units of ammonium
#'
#' @description Ammonia or nitrogen excretion can be measured in a variety of ways. Convert between different measurements.
#'
#' @details
#' The sum of NH4+ and NH3 species are considered. Conversions are based on relationships and values from the package \code{\link[marelac]{marelac}}.
#'
#' @param n_waste a numeric vector of the ammonia or nitrogen value(s).
#' @param from a string describing the unit used to measure \code{n_waste}. Default is "umol_NH4" Options are:\itemize{
#' \item{umol_NH4}{}
#' \item{mg_NH4}{}
#' \item{mg_N}{}
#' }
#' @param to a single string either describing the unit to which the conversion should be conducted (options are the same as in \code{from}), or the string "all" to return all units.
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#'
#' @examples
#' conv_NH4(n_waste = 100)
#' conv_NH4(n_waste = 100, from = 'mg_N')
#' conv_NH4(n_waste = 100, from = 'mg_N', to = 'umol_NH4')
#'
#' @encoding UTF-8
#' @export

conv_NH4 = function(n_waste, from = "umol_NH4", to = "all"){
	if(from == 'umol_NH4') umol_NH4 = n_waste
	if(from == 'mg_NH4') umol_NH4 = measurements::conv_dim(x = n_waste, x_unit = 'mg', trans = marelac::atomicweight$N + marelac::atomicweight$H * 4, trans_unit = 'g_per_mol', y_unit = 'umol')
	if(from == 'mg_N') umol_NH4 = measurements::conv_dim(x = n_waste, x_unit = 'mg', trans = marelac::atomicweight$N, trans_unit = 'g_per_mol', y_unit = 'umol')

	x = list()
	if(to == 'umol_NH4' | to == 'all') x$umol_NH4 = umol_NH4
	if(to == 'mg_NH4' | to == 'all') x$mg_NH4 = measurements::conv_dim(x = umol_NH4, x_unit = 'umol', trans = marelac::atomicweight$N + marelac::atomicweight$H * 4, trans_unit = 'g_per_mol', y_unit = 'mg')
	if(to == 'mg_N' | to == 'all') x$mg_N = measurements::conv_dim(x = umol_NH4, x_unit = 'umol', trans = marelac::atomicweight$N, trans_unit = 'g_per_mol', y_unit = 'mg')
	if(to != 'all') x = unlist(x, use.names = FALSE)
	return(x)
}