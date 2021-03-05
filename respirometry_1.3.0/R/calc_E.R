#' @title Calculate E temperature coefficient
#'
#' @description An E value is a relatively recent metric to parameterize the temperature-sensitivity of a biological rate (MO2). It is similar conceptually (but not numerically) to Q10.
#'
#' @details
#' E is the slope of the relationship between \code{-ln(x)} and \code{1/(kB T)}, where \code{kB} is the Boltzmann constant expressed in eV/K.
#'
#' @param x a numeric vector of rate values (e.g. MO2).
#' @param temp a numeric vector of temperature values (in Celsius).
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Deutsch, Curtis et al. 2015. “Climate Change Tightens a Metabolic Constraint on Marine Habitats.” Science 348(6239): 1132–35.
#' @seealso \code{\link{Q10}}
#'
#' @examples
#' calc_E(x = c(1, 2, 3), temp = c(10, 20, 30))
#'
#' @encoding UTF-8
#' @export


calc_E = function(x, temp){
	kb = 8.617333262145e-5
	df = data.frame(x = x, temp = temp)
	df$logx = log(df$x)
	df$kbt = 1 / (kb * (df$temp + 273.15))
	if(all(is.na(df$logx))) return(NA)
	return(-as.numeric(stats::coef(stats::lm(logx ~ kbt, data = df))['kbt']))
}
