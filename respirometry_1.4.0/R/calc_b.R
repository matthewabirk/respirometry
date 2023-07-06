#' @title Calculate the metabolic scaling coefficient, b
#' 
#' @description For most organisms, metabolic rate does not scale linearly, but rather according to a power function: \eqn{MO2 = b0 * M^b}. This function estimates the scaling coefficient, \code{b}, and normalization constant, \code{b0}, given MO2s from different sized individuals.
#' 
#' @details
#' \deqn{MO2 = b0 * M^b}
#' where \code{b0} is species-specific normalization constant, \code{M} is mass and \code{b} is the scaling coefficient.
#' 
#' @param mass a vector of animal masses.
#' @param MO2 a vector of metabolic rates.
#' @param method a string defining which method of calculating scaling coefficients to use. Default is "nls", which utilizes a nonlinear least squares regression. If this does not fit your data well, "lm" may also be used, which calculates a linear regression of log10(\code{MO2}) ~ log10(\code{mass}) with slope and intercept equivalent to \code{b} and 10^\code{b0}, respectively.
#' @param plot a string defining what kind of plot to display. "linear" for linear axes, "log" for log10-scale axes, and "none" for no plot. Default is "linear".
#' @param b0_start a single numeric value as the starting point for \code{b0} value determination using the "nls" method. The default is \code{1} and should work for most situations, but if the "nls" method is not working and you don't want to use the "lm" method, changing the starting \code{b0} value may help. Ignored when method = "lm".
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @return Returns a list of 1) the \code{b} value, 2) a vector of \code{b0} values corresponding to the input \code{MO2} values, and 3) an average \code{b0} that can be used for summarizing the relationship with an equation.
#' @seealso \code{\link{scale_MO2}}, \code{\link{calc_MO2}}
#' 
#' @examples
#' # Simple example
#' mass <- c(1, 10, 100, 1000, 40, 4, 400, 60, 2, 742, 266, 983) # made up values
#' MO2 <- mass ^ 0.65 + rnorm(n = length(mass)) # make up some data
#' calc_b(mass = mass, MO2 = MO2)
#' 
#' # How about some mass-specific MO2s?
#' msMO2 <- mass ^ -0.25 + rnorm(n = length(mass), sd = 0.05)
#' calc_b(mass = mass, MO2 = msMO2)
#' calc_b(mass = mass, MO2 = msMO2, plot = "log")
#' 
#' @encoding UTF-8
#' @export

calc_b = function(mass, MO2, method = 'nls', plot = 'linear', b0_start = 1){
	if(!(plot %in% c('linear', 'log', 'none'))) stop('"plot" must be "linear", "log", or "none"')
	if(!(method %in% c('nls', 'lm'))) stop('"method" must be "nls" or "lm"')
	if(method == 'nls'){
		b_model = tryCatch(stats::nls(MO2 ~ b0 * mass ^ b, start = list(b0 = b0_start, b = 0.75)), error = function(e) stats::nls(MO2 ~ b0 * mass ^ b, start = list(b0 = b0_start, b = -0.25)))
		b = unname(stats::coef(b_model)['b'])
		b0 = MO2 / mass ^ b
		b0_avg = mean(b0, na.rm = TRUE)
	}
	if(method == 'lm'){
		if(any(MO2 == 0)) MO2[which(MO2 == 0)] = NA
		if(any(mass == 0)) mass[which(mass == 0)] = NA
		b_model = stats::lm(log10(MO2) ~ log10(mass))
		b = unname(stats::coef(b_model)['log10(mass)'])
		b0 = MO2 / mass ^ b
		b0_avg = 10 ^ unname(stats::coef(b_model)['(Intercept)'])
	}
	if(plot != 'none'){
		plot_scale = switch(plot, linear = '', log = 'xy')
		graphics::plot(mass, MO2, log = plot_scale)
		mass_range = birk::range_seq(mass, length.out = 1000)
		pred = stats::predict(b_model, newdata = data.frame(mass = mass_range))
		if(method == 'lm') pred = 10 ^ pred
		graphics::lines(mass_range, pred)
	}
	return(list(b = b, b0 = b0, b0_avg = b0_avg))
}
