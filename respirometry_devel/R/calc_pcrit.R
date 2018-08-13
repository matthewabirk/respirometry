pcrit_internal = function(po2, mo2, level = 0.95, iqr = 1.5){
	df = data.frame(po2 = po2, mo2 = mo2)
	
	# BREAKPOINT METHOD
	
	set.seed(8412)
	seg = segmented::segmented(lm(mo2 ~ po2, data = df), seg.Z = ~po2)
	breakpoint = seg$psi[, 'Est.']
	df_reg = df[which(po2 > breakpoint), ]
	df_reg = df_reg[which(abs(df_reg$mo2 - mean(df_reg$mo2, na.rm = TRUE)) < (IQR(df_reg$mo2) * iqr)), ]
	reg_lm = lm(mo2 ~ po2, data = df_reg)
	po2_range =  birk::range_seq(po2, by = 0.01)
	pi = stats::predict(reg_lm, newdata = data.frame(po2 = po2_range), interval = 'prediction', level = level)
	pi_ll = pi[, 'lwr']
	pi_ul = pi[, 'upr']
	
	conform_slope = segmented::slope(seg)$po2[1, 1]
	conform_int = segmented::intercept(seg)$po2[1, 1]
	conform = conform_slope * po2_range + conform_int
	
	sub_PI = po2_range[tail(which(conform < pi_ll), 1)]
	
	# # NONLINEAR REGRESSION METHOD (Marshall et al. 2013)
	# 
	# start_mo2 = median(mo2[po2 > (max(po2) / 4)])
	# m = 0.065
	# 
	# MM = function(po2, a, b) a * po2 / (b + po2)
	# MM_mod = nls(mo2 ~ MM(po2, a, b), data = df, start = list(a = start_mo2, b = 1))
	# MM_c = coef(MM_mod)
	# MM_deriv = MM_c['a'] * MM_c['b'] / (po2 + MM_c['b']) ^ 2
	# MM_pcrit = as.numeric(sqrt(MM_c['b'] / m) - MM_c['b']) #wrong
	# 
	# powr = function(po2, a, b) a * po2 ^ b
	# powr_mod = nls(mo2 ~ powr(po2, a, b), data = df, start = list(a = start_mo2, b = 1))
	# powr_c = coef(powr_mod)
	# powr_pcrit = (powr_c['a'] * m / powr_c['b']) ^ (1 / (powr_c['b'] - 1)) #wrong
	# 
	# hyperbola = function(po2, a, b, c) a * po2 / (b + po2) + c
	# hyperbola_mod = nls(mo2 ~ hyperbola(po2, a, b, c), data = df, start = list(a = start_mo2, b = 1, c = 0))
	# hyperbola_c = coef(hyperbola_mod)
	# hyperbola_pcrit = as.numeric(sqrt(hyperbola_c['b'] / m) - hyperbola_c['b']) #wrong
	# 
	# weibull = function(po2, a, b, c, d) a * (1 - exp(-(po2 / b) ^ c)) + d
	# weibull_mod = nls(mo2 ~ weibull(po2, a, b, c, d), data = df, start = list(a = start_mo2, b = 2, c = 1, d = 0))
	# 
	# best_mod = AIC(hyperbola_mod, MM_mod, powr_mod, weibull_mod)
	# best_mod = row.names(best_mod)[which.min(best_mod$AIC)]
	
	list(model = seg, breakpoint = breakpoint, sub_PI = sub_PI, reg_data = df_reg, po2_range = po2_range, pi_ll = pi_ll, pi_ul = pi_ul, conform = conform)
}







#' @title Title in title case
#' 
#' @description Briefly, here's what it does.
#' 
#' @details
#' In more depth, here's what it does but using less than 100 characters
#' per line.
#' 
#' @param x factor by which rate changes due to 10° C increase in temperature.
#' @param R2 rate 2.
#' @param R1 rate 1.
#' @param x1,y1 multiple at once.
#' @param T2 temperature 2 (in °C).
#' @param T1 temperature 1 (in °C).
#'
#' @return A data.frame with seven columns is returned.
#' \describe{
#' \item{TIME}{Date and time, POSIXct format.}
#' \item{ERROR_CODE}{Error code from transmitter. See user manual for translation of error code.}
#' }
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Here's an important note.
#' @references Birk and White (2013). Title.
#' @seealso \code{\link{conv_unit_options}}, \code{\link{conv_dim}}
#' 
#' @examples
#' Q10(R2 = 10, R1 = 5, T2 = 20, T1 = 10) # Returns Q10; = 2
#' Q10(Q10 = 2.66, R1 = 5, T2 = 20, T1 = 10) # Returns R2; = 13.3
#'
#' @encoding UTF-8
#' @export
#' 
#' 
calc_pcrit = function(po2, mo2, level = 0.95, iqr = 1.5){
	l = pcrit_internal(po2, mo2, level, iqr)
	return(c(Breakpoint = l$breakpoint, Sub_PI = l$sub_PI))
}







#' @title Title in title case
#' 
#' @description Briefly, here's what it does.
#' 
#' @details
#' In more depth, here's what it does but using less than 100 characters
#' per line.
#' 
#' @param x factor by which rate changes due to 10° C increase in temperature.
#' @param R2 rate 2.
#' @param R1 rate 1.
#' @param x1,y1 multiple at once.
#' @param T2 temperature 2 (in °C).
#' @param T1 temperature 1 (in °C).
#'
#' @return A data.frame with seven columns is returned.
#' \describe{
#' \item{TIME}{Date and time, POSIXct format.}
#' \item{ERROR_CODE}{Error code from transmitter. See user manual for translation of error code.}
#' }
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Here's an important note.
#' @references Birk and White (2013). Title.
#' @seealso \code{\link{conv_unit_options}}, \code{\link{conv_dim}}
#' 
#' @examples
#' Q10(R2 = 10, R1 = 5, T2 = 20, T1 = 10) # Returns Q10; = 2
#' Q10(Q10 = 2.66, R1 = 5, T2 = 20, T1 = 10) # Returns R2; = 13.3
#'
#' @encoding UTF-8
#' @export

plot_pcrit = function(po2, mo2, level = 0.95, iqr = 1.5, ...){
	l = pcrit_internal(po2, mo2, level, iqr)
	plot(l$model, res = TRUE, shade = TRUE, rug = FALSE, conf.level = level, main = paste0('Breakpoint = ', round(l$breakpoint, 3), '\nSub-PI = ', round(l$sub_PI, 3)), ...)
	points(l$reg_data$po2, l$reg_data$mo2, pch = 16, ...)
	lines(l$po2_range, l$pi_ll, lty = 2)
	lines(l$po2_range, l$pi_ul, lty = 2)
	points(x = l$sub_PI, y = l$conform[tail(which(l$conform < l$pi_ll), 1)], col = 'red', pch = 16)
	
	
	# weibull_coefs = coef(weibull_mod)
	# plot(df$po2, df$mo2)
	# curve(weibull(po2 = x, a = weibull_coefs['a'], b = weibull_coefs['b'], c = weibull_coefs['c'], d = weibull_coefs['d']), add = TRUE)
}
