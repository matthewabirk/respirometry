#' @title Predict biological parameters at a new temperature
#' 
#' @description Predicts the values of any inputted biological parameter (e.g. MO2, Pcrit) at a new temperature based on empirical measurements at a range of temperatures. Data can be fit to a temperature-dependence curve using either \code{\link{Q10}} or \code{\link{calc_E}}. By default, the predicted values are also plotted alongside the inputted data to allow the user to assess the quality of the fit.
#' 
#' @param meas_temp a vector of temperature values (°C) corresponding to \code{meas_x}.
#' @param meas_x a vector of biological values (e.g. MO2, Pcrit) corresponding to \code{meas_temp}. These are typically empirically derived.
#' @param temp_new a vector of temperature values (°C) at which new values of "x" should be predicted.
#' @param method which method for calculcating temperature-dependency should be used? Options are "Q10" (default) and "E". If either \code{Q10} or \code{E} parameters have values, then this parameter is ignored.
#' @param Q10 (optional). A Q10 value to be used for predicting new values of "x". If \code{method = "Q10"} and Q10 is not defined here, then an appropriate Q10 value will be calculated internally using \code{\link{Q10}}.
#' @param E (optional). An E value to be used for predicting new values of "x". If \code{method = "E"} and E is not defined here, then an appropriate E value will be calculated internally using \code{\link{calc_E}}.
#' @param show_coef logical. Should the temperature-dependency coefficient (i.e. the numeric value of either Q10 or E) be returned alongside the new values of "x"? Default is \code{FALSE}.
#' @param plot_fit logical. Should a plot be displayed showing how well the new "x" values fit with the inputted data? Default is \code{TRUE}.
#'
#' @return If \code{show_coef = FALSE} (default), then a numeric vector of new values of "x" are returned.
#' If \code{show_coef = TRUE}, then a list of new values and the temperature-dependency coefficient are returned.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{Q10}}, \code{\link{calc_E}}
#' 
#' @examples
#' # I measured Pcrit at four different temperatures. What is the Pcrit at an
#' # intermediate temperature?
#' adj_by_temp(meas_temp = c(5, 10, 15, 20), meas_x = c(3.1, 6.3, 7, 8.4), temp_new = 18)
#'
#' # If requested values exceed the inputted temperature range, a message is reported.
#' # Biology cannot go on forever like the math can.
#' adj_by_temp(meas_temp = c(10, 15, 20, 25), meas_x = c(4.8, 6, 12.3, 13.6), temp_new = 0:30)
#'
#' @encoding UTF-8
#' @export

adj_by_temp = function(meas_temp, meas_x, temp_new, method = 'Q10', Q10, E, show_coef = FALSE, plot_fit = TRUE){
	if(length(meas_temp) != length(meas_x)) stop('meas_temp and meas_x are not the same length. These must be corresponding values.')
	if(length(unique(stats::na.omit(meas_temp))) == 1) stop('Values at 2+ temperatures must be inputted in order to calculate a temperature-dependency curve.')
	if(methods::hasArg(E)) method = 'E'
	if(methods::hasArg(Q10)) method = 'Q10'
	if(method == 'Q10'){
		if(!missing(Q10)){
			x_new = mean(Q10(Q10 = Q10, R1 = meas_x, T1 = meas_temp, T2 = temp_new), na.rm = TRUE)
		}else{
			tmp = respirometry::Q10(R_vec = meas_x, T_vec = meas_temp, model = TRUE)
			Q10 = tmp$Q10
			x_new = as.numeric(10 ^ stats::predict(tmp$model, newdata = data.frame(T_vec = temp_new)))
		}
	}
	if(method == 'E'){
		kb = 8.61733e-5
		if(missing(E)) E = calc_E(x = meas_x, temp = meas_temp)
		x_int = mean(log(meas_x) + E * (1 / (kb * (meas_temp + 273.15))), na.rm = TRUE)
		x_new = exp(x_int - E * 1 / (kb * (temp_new + 273.15)))
	}
	if(min(temp_new, na.rm = TRUE) < min(meas_temp, na.rm = TRUE) | max(temp_new, na.rm = TRUE) > max(meas_temp, na.rm = TRUE)) message('You are extrapolating beyond the temperature range from your input data. Take caution that values outside the temperature range for which you have data may not be realistic.')
	if(show_coef){
		if(method == 'Q10') return(list(new_values = x_new, Q10 = Q10))
		if(method == 'E') return(list(new_values = x_new, E = E))
	}
	if(plot_fit){
		temp_value = round(ifelse(method == 'Q10', Q10, E), 3)
		plot(temp_new, x_new, 
				 col = 'red',
				 xlim = c(min(temp_new, meas_temp, na.rm = TRUE), max(temp_new, meas_temp, na.rm = TRUE)),
				 ylim = c(min(x_new, meas_x, na.rm = TRUE), max(x_new, meas_x, na.rm = TRUE)),
				 xlab = 'Temperature',
				 ylab = ''
				 )
		graphics::lines(temp_new, x_new, col = 'red')
		graphics::points(meas_temp, meas_x, pch = 16)
		graphics::title(main = 'Black points are inputted values.', line = 2)
		graphics::title(main = paste0('Red points are based on ', method, ' = ', temp_value), col.main = 'red', line = 1)
		# title line here saying black is input data and red is output data
	}
	return(x_new)
}
