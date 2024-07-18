#' @title Estimate when the O2 level will reach a defined level
#' 
#' @description Estimates the time at which O2 will reach a defined level assuming a linear change in O2 over time.
#' 
#' @param past_o2 a numeric vector of at least two oxygen measurements previously during the trial.
#' @param past_time a vector of timepoints corresponding to when \code{past_o2} values were recorded. Can be a numeric vector for duration since trial began or a POSIX vector of time values.
#' @param goal_o2 a numeric vector or single value describing the O2 level of interest.
#' @param plot logical. Do you want to see a plot to visualize this prediction?
#'
#' @return A prediction of the time when O2 will reach \code{goal_o2}. If \code{past_time} is numeric, then a numeric value(s) will be returned. If POSIX, then POSIX will be returned.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Viewing the plot can be valuable if the O2 consumption or production is not linear.
#' @seealso \code{\link{predict_pH}}, \code{\link{predict_NH4}}
#' 
#' @examples
#' guess_when(past_o2 = rnorm(n = 10, mean = 100:91), past_time = 1:10, goal_o2 = 75, plot = FALSE)
#' guess_when(past_o2 = rnorm(n = 10, mean = 100:91, sd = 10), past_time = 1:10, goal_o2 = 75)
#' # Viewing the plot can be helpful to see how trustworthy the prediction is
#' # when signal:noise is low.
#'
#' @encoding UTF-8
#' @export

guess_when = function(past_o2, past_time, goal_o2, plot = TRUE){
	rate = stats::lm(past_o2 ~ past_time)
	when = (goal_o2 - unname(stats::coef(rate)['(Intercept)'])) / unname(stats::coef(rate)['past_time'])
	if(plot == TRUE){
		plot(past_time, past_o2, type = 'o', xlim = range(c(past_time[1], when)), ylim = range(c(goal_o2, max(past_o2))), xlab = 'Time', ylab = expression('O'[2]))
		graphics::abline(rate, col = 'red')
		graphics::points(x = when, y = goal_o2, pch = 16, col = 'red')
	}
	if(lubridate::is.POSIXct(past_time)) when = as.POSIXct(when, origin = '1970-01-01')
	return(when)
}