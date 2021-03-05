#' @title Calculate the oxygen supply capacity (alpha)
#' 
#' @description The oxygen supply capacity (\eqn{\alpha}) is a species- and temperature-specific value quantifying an animal's ability to extract oxygen from the ambient medium to support its metabolism (e.g. umol O2 / g / hr / kPa). This function calculates \eqn{\alpha} based on the single highest \eqn{\alpha0} (MO2/PO2) value in the dataset. If there are outliers that make this prohibitive, consider setting a threshold MO2 value with \code{mo2_threshold}.  
#' 
#' @param po2 a vector of PO2 values.
#' @param mo2 a vector of metabolic rate values. Must be the same length and corresponding to \code{po2}.
#' @param avg_top_n a numeric value representing the number of top \eqn{\alpha0} (MO2/PO2) values to average together to estimate \eqn{\alpha}. Default is 1. When analyzing a trial where the animal was not at MMR the whole time, we recommend no more than 3 to avoid diminishing the \eqn{\alpha} value with sub-maximal observations. If all observations are believed to be at maximal O2 supply capacity, \code{Inf} can be used to average all observations.
#' @param MR a vector of values for the metabolic rate at which \code{pcrit_alpha} should be returned. Default is \code{NULL}. If not specified, then \code{pcrit_alpha} is not returned and a message is added to the top of the return.
#' @param mo2_threshold a single numeric value above which \code{mo2} values are ignored. Useful to removing obviously erroneous values. Default is \code{Inf}.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Seibel, B. A., A. Andres, M. A. Birk, A. L. Burns, C. T. Shaw, A. W. Timpe, C. J. Welsh. 2021. “Oxygen supply capacity breathes new life into the critical oxygen partial pressure (Pcrit).” Journal of Experimental Biology.
#' 
#' @return Returns a list of 1) alpha, 2) a list of the PO2, MO2, and alpha0 value(s) where alpha was reached (the number of observations averaged is set by \code{avg_top_n}), and 3) the Pcrit at a metabolic rate of \code{MR}.
#' @seealso \code{\link{calc_pcrit}}, \code{\link{plot_pcrit}}
#' 
#' @examples
#' mo2_data <- read.csv(system.file('extdata', 'mo2_v_po2.csv', package = 'respirometry'))
#' calc_alpha(po2 = mo2_data$po2, mo2 = mo2_data$mo2, MR = 1.5) # MR set to 1.5 to capture the 
#' # Pcrit corresponding to some of the lowest MO2 values recorded (something close to SMR).
#' 
#' # extract the alpha0 values that were averaged together
#' sapply(calc_alpha(po2 = mo2_data$po2, mo2 = mo2_data$mo2, 
#'   MR = 1.5, avg_top_n = 3)$alpha_obs, function(i) i[3])
#' 
#' @encoding UTF-8
#' @export

calc_alpha = function(po2, mo2, avg_top_n = 1, MR = NULL, mo2_threshold = Inf){
	alpha0 = mo2 / po2
	alpha0 = alpha0[mo2 <= mo2_threshold]
	sort_alpha0 = sort(alpha0, decreasing = TRUE, na.last = TRUE, index.return = TRUE)
	if(avg_top_n > length(alpha0)){
		message('\"avg_top_n\" is larger than the number of observations in the dataset. Using the average of all the observations to determine alpha.')
		avg_top_n = length(alpha0)
	} 
	alpha = mean(sort_alpha0$x[1:avg_top_n], na.rm = TRUE)
	alpha_obs_idx = sort_alpha0$ix[1:avg_top_n]
	alpha_obs = lapply(alpha_obs_idx, function(i) c(po2 = po2[i], mo2 = mo2[i], alpha0 = alpha0[i]))
	if(is.null(MR)) pcrit_alpha = message('MR must be specified to calculate Pcrit-alpha.') else pcrit_alpha = MR / alpha
	
	
	return(list(alpha = alpha, alpha_obs = alpha_obs, pcrit_alpha = pcrit_alpha))
}
