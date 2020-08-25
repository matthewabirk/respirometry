#' @title Calculate the oxygen supply capacity (alpha)
#' 
#' @description The oxygen supply capacity (\eqn{\alpha}) is a species- and temperature-specific value quantifying an animal's ability to extract oxygen from the ambient medium to support its metabolism (e.g. umol O2 / g / hr / kPa).
#' 
#' @param po2 a vector of PO2 values.
#' @param mo2 a vector of metabolic rate values. Must be the same length and corresponding to \code{po2}.
#' @param MR a vector of values for the metabolic rate at which \code{pcrit_alpha} should be returned. Default is \code{NULL}. If not specified, then \code{pcrit_alpha} is not returned and a message is added to the top of the return.
#' @param mo2_threshold a single numeric value above which \code{mo2} values are ignored. Useful to removing obviously erroneous values. Default is \code{Inf}.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Seibel et al. 2020 (in prep).
#' 
#' @return Returns a list of 1) alpha, 2) the PO2 and MO2 coordinates of the observation where alpha was reached, and 3) the Pcrit at a metabolic rate of \code{MR}.
#' @seealso \code{\link{calc_pcrit}}, \code{\link{plot_pcrit}}
#' 
#' @examples
#' mo2_data <- read.csv(system.file('extdata', 'mo2_v_po2.csv', package = 'respirometry'))
#' calc_alpha(po2 = mo2_data$po2, mo2 = mo2_data$mo2, MR = 1.5) # MR set to 1.5 to capture the 
#' # Pcrit corresponding to some of the lowest MO2 values recorded (something close to SMR).
#' 
#' @encoding UTF-8
#' @export

calc_alpha = function(po2, mo2, MR = NULL, mo2_threshold = Inf){
	alpha = mo2 / po2
	alpha = alpha[mo2 <= mo2_threshold]
	alpha_max = max(alpha, na.rm = TRUE)
	alpha_obs_idx = which(mo2 / po2 == alpha_max)[1]
	alpha_obs = c(po2[alpha_obs_idx], mo2[alpha_obs_idx])
	if(is.null(MR)) pcrit_alpha = message('MR must be specified to calculate Pcrit-alpha.') else pcrit_alpha = MR / alpha_max
	
	
	return(list(alpha = alpha_max, alpha_obs = alpha_obs, pcrit_alpha = pcrit_alpha))
}
