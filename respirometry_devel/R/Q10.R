#' @title Parameters of Q10 Temperature Coefficient
#'
#' @description Calculates parameters from Q10 temperature coefficient for chemical or biological systems. This function can be used in two ways. 1. if four of the first five parameters are given (\code{Q10}, \code{R1}, \code{R2}, \code{T1}, \code{T2}) then the fifth parameter is returned, or 2. if \code{R_vec} and \code{T_vec} are given, then the best Q10 for those data is returned.
#'
#' @details
#' \deqn{Q_{10} = (R_2 / R_1) ^ {\frac{10}{T_2 - T_1}}}
#'
#' @param Q10 factor by which rate changes due to 10 °C increase in temperature.
#' @param R1 rate 1. Could also be Pcrit or any other temperature-dependent biological parameters.
#' @param R2 rate 2. Could also be Pcrit or any other temperature-dependent biological parameters.
#' @param T1 temperature 1 (in °C).
#' @param T2 temperature 2 (in °C).
#' @param R_vec a vector of temperature-dependent values, such as rates (e.g. MO2), Pcrit or other biological parameters.
#' @param T_vec a vector of temperature values (in °C).
#' @param model logical. If \code{TRUE}, then a list is returned which includes an linear model of \code{log10(R_vec)} and \code{T_vec} fit by \code{stats::lm()}. Default is \code{FALSE}.
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{scale_MO2}}, \code{\link{calc_E}}, \code{\link{adj_by_temp}}
#'
#' @examples
#' Q10(R1 = 5, R2 = 10, T1 = 10, T2 = 20) # Returns Q10; = 2
#' Q10(Q10 = 2.66, R1 = 5, T1 = 10, T2 = 20) # Returns R2; = 13.3
#' 
#' # My species has an MO2 of 9.5 umol/g/h at 10 *C. What MO2 should I expect at 13 *C?
#' Q10(Q10 = 2, R1 = 9.5, T1 = 10, T2 = 13) # expect ~11.7 umol/g/h at 13 *C.
#' 
#' # I measured MO2 at a spectrum of temperatures. What Q10 value best fits my data?
#' Q10(R_vec = c(1, 2, 5, NA, 18, 33), T_vec = c(0, 10, 20, 30, 40, 50))
#' 
#' # A 100 g individual at 10 *C has an MO2 of 1270 umol/h. How much
#' # would a 250 g individual likely consume at 14 *C?
#' Q10(Q10 = 2, R1 = scale_MO2(mass_1 = 100, MO2_1 = 1270, mass_2 = 250), T1 = 10, T2 = 14)
#' 
#' # Visualize MO2 scaling by mass and temperature:
#' mass <- seq(10, 200, 10)
#' temp <- 10:25
#' base_mass <- 50
#' base_temp <- 20
#' base_MO2 <- 750
#' mo2 <- outer(mass, temp, function(mass, temp){
#' 	scale_MO2(mass_1 = base_mass, mass_2 = mass, MO2_1 = Q10(Q10 = 2, R1 = base_MO2,
#' 	 T1 = base_temp, T2 = temp))
#' })
#' persp(mass, temp, mo2, xlab = 'Mass (g)', ylab = 'Temperature (*C)', zlab = 'MO2 (umol / hr)',
#'  theta = 35, phi = 15, expand = 0.5, ticktype = 'detailed', nticks = 10)
#'
#' @encoding UTF-8
#' @export

Q10 = function(Q10, R1, R2, T1, T2, R_vec, T_vec, model = FALSE){
	q10 = methods::hasArg(Q10)
	r2 = methods::hasArg(R2)
	r1 = methods::hasArg(R1)
	t2 = methods::hasArg(T2)
	t1 = methods::hasArg(T1)
	if(methods::hasArg(R_vec) & methods::hasArg(T_vec)){
		dat = data.frame(R_vec, T_vec)
		mod = stats::lm(log10(R_vec) ~ T_vec)
		inter = list(Q10 = as.numeric(10 ^ (stats::coef(mod)['T_vec'] * 10)), model = mod)
		ifelse(test = model, yes = return(inter), no = return(inter$Q10))
	}
	else{
		if(sum(q10, r2, r1, t2, t1) < 4) stop('Four parameters are needed')
		if(sum(q10, r2, r1, t2, t1) == 5) stop('All parameters already provided. Nothing to calculate...')
		if(q10 == FALSE){
			Q10 = (R2 / R1)^(10 / (T2 - T1))
			return(Q10)
		}
		if(r2 == FALSE){
			R2 = Q10^((T2 - T1) / 10) * R1
			return(R2)
		}
		if(r1 == FALSE){
			R1 = Q10^((T1 - T2) / 10) * R2
			return(R1)
		}
		if(t2 == FALSE){
			T2 = 10 / log(Q10, base = R2 / R1) + T1
			return(T2)
		}
		if(t1 == FALSE){
			T1 = 10 / log(Q10, base = R1 / R2) + T2
			return(T1)
		}
	}
}

#' @rdname Q10
#' @export
q10 = Q10

#' @rdname Q10
#' @export
calc_q10 = Q10