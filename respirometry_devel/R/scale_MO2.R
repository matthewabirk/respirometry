#' @title Mass-correct metabolic rate
#' 
#' @description For most organisms, metabolic rate does not scale linearly, but rather according to a power curve. This function estimates MO2 or size of an individual organism given the MO2 and size of another individual of a different size. To mass-correct your MO2 data, plug in your desired mass in \code{mass_2} and the output from \code{\link{calc_b}} to the \code{b} parameter.
#' 
#' @details
#' \deqn{(MO2 = b0 * M^b)}
#' where \code{b0} is species-specific normalization constant, \code{M} is mass and \code{b} is the scaling coefficient which is around 0.75 for many organisms.
#' 
#' For scaling of \strong{mass-specific} metabolic rates, use something closer to \code{b = -0.25} rather than \code{b = 0.75}.
#' 
#' @param mass_1 animal mass for \code{MO2_1}.
#' @param MO2_1 metabolic rate for \code{mass_1}.
#' @param mass_2 animal mass for \code{MO2_2}.
#' @param MO2_2 metabolic rate for \code{mass_2}.
#' @param b scaling coefficient for MO2. Default is 0.75.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{Q10}}, \code{\link{calc_b}}
#' 
#' @examples
#' # I know a species has an SMR of 800 umol O2/h at 200 g.
#' # What would be a likely SMR for a 300 g individual?
#' scale_MO2(mass_1 = 200, MO2_1 = 800, mass_2 = 300)
#' 
#' # Some squids have a much higher scaling coefficient:
#' scale_MO2(mass_1 = 200, MO2_1 = 800, mass_2 = 300, b = 0.92)
#' 
#' # A 100 g individual at 10 *C has an MO2 of 1270 umol/h. How much
#' # would a 250 g individual likely consume at 14 *C?
#' Q10(Q10 = 2, R1 = scale_MO2(mass_1 = 100, MO2_1 = 1270, mass_2 = 250), T1 = 10, T2 = 14)
#' 
#' # Now I have data from real animals and I want to mass-correct them all to a 10 g animal.
#' mass = 2:20 # obviously not real but you get the point
#' mo2 = c(44.8, 41, 36, 35, 35, 33.5, 34.5, 40, 30, 23, 27, 30, 25.6, 27.8, 28, 24, 27, 28, 20)
#' desired_mass = 10
#' 
#' b = calc_b(mass = mass, MO2 = mo2)
#' scale_MO2(mass_1 = mass, MO2_1 = mo2, mass_2 = desired_mass, b = b$b)
#' 
#' plot(mass, mo2, ylab = 'Raw MO2') # before
#' plot(mass, scale_MO2(mass_1 = mass, MO2_1 = mo2, mass_2 = 10, b = b$b),
#' ylab = 'Mass-corrected MO2') # after
#' 
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

scale_MO2 = function(mass_1, MO2_1, mass_2, MO2_2, b = 0.75){
	if(sum(missing(mass_1), missing(MO2_1), missing(mass_2), missing(MO2_2)) != 1) stop('3 of 4 of the parameters must be provided.')
	if(missing(mass_1)){
		b0 = MO2_2 / mass_2 ^ b
		mass_1 = (MO2_1 / b0) ^ (1/b)
		return(mass_1)
	}
	if(missing(MO2_1)){
		b0 = MO2_2 / mass_2 ^ b
		MO2_1 = b0 * mass_1 ^ b
		return(MO2_1)
	}
	if(missing(mass_2)){
		b0 = MO2_1 / mass_1 ^ b
		mass_2 = (MO2_2 / b0) ^ (1/b)
		return(mass_2)
	}
	if(missing(MO2_2)){
		b0 = MO2_1 / mass_1 ^ b
		MO2_2 = b0 * mass_2 ^ b
		return(MO2_2)
	}
}

#' @rdname scale_MO2
#' @export
scale_mo2 = scale_MO2