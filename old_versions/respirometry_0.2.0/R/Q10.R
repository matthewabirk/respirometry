#' @title Parameters of Q10 Temperature Coefficient
#'
#' @description Returns the unknown parameter given 4 of 5 parameters from Q10 temperature coefficient calculation for chemical or biological systems.
#'
#' @details
#' Given four parameters, the fifth parameter will be returned.
#'
#' @param Q10 factor by which rate changes due to 10 °C increase in temperature.
#' @param R1 rate 1.
#' @param R2 rate 2.
#' @param T1 temperature 1 (in °C).
#' @param T2 temperature 2 (in °C).
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{scale_MO2}}
#'
#' @examples
#' Q10(R1 = 5, R2 = 10, T1 = 10, T2 = 20) # Returns Q10; = 2
#' Q10(Q10 = 2.66, R1 = 5, T1 = 10, T2 = 20) # Returns R2; = 13.3
#' 
#' # My species has an MO2 of 9.5 umol/g/h at 10 *C. What MO2 should I expect at 13 *C?
#' Q10(Q10 = 2, R1 = 9.5, T1 = 10, T2 = 13) # expect ~11.7 umol/g/h at 13 *C.
#' 
#' # A 100 g individual at 10 *C has an MO2 of 1270 umol/h. How much
#' # would a 250 g individual likely consume at 14 *C?
#' Q10(Q10 = 2, R1 = scale_MO2(mass_1 = 100, MO2_1 = 1270, mass_2 = 250)$MO2_2, T1 = 10, T2 = 14)
#' 
#' # Visualize MO2 scaling by mass and temperature:
#' mass = seq(10, 200, 10)
#' temp = 10:25
#' base_mass = 50
#' base_temp = 20
#' base_MO2 = 750
#' mo2 = outer(mass, temp, function(mass, temp){
#' 	scale_MO2(mass_1 = base_mass, mass_2 = mass, MO2_1 = Q10(Q10 = 2, R1 = base_MO2,
#' 	 T1 = base_temp, T2 = temp)$R2)$MO2_2
#' })
#' persp(mass, temp, mo2, xlab = 'Mass (g)', ylab = 'Temperature (*C)', zlab = 'MO2 (umol / hr)',
#'  theta = 35, phi = 15, expand = 0.5, ticktype = 'detailed', nticks = 10)
#'
#' @encoding UTF-8
#' @export

Q10 = function(Q10, R1, R2, T1, T2){
  q10 = methods::hasArg(Q10)
  r2 = methods::hasArg(R2)
  r1 = methods::hasArg(R1)
  t2 = methods::hasArg(T2)
  t1 = methods::hasArg(T1)
  if(sum(q10, r2, r1, t2, t1) < 4) stop('Four parameters are needed')
  if(sum(q10, r2, r1, t2, t1) == 5) stop('All parameters already provided. Nothing to calculate...')
  if(q10 == F){
    Q10 = list(Q10 = (R2 / R1)^(10 / (T2 - T1)))
    return(Q10)
  }
  if(r2 == F){
    R2 = list(R2 = Q10^((T2 - T1) / 10) * R1)
    return(R2)
  }
  if(r1 == F){
    R1 = list(R1 = Q10^((T1 - T2) / 10) * R2)
    return(R1)
  }
  if(t2 == F){
    T2 = list(T2 = 10 / log(Q10, base = R2 / R1) + T1)
    return(T2)
  }
  if(t1==F){
    T1 = list(T1 = 10 / log(Q10, base = R1 / R2) + T2)
    return(T1)
  }
}