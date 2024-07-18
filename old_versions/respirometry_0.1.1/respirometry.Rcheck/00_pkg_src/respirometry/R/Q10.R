#' @title Parameters of Q10 Temperature Coefficient
#'
#' @description Returns the unknown parameter given 4 of 5 parameters from Q10 temperature coefficient calculation for chemical or biological systems.
#'
#' @details
#' Given four parameters, the fifth parameter will be returned.
#'
#' @param Q10 factor by which rate changes due to 10 °C increase in temperature.
#' @param R2 rate 2.
#' @param R1 rate 1.
#' @param T2 temperature 2 (in °C).
#' @param T1 temperature 1 (in °C).
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#'
#' @examples
#' Q10(R2 = 10, R1 = 5, T2 = 20, T1 = 10) # Returns Q10; = 2
#' Q10(Q10 = 2.66, R1 = 5, T2 = 20, T1 = 10) # Returns R2; = 13.3
#'
#' @encoding UTF-8
#' @export

Q10 = function(Q10, R2, R1, T2, T1){
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