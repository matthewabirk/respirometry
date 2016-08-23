#' @title Estimate total alkalinity from salinity
#' 
#' @description Estimate total alkalinity from salinity and temperature of surface seawater according to Lee et al. 2006. Useful when a rough guess of TA is needed because measuring TA is not possible or practical.
#' 
#' @details
#' \describe{
#' 	\item{(Sub)tropics}{\code{temp} \eqn{\ge} 20 and 31 \eqn{\le} \code{sal} \eqn{\le} 38}
#' 	\item{Equatorial Upwelling Pacific}{\code{temp} \eqn{\ge} 18 and 31 \eqn{\le} \code{sal} \eqn{\le} 36.5}
#' 	\item{North Atlantic}{0 \eqn{\le} \code{temp} \eqn{\le} 20 and 31 \eqn{\le} \code{sal} \eqn{\le} 37}
#' 	\item{North Pacific}{\code{temp} \eqn{\le} 20 and 31 \eqn{\le} \code{sal} \eqn{\le} 35}
#' 	\item{Southern Ocean}{\code{temp} \eqn{\le} 20 and 33 \eqn{\le} \code{sal} \eqn{\le} 36}
#' }
#' Estimates total alkalinity using the equations provided by Lee et al. 2006 (Geophysical Research Letters). While these equations are designed for open ocean environments, they can provide a rough estimate even for coastal environments. For improved estimate accuracy, the geographic region can be provided. The North Pacific region is longitude-dependent so a longitude of 150 °W is assumed which provides a typical value within the range. Only applicable for surface waters, not very accurate for the ocean interior.
#' 
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu. 31 \eqn{\le} \code{sal} \eqn{\le} 38; may be narrower for some regions.
#' @param region (optional) geographic region. Options are "(Sub)tropics", "Equatorial Upwelling Pacific", "North Atlantic", "North Pacific", and "Southern Ocean". Default is NULL. If undefined, the average from all these regions is used.
#' 
#' @return An estimate of the total alkalinity (umol / kg). If \code{NA} or \code{NaN} are returned, confirm the \code{temp} and \code{sal} values are within acceptable ranges for the region of interest.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Lee K, Tong LT, Millero FJ, Sabine CL, Dickson AG, Goyet C, Park G-H, Wanninkhof R, Feely RA, Key RM. 2006. Global relationships of total alkalinity with salinity and temperature in surface waters of the world’s oceans. Geophys Res Lett. 33:L19605.
#' @seealso \code{\link{predict_pH}}
#' 
#' @examples
#' guess_TA(temp = 22, sal = 33)
#' guess_TA(temp = 12, sal = 33, region = "North Atlantic")
#' guess_TA(temp = 20, sal = 31:35)
#' 
#' @encoding UTF-8
#' @export

guess_TA = function(temp = 25, sal = 35, region = NULL){
	TA_df = data.frame(temp = temp, sal = sal)
  TA_df[, '(Sub)tropics'] = 2305 + 58.66 * (sal - 35) + 2.32 * (sal - 35)^2 - 1.41 * (temp - 20) + 0.040 * (temp - 20)^2
  TA_df[temp < 20 | sal < 31 | sal > 38, '(Sub)tropics'] = NA
  
  TA_df[, 'Equatorial Upwelling Pacific'] = 2294 + 64.88 * (sal - 35) + 0.39 * (sal - 35)^2 - 4.52 * (temp - 29) - 0.232 * (temp - 29)^2
  TA_df[temp < 18 | sal < 31 | sal > 36.5, 'Equatorial Upwelling Pacific'] = NA
  
  TA_df[, 'North Atlantic'] = 2305 + 53.97 * (sal - 35) + 2.74 * (sal - 35)^2 - 1.16 * (temp - 20) - 0.040 * (temp - 20)^2
  TA_df[temp < 0 | temp > 20 | sal < 31 | sal > 37, 'North Atlantic'] = NA
  
  TA_df[, 'North Pacific'] = 2305 + 53.23 * (sal - 35) + 1.85 * (sal - 35)^2 - 14.72 * (temp - 20) - 0.158 * (temp - 20)^2 + 0.062 * (temp - 20) * 150
  TA_df[temp > 20 | sal < 31 | sal > 35, 'North Pacific'] = NA
  
  TA_df[, 'Southern Ocean'] = 2305 + 52.48 * (sal - 35) + 2.85 * (sal - 35)^2 - 0.49 * (temp - 20) + 0.086 * (temp - 20)^2
  TA_df[temp > 20 | sal < 33 | sal > 36, 'Southern Ocean'] = NA
  
  if(!is.null(region)) TA = TA_df[, region] else TA = rowMeans(TA_df[, -(which(colnames(TA_df) %in% c('temp', 'sal')))], na.rm = TRUE)
  return(TA) # umol/kg
}