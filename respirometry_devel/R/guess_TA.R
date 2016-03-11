#' @title Estimate total alkalinity from salinity
#' 
#' @description Estimate total alkalinity from salinity and temperature of seawater according to Lee et al. 2006. Useful when a rough guess of TA is needed because measuring TA is not possible or practical.
#' 
#' @details
#' Estimates total alkalinity using the equations provided by Lee et al. 2006 (Geophysical Research Letters). For improved estimate accuracy, the geographic region can be provided. The North Pacific region is longitude-dependent so a longitude of 150 °W is assumed which provides a typical value within the range.
#' 
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param region (optional) geographic region. Options are "(Sub)tropics", "Equatorial Upwelling Pacific", "North Atlantic", "North Pacific", and "Southern Ocean". Default is NULL. If undefined, the average from all these regions is used.
#' 
#' @return An estimate of the total alkalinity (umol / kg).
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Lee K, Tong LT, Millero FJ, Sabine CL, Dickson AG, Goyet C, Park G-H, Wanninkhof R, Feely RA, Key RM. 2006. Global relationships of total alkalinity with salinity and temperature in surface waters of the world’s oceans. Geophys Res Lett. 33:L19605.
#' @seealso \code{\link{predict_pH}}
#' 
#' @examples
#' guess_TA(temp = 22, sal = 33)
#' guess_TA(temp = 22, sal = 33, region = "North Atlantic")
#' 
#' @encoding UTF-8
#' @export

guess_TA = function(temp = 25, sal = 35, region = NULL){
  TA_df = data.frame(region = '(Sub)tropics', TA = 2305 + 58.66 * (sal - 35) + 2.32 * (sal - 35)^2 - 1.41 * (temp - 20) + 0.040 * (temp - 20)^2)
  TA_df = rbind(TA_df, data.frame(region = 'Equatorial Upwelling Pacific', TA = 2294 + 64.88 * (sal - 35) + 0.39 * (sal - 35)^2 - 4.52 * (temp - 29) - 0.232 * (temp - 29)^2))
  TA_df = rbind(TA_df, data.frame(region = 'North Atlantic', TA = 2305 + 53.97 * (sal - 35) + 2.74 * (sal - 35)^2 - 1.16 * (temp - 20) - 0.040 * (temp - 20)^2))
  TA_df = rbind(TA_df, data.frame(region = 'North Pacific', TA = 2305 + 53.23 * (sal - 35) + 1.85 * (sal - 35)^2 - 14.72 * (temp - 20) - 0.158 * (temp - 20)^2 + 0.062 * (temp - 20) * 150))
  TA_df = rbind(TA_df, data.frame(region = 'Southern Ocean', TA = 2305 + 52.48 * (sal - 35) + 2.85 * (sal - 35)^2 - 0.49 * (temp - 20) + 0.086 * (temp - 20)^2))
  
  if(!is.null(region)) TA = TA_df[which(TA_df$region == region), 'TA'] else TA = mean(TA_df$TA)
  return(TA) # umol/kg
}