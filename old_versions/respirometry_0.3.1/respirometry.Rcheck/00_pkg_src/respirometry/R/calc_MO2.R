#' @title Calculate metabolic rate
#' 
#' @description Calculates metabolic rate (MO2) given O2 measurements over time. Oxygen measurements are split into bins and MO2s are calculated from each bin (unless \code{bin_width} is set to \code{0}). The \code{bin_width} parameter defines the width of the bin in timed intervals (e.g. 15 minutes). Linear regressions are fit through each bin and the calculated MO2 is returned as the slope of the change in O2 over time.
#' 
#' @param duration numeric vector of the timepoint for each observation (minutes).
#' @param o2 numeric vector of O2 observations.
#' @param o2_unit a string describing the unit used to measure \code{o2}. Default is "percent_a.s." Options are from \code{\link{conv_o2}}.
#' @param bin_width numeric. A number defining how long of a period should be binned for each MO2 determination (minutes). If MO2 is to be calculated from one observation to the next (rather than binned observations), set \code{bin_width} to 0. To calculate one MO2 value from all observations, set \code{bin_width} to \code{Inf}.
#' @param vol volume of the respirometer (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' @param good_data logical vector of whether O2 observations are "good" measurements and should be included in analysis. Default is that all observations are \code{TRUE}.
#'
#' @return A data frame with seven columns is returned:
#' \describe{
#' \item{MEAN_O2}{Mean O2 value of the bin in the unit chosen by \code{o2_unit}).}
#' \item{O2_RANGE}{Range of O2 values in the bin.}
#' \item{MEAN_DURATION}{Mean duration of the bin (minutes).}
#' \item{DUR_RANGE}{Range of duration timepoints in the bin.}
#' \item{MO2}{Metabolic rate (umol O2 / hour).}
#' \item{R2}{Coefficient of determination for the linear regression fit to calculate MO2.}
#' \item{N}{Number of observations in the bin.}
#' }
#' 
#' @note If only beginning and ending O2 observations are known, consider using \code{\link{closed}}. Both functions will work fine, but \code{\link{closed}} is simpler.
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{calc_b}}, \code{\link{closed}}, \code{\link{scale_MO2}}, \code{\link{conv_resp_unit}}
#' 
#' @examples
#' # get O2 data
#' file <- system.file('extdata', 'witrox_file.txt', package = 'respirometry')
#' o2_data <- na.omit(import_witrox(file, split_channels = TRUE)$CH_4)
#' 
#' # calculate MO2
#' (mo2_5_min <- calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
#' bin_width = 5, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL))
#' 
#' # what if measurements from the 10 to 12 minute marks can't be trusted?
#' bad_data = o2_data$DURATION >= 10 & o2_data$DURATION <= 12
#' (mo2_5_min <- calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
#' bin_width = 5, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL, good_data = !bad_data))
#' 
#' # easily make a Pcrit plot
#' plot(mo2_5_min$MEAN_O2, mo2_5_min$MO2)
#' 
#' # I want to express MO2 in mg per min instead.
#' (mo2_5_min$MO2 <- conv_resp_unit(value = mo2_5_min$MO2, from = 'umol_O2 / hr', to = 'mg_O2 / min'))
#'
#' # just endpoint measurement:
#' calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
#' bin_width = Inf, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL)
#' 
#' @encoding UTF-8
#' @export

calc_MO2 = function(duration, o2, o2_unit = 'percent_a.s.', bin_width, vol, temp = 25, sal = 35, atm_pres = 1013.25, good_data = TRUE){
	if(missing(bin_width)) stop('"bin_width" must be provided. If MO2 should be calculated from one observation to the next, set "bin_width" to 0.')
	data = data.frame(duration, o2, temp, sal, atm_pres, good = good_data)
	data = data[good_data, ]
	data$umol_o2 = conv_o2(o2 = data$o2, from = o2_unit, to = 'umol_per_l', temp = data$temp, sal = data$sal, atm_pres = data$atm_pres)
	if(bin_width != 0){
		data$bin = floor(data$duration / bin_width)
		mo2s = data.frame(
			MEAN_O2 = as.numeric(tapply(data$o2, data$bin, mean, na.rm = TRUE)),
			O2_RANGE = sapply(tapply(data$o2, data$bin, range, na.rm = TRUE), function(i) paste(rev(i), collapse = ' - ')),
			MEAN_DURATION = as.numeric(tapply(data$duration, data$bin, mean, na.rm = TRUE)),
			DUR_RANGE = sapply(tapply(data$duration, data$bin, range, na.rm = TRUE), function(i) paste(signif(i, 2), collapse = ' - ')),
			MO2 = as.vector(-by(data, data$bin, function(i) stats::coef(stats::lm(umol_o2 ~ duration, data = i))['duration'])) * 60 * vol,
			R2 = as.vector(by(data, data$bin, function(i) summary(stats::lm(umol_o2 ~ duration, data = i))$r.squared)),
			N = as.numeric(table(data$bin)))
		return(mo2s)
	}
	
	if(bin_width == 0){
		mo2s = data.frame(
			MEAN_O2 = utils::head(data$o2, -1) + diff(data$o2) / 2,
			O2_RANGE = paste(data$o2[1:(nrow(data) - 1)], data$o2[2:nrow(data)], sep = ' - '),
			MEAN_DURATION = utils::head(data$duration, -1) + diff(data$duration) / 2,
			DUR_RANGE = paste(signif(data$duration[1:(nrow(data) - 1)], 3), signif(data$duration[2:nrow(data)], 3), sep = ' - '),
			MO2 = -diff(data$umol_o2) / diff(data$duration) * 60 * vol,
			R2 = NA,
			N = 2)
		return(mo2s)
	}
}