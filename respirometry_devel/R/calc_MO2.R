#' @title Calculate metabolic rate
#' 
#' @description Calculates metabolic rate (MO2) given O2 measurements over time. Oxygen measurements are split into bins and MO2s are calculated from each bin (unless \code{bin_width} is set to \code{0}). The \code{bin_width} parameter defines the width of the bins in timed intervals (e.g. 15 minutes). Linear regressions are fit through each bin and the calculated MO2 is returned as the slope of the change in O2 over time.
#' 
#' @param duration numeric vector of the timepoint for each observation (minutes).
#' @param o2 numeric vector of O2 observations.
#' @param o2_unit a string describing the unit used to measure \code{o2}. Default is "percent_a.s." Options are from \code{\link{conv_o2}}.
#' @param bin_width numeric or data frame. \describe{
#'	\item{OPTION 1: }{A single number defining how long of a period should be binned for each MO2 determination (minutes). If MO2 is to be calculated from one observation to the next (rather than binned observations), set \code{bin_width} to 0. To calculate a single MO2 value from all observations, set \code{bin_width} to \code{Inf}.}
#'	\item{OPTION 2: }{A data frame with two numeric columns: "o2" and "width". Useful for Pcrit calculations or another application where bins of different widths are desired at different PO2s. The data frame can be generated automatically by \code{\link{make_bins}} or manually by the user. For each row, set the "width" value to the bin duration (minutes) desired for observations >= the value in the "o2" column but < the next greater O2 value in another row.}
#'	}
#' @param vol volume of the respirometer (liter).
#' @param temp temperature (°C). Default is 25 °C.
#' @param sal salinity (psu). Default is 35 psu.
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' @param time (optional). Numeric vector of timestamp observations.
#' @param pH (optional). Numeric vector of pH observations.
#' @param good_data logical vector of whether O2 observations are "good" measurements and should be included in analysis. For intermittent respirometry, flush periods should be labelled as \code{FALSE}. Linear regressions will not be fit over bins that include "bad" data. Bins will be split at bad data points. Default is that all observations are \code{TRUE}.
#'
#' @return A data frame is returned:
#' \describe{
#' \item{DUR_MEAN}{Mean duration of the bin (minutes).}
#' \item{DUR_RANGE}{Range of duration timepoints in the bin.}
#' \item{TIME_MEAN}{Exists only if the parameter \code{time} has values. Mean timestamp of the bin.}
#' \item{TIME_RANGE}{Exists only if the parameter \code{time} has values. Range of timestamps in the bin.}
#' \item{TEMP_MEAN}{Mean temperature of the bin.}
#' \item{PH_MEAN}{Exists only if the parameter \code{pH} has values. Mean pH of the bin. Averaged using \code{mean_pH()}.}
#' \item{O2_MEAN}{Mean O2 value of the bin in the unit chosen by \code{o2_unit}).}
#' \item{O2_RANGE}{Range of O2 values in the bin.}
#' \item{MO2}{Metabolic rate (umol O2 / hour).}
#' \item{R2}{Coefficient of determination for the linear regression fit to calculate MO2.}
#' \item{N}{Number of observations in the bin.}
#' }
#' 
#' @note Whole-animal MO2 is returned. If mass-specific MO2 is desired, the output from \code{calc_MO2} can be divided by the animal's mass.
#' No matter what unit of oxygen partial pressure or concentration measurement you put into the function as \code{o2_unit}, the output in the MO2 column is always expressed in umol O2 / hour. This is because there is a vast variety of units for which people prefer to report dissolved oxygen levels, but most physiologists are at least unified in reporting metabolic rate as umol O2 per hour. If you prefer to report MO2 as mg O2 per hour, for example, you can always do something like:
#' \code{conv_resp_unit(df$MO2, from = 'umol_O2 / hr', 'mg_O2 / hr')}
#' If only beginning and ending O2 observations are known, consider using \code{\link{closed}}. Both functions will work fine, but \code{\link{closed}} is simpler.
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{make_bins}}, \code{\link{calc_b}}, \code{\link{closed}}, \code{\link{scale_MO2}}, \code{\link{conv_resp_unit}}
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
#' plot(mo2_5_min$O2_MEAN, mo2_5_min$MO2)
#' 
#' # I want to express MO2 in mg per min instead.
#' (mo2_5_min$MO2 <- conv_resp_unit(value = mo2_5_min$MO2, from = 'umol_O2 / hr', to = 'mg_O2 / min'))
#'
#' # just endpoint measurement:
#' calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
#' bin_width = Inf, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL)
#' 
#' # In my trial, observations above 77% air saturation were really noisy, but much less noisy at
#' # lower O2 values. I want to adjust my bin width based on the PO2 to obtain the best balance of
#' # resolution and precision throughout the whole trial. Below 77% a.s., use 4 minute bins. Above
#' # 77% a.s. use 10 minute bins.
#' bins = data.frame(o2 = c(77, 100), width = c(4, 10))
#' calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
#' bin_width = bins, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL)
#' 
#' @encoding UTF-8
#' @export

calc_MO2 = function(duration, o2, o2_unit = 'percent_a.s.', bin_width, vol, temp = 25, sal = 35, atm_pres = 1013.25, time, pH, good_data = TRUE){
	if(missing(bin_width)) stop('"bin_width" must be provided. If MO2 should be calculated from one observation to the next, set "bin_width" to 0.')
	
	data = data.frame(duration, o2, temp, sal, atm_pres, good = good_data)
	
		data = data[!is.na(data$o2), ]
	
		if(methods::hasArg(time)) data$time = time
		if(methods::hasArg(pH)) data$pH = pH
	data$umol_o2 = conv_o2(o2 = data$o2, from = o2_unit, to = 'umol_per_l', temp = data$temp, sal = data$sal, atm_pres = data$atm_pres)
	
	if(any(bin_width != 0)){
		if(is.numeric(bin_width)){
			data$bin = floor((data$duration - min(data$duration, na.rm = TRUE)) / bin_width)
			data$bin = unlist(by(data, data$bin, function(i){ # split bins by bad data
				breaks = rle(i$good)[['lengths']]
				paste0(i$bin, rep(letters[1:length(breaks)], breaks))
			}))
			inter1 = by(data, data$bin, function(i) sum(i$good) != 0) # identify bad bins
			data = data[data$bin %in% names(inter1)[inter1], ] # remove bad bins
		}
		
		if(is.data.frame(bin_width)){
			if(any(colnames(bin_width) != c('o2', 'width'))) stop('"bin_width" must be a data frame with two numeric columns: "o2" and "width". See "make_bins()" to use a dataframe of bins.')
			bin_width = bin_width[order(bin_width$o2, decreasing = TRUE), ]
			data$bin = NA
			index1 = 1
			for(i in 1:nrow(bin_width)){
				for(j in index1:nrow(data)){
					if(is.na(o2[j])) next
					if(o2[j] >= bin_width[i, 'o2']) data[j, 'bin'] = i else {
						index1 = j
						break
					}
				}
			}
			
			data$bin = paste0(data$bin, '_', floor(data$duration / bin_width[data$bin, 'width']))
			
			data_grouped = by(data, INDICES = data$bin, FUN = function(i){
			  inter1 = rle(i$good)
			  inter2 = c(1, cumsum(inter1[['lengths']]))
			  inter3 = unique(i$bin)
			  for(j in 1:length(inter1[['lengths']])){ # make sub-bins based on bad data
			    i[birk::range_seq(inter2[c(j, j + 1)]), 'bin'] = paste0(inter3, letters[j])
			  }
			  return(i)
			})
			data = do.call(rbind, data_grouped)
			data = data[order(data$duration), ]
			if(any(!data$good)) data = data[!(data$bin %in% c(sort(unique(data$bin))[table(data$bin, data$good)[, 'FALSE'] > 1])), ] # get rid of bins with bad data

		}
		
	  data = data[data$good, ]
		mo2s = data.frame(
			DUR_MEAN = as.numeric(tapply(data$duration, data$bin, mean, na.rm = TRUE)),
			DUR_RANGE = sapply(tapply(data$duration, data$bin, range, na.rm = TRUE), function(i) paste(i, collapse = ' - '))
		)
		if(methods::hasArg(time)){
			mo2s$TIME_MEAN = as.POSIXct(tapply(data$time, data$bin, mean, na.rm = TRUE), origin = '1970-01-01')
			mo2s$TIME_RANGE = sapply(tapply(data$time, data$bin, range, na.rm = TRUE), function(i){
				i = as.character(i)
				if(lubridate::date(i[1]) == lubridate::date(i[2])) i[2] = strftime(i[2], format='%T') 
				paste(i, collapse = ' - ')
			})
		}
		if(methods::hasArg(pH)) mo2s$PH_MEAN = as.numeric(tapply(data$pH, data$bin, mean_pH, na.rm = TRUE))
		
		mo2s$TEMP_MEAN = as.numeric(tapply(data$temp, data$bin, mean, na.rm = TRUE))
		
		mo2s$O2_MEAN = as.numeric(tapply(data$o2, data$bin, mean, na.rm = TRUE))
		mo2s$O2_RANGE = sapply(tapply(data$o2, data$bin, range, na.rm = TRUE), function(i) paste(rev(i), collapse = ' - '))
		mo2s$MO2 = as.vector(-by(data, data$bin, function(i) if(any(!is.na(i$umol_o2))) stats::coef(stats::lm(umol_o2 ~ duration, data = i))['duration'] else NA)) * 60 * vol
		mo2s$R2 = as.vector(by(data, data$bin, function(i) if(any(!is.na(i$umol_o2))) summary(stats::lm(umol_o2 ~ duration, data = i))$r.squared else NA))
		mo2s$N = as.numeric(table(stats::na.omit(data[, c('duration', 'umol_o2', 'bin')])$bin))
		
		mo2s = mo2s[order(mo2s$DUR_MEAN), ]
		row.names(mo2s) = NULL
	}
	
	if(all(bin_width == 0)){
	  data = data[good_data, ]
	  
		mo2s = data.frame(
			DUR_MEAN = utils::head(data$duration, -1) + diff(data$duration) / 2,
			DUR_RANGE = paste(data$duration[1:(nrow(data) - 1)], data$duration[2:nrow(data)], sep = ' - '))
		if(methods::hasArg(time)){
			mo2s$TIME_MEAN = utils::head(data$time, -1) + diff(data$time) / 2
			mo2s$TIME_RANGE = sapply(1:(nrow(data) - 1), function(i){
				i = as.character(data[i:(i + 1), 'time'])
				if(lubridate::date(i[1]) == lubridate::date(i[2])) i[2] = strftime(i[2], format='%T') 
				paste(i, collapse = ' - ')
			})
		}
		if(methods::hasArg(pH)) mo2s$PH_MEAN = utils::head(data$pH, -1) + diff(data$pH) / 2
			mo2s$TEMP_MEAN = utils::head(data$temp, -1) + diff(data$temp) / 2
			mo2s$O2_MEAN = utils::head(data$o2, -1) + diff(data$o2) / 2
			mo2s$O2_RANGE = paste(data$o2[1:(nrow(data) - 1)], data$o2[2:nrow(data)], sep = ' - ')
			mo2s$MO2 = -diff(data$umol_o2) / diff(data$duration) * 60 * vol
	    mo2s$R2 = NA
			mo2s$N = 2
	}
	attr(mo2s, 'mo2_data') = TRUE
	return(mo2s)
}

#' @rdname calc_MO2
#' @export
calc_mo2 = calc_MO2