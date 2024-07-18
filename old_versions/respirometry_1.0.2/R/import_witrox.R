#' Import data from a Loligo Systems Witrox O2 transmitter
#'
#' Imports the standard txt file output from Loligo Systems Witrox fiber optic O2 transmitters and converts the data into one or more data frames.
#'
#' The following Loligo Systems fiber optic O2 transmitters are supported:
#' \itemize{
#' \item{Witrox 4}{}
#' }
#' If you would like support for the Witrox 1, email me a data file from this device.
#'
#' @param file a character string. The filepath for the file to be read.
#' @param o2_unit a character string. The unit of O2 measurement to be output in the data frame. Options are described in \code{\link{conv_o2}}.
#' @param date a character string. The date format to be passed to \code{\link{strptime}}.
#' @param overwrite_sal Default \code{NULL}. To overwrite the salinity value(s) from calibration, enter a single numeric value for all channels or a numeric vector with values for each channel. Salinity of water sample (psu).
#' @param drop_channels logical. Should channels without any O2 data be dropped? Default is \code{TRUE}.
#' @param split_channels logical. Should a list of data frames be returned with a separate data frame for each channel? Default is \code{FALSE}.
#'
#' @return A data frame (or list of data frames) is returned.
#' \describe{
#' \item{TIME}{Date and time, POSIXlt format.}
#' \item{DURATION}{Duration of measurement trial (minutes).}
#' \item{ATM_PRES}{Atmospheric pressure (mbar).}
#' \item{CH_X_PHASE}{Phase recorded. Phase is inversely related to O2.}
#' \item{CH_X_TEMP}{Temperature recorded or defined at beginning of measurement trial.}
#' \item{CH_X_SAL}{Salinity (psu).}
#' \item{CH_X_O2}{Oxygen measurement in desired unit as determined by \code{o2_unit}.}
#' \item{...}{Channel columns (CH_...) are repeated for each channel.}
#' }
#' If \code{split_channels = TRUE}, then "\code{CH_X_}" is removed from the column names and multiple data frames are returned in a list.
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{import_firesting}}, \code{\link{import_presens}}, \code{\link{conv_o2}}
#' @examples
#' \dontrun{
#' file <- system.file('extdata', 'witrox_file.txt', package = 'respirometry')
#' import_witrox(file, o2_unit = 'umol_per_l')
#' 
#' # Oops. I forgot to change the salinity value when I calibrated
#' # the instrument. Override the values in the file for 35 psu.
#' import_witrox(file, o2_unit = 'umol_per_kg', overwrite_sal = 35)
#' 
#' # I want each channel as a separate data frame.
#' data_list <- import_witrox(file, split_channels = TRUE)
#' data_list$CH_3 # here's the channel 3 data frame.
#' }
#'
#' @encoding UTF-8
#' @export

import_witrox = function(file, o2_unit = 'percent_a.s.', date = '%m/%d/%Y %I:%M:%S %p', overwrite_sal = NULL, drop_channels = TRUE, split_channels = FALSE){
	raw = readChar(file, nchars = file.info(file)$size, useBytes = TRUE)
	raw = gsub(pattern = '\xb0|\xa9|\xfc\xbe\x8e\x93\xa0\xbc', replacement = ' ', raw) # replace non ASCII characters
	raw = strsplit(raw, split = '\r\n', fixed = T)[[1]]
	raw = raw[sapply(raw, nchar) > 0] # remove blank rows
	f = strsplit(raw, split = '\t', fixed = TRUE)
	sals = as.numeric(sapply(f[grep('CALIBRATION DATA', f[1:20]) + 2:5], '[', 9))
	if(is.null(overwrite_sal)) overwrite_sal = sals
	if(length(overwrite_sal) == 1) overwrite_sal = rep(overwrite_sal, length(sals))
	f = f[grep('Date &Time [', f[1:20], fixed = TRUE):length(f)]
	f = as.data.frame(matrix(unlist(f), ncol = length(f[[1]]), byrow = TRUE), stringsAsFactors = FALSE)
	colnames(f) = as.character(f[1, ])
	f = f[-1, ]
	colnames(f) = gsub('.*,', '', colnames(f)) # remove serial number from cols
	if(!(o2_unit %in% names(conv_o2()))) stop('the o2_unit argument is not an acceptable unit', call. = FALSE)
	o2_cols = grep('CH \\d O2', colnames(f))
	orig_o2_units = gsub(' CH \\d O2 \\[(.*)\\]', '\\1', colnames(f[grep('CH \\d O2', colnames(f))]))
	colnames(f) = c('TIME', 'DURATION', 'ATM_PRES', 'CH_1_PHASE', 'CH_1_TEMP', 'CH_1_O2', 'CH_2_PHASE', 'CH_2_TEMP', 'CH_2_O2', 'CH_3_PHASE', 'CH_3_TEMP', 'CH_3_O2', 'CH_4_PHASE', 'CH_4_TEMP', 'CH_4_O2')
	o2_string_options = list(
		'% air saturation' = 'percent_a.s.',
		'% oxygen saturation' = 'percent_o2',
		'kPa' = 'kPa',
		'Torr' = 'torr',
		'mg/L' = 'mg_per_l',
		'mmol' = 'mmol_per_l',
		'ml/L' = 'ml_per_l'
	)
	f[, -1] = sapply(2:ncol(f), function(i) as.numeric(f[, i]))
	f[, o2_cols] = sapply(o2_cols, function(i){
		inter1 = conv_o2(o2 = f[, i], from = o2_string_options[[orig_o2_units[which(i == o2_cols)]]], to = o2_unit, temp = f[, i - 1], sal = sals[which(i == o2_cols)], atm_pres = f$ATM_PRES)
		inter1 = inter1 / conv_o2(to = o2_unit, temp = f[, i - 1], sal = sals[which(i == o2_cols)], atm_pres = f$ATM_PRES) * conv_o2(to = o2_unit, temp = f[, i - 1], sal = overwrite_sal[which(i == o2_cols)], atm_pres = f$ATM_PRES)
		inter1
	})
	f$TIME = strptime(f$TIME, format = date)
	f$DURATION = as.numeric(difftime(time1 = f$TIME, time2 = f[1, 'TIME'], units = 'mins'))
	for(i in 1:length(overwrite_sal)){
		sal_list = list(overwrite_sal[i])
		names(sal_list) = paste('CH', i, 'SAL', sep = '_')
		f = data.frame(append(f, sal_list, after = grep('_TEMP$', colnames(f))[i]))
	}
	o2_cols = grep('CH_\\d_O2', colnames(f)) # update now that SAL exists
	if(any(is.na(f$TIME))) stop(paste('The time record does not match', date, 'on at least some of the lines between', range(which(is.na(f$TIME)))[1], 'and', range(which(is.na(f$TIME)))[2]), call. = FALSE)
	row.names(f) = NULL
	if(drop_channels){
		blank_cols = apply(f[, o2_cols], 2, function(i) all(is.na(i)))
		if(any(blank_cols)){
			drop = gsub('_O2$', '', names(which(blank_cols)))
			f = f[, !grepl(paste(drop, collapse = '|'), colnames(f))]
		}
	}
	if(split_channels){
		channel_names = unique(gsub('(^CH_\\d).*', '\\1', grep('^CH_\\d', colnames(f), value = TRUE)))
		f = lapply(0:(sum(grepl('^CH_', colnames(f))) / 4 - 1), function(i){
			f_sub = f[, c(grep('^CH_', colnames(f), value = TRUE, invert = TRUE), grep('^CH_', colnames(f), value = TRUE)[1:4 + 4 * i])]
			colnames(f_sub) = gsub('^CH_\\d_(.*)', '\\1', colnames(f_sub))
			if(any(is.na(f_sub$O2))){
				f_sub = f_sub[birk::range_seq(which(is.na(f_sub$O2) == FALSE)), ]
				f_sub$DURATION = as.numeric(difftime(time1 = f_sub$TIME, time2 = f_sub[1, 'TIME'], units = 'mins'))
				row.names(f_sub) = NULL
			}
			return(f_sub)
		})
		names(f) = channel_names
	}
	return(f)
}
