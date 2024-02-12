#' Import data from Pyro Oxygen Logger
#'
#' Imports the standard txt file output from Pyroscience's deprecated Pyro Oxygen Logger software and converts the data into one or more data frames. If using the newer Pyroscience Workbench software, use \code{\link{import_pyroscience_workbench}} instead.
#'
#' The following FireSting fiber optic O2 transmitters are supported:
#' \itemize{
#' \item{FireStingO2}{}
#' \item{FireStingO2 (1st generation)}{}
#' }
#' If you would like support for the Piccolo2, FireStingO2-Mini, TeX4, or any OEM instruments, email me a data file from the device.
#'
#' @param file a character string. The filepath for the file to be read.
#' @param o2_unit a character string. The unit of O2 measurement to be output in the data frame. Options are described in \code{\link{conv_o2}}.
#' @param date a character string. The date format to be passed to \code{\link{strptime}}.
#' @param overwrite_sal Default \code{NULL}. To overwrite the salinity value(s) from calibration, enter a single numeric value for all channels or a numeric vector with values for each channel. Salinity of water sample (psu).
#' @param keep_metadata logical. Should metadata from the file be returned as extra columns in the returned data frame? Default is \code{FALSE}.
#' @param drop_channels logical. Should channels without any O2 data be dropped? Default is \code{TRUE}.
#' @param split_channels logical. Should a list of data frames be returned with a separate data frame for each channel? Default is \code{FALSE}.
#'
#' @return A data frame (or list of data frames) is returned.
#' \describe{
#' \item{TIME}{Date and time, POSIXlt format.}
#' \item{DURATION}{Duration of measurement trial (minutes).}
#' \item{CH_X_O2}{Oxygen measurement in desired unit as determined by \code{o2_unit}.}
#' \item{CH_X_TEMP}{Temperature recorded or defined at beginning of measurement trial.}
#' \item{CH_X_SAL}{Salinity (psu).}
#' \item{...}{Channel columns (CH_...) are repeated for each channel.}
#' \item{COMMENT}{Comments from FireSting file.}
#' }
#' If \code{keep_metadata = TRUE}, then the following columns are appended to the returned data frame:
#' \describe{
#' \item{ATM_PRES}{Atmospheric pressure (mbar).}
#' \item{HUMIDITY}{Relative humidity (\% RH).}
#' \item{PROBE_TEMP}{Probe temperature.}
#' \item{INTERNAL_TEMP}{Transmitter internal temperature.}
#' \item{ANALOG_IN}{Voltage input from the extension port (mV).}
#' \item{CH_X_PHASE}{Phase recorded. Phase is inversely related to O2.}
#' \item{CH_X_INTENSITY}{Intensity is an indicator of the quality of the signal. A low intensity warning is produced by the transmitter below 10 mV.}
#' \item{CH_X_AMB_LIGHT}{Ambient light on the sensor. Expressed in mV.}
#' }
#' 
#' If \code{split_channels = TRUE}, then "\code{CH_X_}" is removed from the column names and multiple data frames are returned in a named list.
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Oxygen conversions are estimates based on the \code{\link[marelac]{marelac}} package.
#' @seealso \code{\link{import_pyroscience_workbench}}, \code{\link{import_presens}}, \code{\link{import_witrox}}, \code{\link{conv_o2}}
#' @examples
#' \dontrun{
#' file <- system.file('extdata', 'pyro_oxygen_logger_file.txt', package = 'respirometry')
#' import_firesting(file, o2_unit = 'umol_per_l')
#' 
#' # I want each channel as a separate data frame.
#' data_list <- import_firesting(file, split_channels = TRUE)
#' data_list$CH_3 # here's the channel 3 data frame.
#' }
#'
#' @encoding UTF-8
#' @export

import_firesting = function(file, o2_unit = 'percent_a.s.', date = '%m/%d/%Y %X', overwrite_sal = NULL, keep_metadata = FALSE, drop_channels = TRUE, split_channels = FALSE){
	raw = readChar(file, nchars = file.info(file)$size, useBytes = TRUE)
	#raw = gsub(pattern = '\xb0|\xa9|\xfc\xbe\x8e\x93\xa0\xbc', replacement = ' ', raw) # replace non ASCII characters
	Encoding(raw) = 'UTF-8'
	raw = iconv(raw, 'UTF-8', 'UTF-8', sub = ' ')
	raw = strsplit(raw, split = '\r+\n')[[1]]
	raw = raw[sapply(raw, nchar) > 0] # remove blank rows
	f = strsplit(raw, split = '\t', fixed = TRUE)
	channels_calib = suppressWarnings(stats::na.omit(as.numeric(gsub('Ch ', '', sapply(f[grep('Settings:', f[1:50]) + 1:4], '[', 1)))))
	n_channels_calib = length(channels_calib)
	sals = as.numeric(sapply(f[grep('Settings:', f[1:50]) + 1:n_channels_calib], '[', grep('Salinity', f[[grep('Settings:', f[1:50])]])))
	if(is.null(overwrite_sal)) overwrite_sal = sals
	if(length(overwrite_sal) == 1) overwrite_sal = rep(overwrite_sal, length(sals))
	orig_o2_units = sapply(f[grep('Settings:', f[1:50]) + 1:n_channels_calib], '[', 3)
	software_version = paste(f[[2]], collapse = ' ')
	if(grepl('Firesting Logger', software_version)){
		atm_pres = as.numeric(sapply(f[grep('Settings:', f[1:50]) + 1:n_channels_calib], '[', grep('Press.', f[[grep('Settings:', f[1:50])]])))
		date_start = f[[grep('Date:', f[1:50])]][2]
	}
	f = f[(utils::tail(grep('Time', f[1:50], fixed = TRUE), 1) - 1):length(f)]
	if(grepl('Firesting Logger', software_version)){
		f[[1]][21] = ''
		if(length(unique(sapply(f, length))) != 1){
			inter1 = which(sapply(f, length) != 21)
			f = f[-c(inter1, utils::tail(inter1, 1) + 1:2)]
		}
	}
	if(grepl('Pyro Oxygen Logger', software_version)){
		if(length(unique(sapply(f, length))) != 1) f = lapply(f, function(i) i[-length(f[[1]])])
	}
	f = as.data.frame(matrix(unlist(f), ncol = length(f[[1]]), byrow = TRUE), stringsAsFactors = FALSE)
	n_channels = max(as.numeric(gsub('^Ch\\s?(\\d)', '\\1', f[2, grep('^Ch\\s?\\d', f[2, ])])))
	channel_names = paste('CH', 1:n_channels, sep = '_')
	if(grepl('Firesting Logger', software_version)){
		colnames(f) = c('TIME', 'COMMENT', 'TEMP', paste('CH', 1:n_channels, 'O2', sep = '_'), '', paste('CH', 1:n_channels, 'PHASE', sep = '_'), '', paste('CH', 1:n_channels, 'INTENSITY', sep = '_'), paste('CH', 1:n_channels, 'AMB_LIGHT', sep = '_'))
		
	}
	if(grepl('Pyro Oxygen Logger', software_version)){
		colnames(f) = c('DATE', 'TIME', 'DURATION', 'COMMENT', paste('CH', 1:n_channels, 'O2', sep = '_'), paste('CH', 1:n_channels, 'TEMP', sep = '_'), 'ATM_PRES', 'HUMIDITY', 'PROBE_TEMP', 'INTERNAL_TEMP', 'ANALOG_IN', paste('CH', 1:n_channels, 'PHASE', sep = '_'), paste('CH', 1:n_channels, 'INTENSITY', sep = '_'), paste('CH', 1:n_channels, 'AMB_LIGHT', sep = '_'))
		
	}
	f = f[-(1:2), ]
	if(!(o2_unit %in% c(names(conv_o2()), 'raw', 'dphi'))) stop('the o2_unit argument is not an acceptable unit', call. = FALSE)
	o2_cols = grep('^CH_\\d_O2$', colnames(f))
	o2_cols = o2_cols[channels_calib]
	o2_string_options = list(
		'% air sat' = 'percent_a.s.',
		'% O2' = 'percent_o2',
		'ml/l' = 'ml_per_l',
		'umol/l' = 'umol_per_l',
		'mg/l (ppm)' = 'mg_per_l',
		'hPa (mbar)' = 'hPa',
		'mmHg (Torr)' = 'mmHg',
		'ug/l (ppb)' = 'ug_per_l'
	)
	f[f == '---'] = NA
	f[, -which(colnames(f) %in% c('DATE', 'TIME', 'COMMENT'))] = sapply((1:ncol(f))[-which(colnames(f) %in% c('DATE', 'TIME', 'COMMENT'))], function(i) as.numeric(f[, i])) # all except date, time, and comment
	f[, o2_cols] = sapply(o2_cols, function(i){
		if(orig_o2_units[which(i == o2_cols)] %in% c('raw', 'dphi')){
			warning('The O2 values in channel ', gsub('CH_(\\d)_O2', '\\1', colnames(f)[i]), ' are expressed in either "raw" or "dphi" and cannot be converted to ', o2_unit)
			return(f[, i])
		}
		if(grepl('Firesting Logger', software_version)){
			inter1 = conv_o2(o2 = f[, i], from = o2_string_options[[orig_o2_units[which(i == o2_cols)]]], to = o2_unit, temp = f$TEMP, sal = sals[which(i == o2_cols)], atm_pres = atm_pres[which(i == o2_cols)])
			inter1 = inter1 / conv_o2(to = o2_unit, temp = f$TEMP, sal = sals[which(i == o2_cols)], atm_pres = atm_pres[which(i == o2_cols)]) * conv_o2(to = o2_unit, temp = f$TEMP, sal = overwrite_sal[which(i == o2_cols)], atm_pres = atm_pres[which(i == o2_cols)])
			return(inter1)
		}
		if(grepl('Pyro Oxygen Logger', software_version)){
			inter1 = conv_o2(o2 = f[, i], from = o2_string_options[[orig_o2_units[which(i == o2_cols)]]], to = o2_unit, temp = f[, i + n_channels], sal = sals[which(i == o2_cols)], atm_pres = f$ATM_PRES)
			inter1 = inter1 / conv_o2(to = o2_unit, temp = f[, i + n_channels], sal = sals[which(i == o2_cols)], atm_pres = f$ATM_PRES) * conv_o2(to = o2_unit, temp = f[, i + n_channels], sal = overwrite_sal[which(i == o2_cols)], atm_pres = f$ATM_PRES)
			return(inter1)
		}
	})
	if(grepl('Firesting Logger', software_version)) f$DATE = date_start
	f$TIME = strptime(paste(f$DATE, f$TIME), format = date)
	f$DATE = NULL
	if(grepl('Firesting Logger', software_version)){
		inter1 = c(0, which(diff(f$TIME) < 0), nrow(f))
		inter2 = lapply(1:(length(inter1) - 1), function(i){
			f[(inter1[i] + 1):inter1[i + 1], 'TIME'] + lubridate::days(i - 1)
		})
		f$TIME = do.call(c, inter2)
		f$DURATION = as.numeric(difftime(time1 = f$TIME, time2 = f[1, 'TIME'], units = 'sec'))
	}
	f$DURATION = f$DURATION / 60 # convert from secs to mins
	if(grepl('Firesting Logger', software_version)) f = f[, colnames(f) != '']
	
	#o2_cols = grep('^CH_\\d_O2$', colnames(f))
	for(i in rev(1:length(overwrite_sal))){
		sal_list = list(overwrite_sal[i])
		names(sal_list) = paste('CH', i, 'SAL', sep = '_')
		if(grepl('Firesting Logger', software_version)) f = data.frame(append(f, sal_list, after = grep(paste('CH', n_channels, 'O2', sep = '_'), colnames(f))))
		if(grepl('Pyro Oxygen Logger', software_version)) f = data.frame(append(f, sal_list, after = grep(paste('CH', n_channels, 'TEMP', sep = '_'), colnames(f))))
	}
	
	f$COMMENT = as.character(f$COMMENT)
	comment_col = grep('COMMENT', colnames(f))
	last_sal_col = max(grep('CH_\\d_SAL', colnames(f)))
	f = f[, c(setdiff(1:last_sal_col, comment_col), comment_col, (last_sal_col + 1):ncol(f))]
	
	
	if(any(is.na(f$TIME))) stop(paste('The time record does not match', date, 'on at least some of the lines between', range(which(is.na(f$TIME)))[1], 'and', range(which(is.na(f$TIME)))[2]), call. = FALSE)
	
	inter1 = f[, paste0(channel_names, '_INTENSITY')] < 10
	inter1[is.na(f[, paste0(channel_names, '_O2')])] = FALSE # ignore values where O2 is NA
	if(any(inter1)){
		inter2 = which(inter1, arr.ind = TRUE)
		warning(paste('The signal intensity was too low (i.e. < 10 mV) for channel(s)', paste(gsub('_INTENSITY', '', colnames(inter1)[unique(inter2[, 'col'])]), collapse = ', '), 'between', paste(f[range(inter2[, 'row']), 'TIME'], collapse = ' and ')), call. = FALSE)
	}
	
	if(drop_channels){
		blank_cols = apply(f[, o2_cols], 2, function(i) all(is.na(i)))
		if(any(blank_cols)){
			drop = gsub('_O2$', '', names(which(blank_cols)))
			f = f[, !grepl(paste(drop, collapse = '|'), colnames(f))]
		}
	}
	
	if(!keep_metadata) f = f[, -grep(paste('ATM_PRES', 'HUMIDITY', 'PROBE_TEMP', 'INTERNAL_TEMP', 'ANALOG_IN', 'CH_\\d_PHASE', 'CH_\\d_INTENSITY', 'CH_\\d_AMB_LIGHT', sep = '|'), colnames(f))]
	
	if(split_channels){
#		channel_names = unique(gsub('(^CH_\\d).*', '\\1', grep('^CH_\\d', colnames(f), value = TRUE)))
		f = lapply(1:length(channel_names), function(i){
			f_sub = f[, grep(paste(channel_names[!channel_names %in% paste0('CH_', i)], collapse = '|'), colnames(f), invert = TRUE)]
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