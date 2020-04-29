#' Import data from a PreSens O2 transmitter
#'
#' Imports the standard text file output from most single channel PreSens fiber optic O2 transmitters and converts the data into a data frame.
#'
#' The following PreSens fiber optic O2 transmitters are supported:
#' \itemize{
#' \item{Fibox 4}{}
#' \item{Fibox 3}{}
#' \item{Fibox 3 trace}{}
#' \item{Fibox 3 LCD trace}{}
#' \item{Microx TX3}{}
#' \item{Microx TX3 trace}{}
#' \item{SDR SensorDish Reader}{}
# \item{OXY-4 mini}{}
# \item{OXY-4 micro}{}
# \item{OXY-4 trace}{}
# \item{OXY-10 mini}{}
# \item{OXY-10 micro}{}
# \item{OXY-10 trace}{}
#' }
#' If you would like support for another PreSens O2 meter, email the package maintainer a data file from the device you would like supported.
#' It is very important to note that the PreSens fiber optics O2 transmitters that are supported with this function (except the Fibox 4) DO NOT account for salinity (i.e. they assume salinity = 0 ppt). If the water sample measured was not fresh water, the oxygen concentrations (e.g. mg per liter or umol per liter) are incorrect in the PreSens txt file. This function corrects these O2 concentrations based on the salinity value defined by the \code{sal} argument. Absolute partial pressures (i.e. hPa and torr) will also be slightly different due to the slight influence of salinity on water's vapor pressure. This difference is typically ~0.05\% of the recorded value.
#'
#' @param file a character string. The filepath for the file to be read.
#' @param o2_unit a character string. The unit of O2 measurement to be output in the data frame. Options are described in \code{\link{conv_o2}}.
#' @param date a character string. The date format to be passed to \code{\link{strptime}}.
#' @param sal salinity of water sample (psu). Default is 35 psu. Ignored for Fibox 4 files since salinity is provided by the file.
#' @param all_cols logical. For Fibox 4 files only. Should all columns (including calibration data and serial numbers) be output?
#' @param split_channels logical. For SDR SensorDish only. Should a list of data frames be returned with a separate data frame for each channel? Default is \code{FALSE}.

#'
#' @return A data frame is returned.
#' \describe{
#' \item{TIME}{Date and time, POSIXct format.}
#' \item{DURATION}{Duration of measurement trial (minutes).}
#' \item{O2}{Oxygen measurement in desired unit as determined by \code{o2_unit}.}
#' \item{PHASE}{Phase recorded. Phase is inversely related to O2. Not included in SDR SensorDish Reader files.}
#' \item{AMPLITUDE}{Amplitude recorded. Amplitude is an indicator of the quality of the signal. A low amplitude warning is produced by the transmitter below 2500. Not included in SDR SensorDish Reader files.}
#' \item{TEMP}{Temperature recorded or defined at beginning of measurement trial.}
#' \item{ATM_PRES}{Atmospheric pressure (mbar).}
#' \item{SAL}{Salinity (psu).}
#' \item{ERROR_CODE}{Error code from transmitter. See PreSens user manual for translation of error code. Not included in SDR SensorDish Reader files.}
#' }
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Oxygen conversions are based on \code{\link{conv_o2}} and therefore differ slightly from the conversions provided by PreSens.
#' @seealso \code{\link{import_firesting}}, \code{\link{import_witrox}}, \code{\link{conv_o2}}
#' @examples
#' \dontrun{
#' 
#' # Import a Fibox 3 file.
#' file <- system.file('extdata', 'fibox_3_file.txt', package = 'respirometry')
#' import_presens(file, o2_unit = 'umol_per_l', sal = 25)
#' 
#' # Import a Fibox 4 file.
#' file <- system.file('extdata', 'fibox_4_file.csv', package = 'respirometry')
#' import_presens(file = file, date = '%d-%b-%Y')
#' 
#' # Import an SDR SensorDish Reader file.
#' file <- system.file('extdata', 'sdr_file.txt', package = 'respirometry')
#' import_presens(file = file, date = '%d.%m.%y%X')
#'
#' }
#' @encoding UTF-8
#' @export

import_presens = function(file, o2_unit = 'percent_a.s.', date = '%d/%m/%y', sal = 35, all_cols = FALSE, split_channels = FALSE)
{
	raw = readChar(file, nchars = file.info(file)$size, useBytes = TRUE)
	raw = gsub(pattern = '\xb0|\xa9|\xfc\xbe\x8e\x93\xa0\xbc', replacement = ' ', raw) # replace non ASCII characters
	raw = strsplit(raw, split = '\r*\n')[[1]]
	raw = raw[sapply(raw, nchar) > 0] # remove blank rows
  
  ########################## FIBOX 3 TYPE FILE ##################################
  
  if(length(grep('DESCRIPTION', raw[1])) == 1){ # this is a Fibox 3 type file
  	f = suppressWarnings(raw[grep('logtime|Logtime', raw):length(raw)]) # start dataframe from one row below the word 'logtime'
  	f = gsub(pattern = ' ', replacement = '', f)
  	f = strsplit(f, split = ';', fixed = TRUE)
  	f = as.data.frame(matrix(unlist(f), ncol = 8, byrow = TRUE), stringsAsFactors = FALSE)
  	if(!(o2_unit %in% names(conv_o2()))) stop('the o2_unit argument is not an acceptable unit', call. = FALSE)
  	atm_pres = suppressWarnings(as.numeric(stats::na.omit(as.numeric(unlist(strsplit(raw[grep('Pressure', raw)], ' |;'))))))
  	colnames(f) = c('DATE', 'TIME', 'DURATION', 'O2', 'PHASE', 'AMPLITUDE', 'TEMP', 'ERROR_CODE')
  	unit_id_index = grep('oxygen', f$O2)
  	o2_string_options = list(
  		percent_a.s. = 'oxygen/%airsatur.',
  		percent_o2 = 'oxygen/%O2',
  		hPa = 'oxygen/hPa(mbar)',
  		torr = 'oxygen/Torr',
  		mg_per_l = 'oxygen/mg/L(ppm)',
  		umol_per_l = 'oxygen/umol/L'
  	)
  	f_split = split(f, findInterval(1:nrow(f), unit_id_index))
  	f_split = lapply(f_split, function(i){
  		inter1 = names(which(i[1, 'O2'] == o2_string_options))
  		i = i[-1, ]
  		i$O2 = as.numeric(i$O2)
  		i$O2 = i$O2 / conv_o2(to = inter1, temp = as.numeric(i$TEMP), sal = 0, atm_pres = atm_pres) * conv_o2(to = o2_unit, temp = as.numeric(i$TEMP), sal = sal, atm_pres = atm_pres)
  		i
  	})
  	f = do.call('rbind', f_split)
  	f[, 'TIME'] = as.data.frame(strptime(paste(f$DATE, f$TIME), paste(date, '%T')))
  	f$DATE = NULL
  	f$DURATION = as.numeric(f$DURATION)
  	if(any(is.na(f$TIME))) stop(paste('The time record does not match', date, 'on at least some of the lines between', range(which(is.na(f$TIME)))[1], 'and', range(which(is.na(f$TIME)))[2]), call. = FALSE)
  	f$PHASE = as.numeric(f$PHASE)
  	f$AMPLITUDE = as.numeric(f$AMPLITUDE)
  	f$TEMP = as.numeric(f$TEMP)
  	f$ERROR_CODE = as.factor(f$ERROR_CODE)
  	f$ATM_PRES = atm_pres
  	f$SAL = sal
  	if(length(unit_id_index) > 1) message(paste('Multiple units of O2 were recorded. All values were converted to', o2_unit))
  	if(any(f$ERROR_CODE != 'E0')) warning('Errors recorded during trial! Check ERROR_CODE column', call. = FALSE)
  	row.names(f) = NULL
  }
  
  ########################## FIBOX 4 TYPE FILE ##################################
  
  if(length(grep('Date;Time;User;', raw[1:2])) == 1){ # this is a Fibox 4 type file
  	f = gsub(pattern = ' ', replacement = '', raw)
  	f = strsplit(f, split = ';', fixed = TRUE)
  	f[[length(f)]] = NULL
  	if(grepl('ExportedwithPreSensDatamanager', f[[1]][1])) f = f[-1]
  	f = as.data.frame(matrix(unlist(f), ncol = length(f[[1]]), byrow = TRUE), stringsAsFactors = FALSE)
  	colnames(f) = as.character(f[1, ])
  	f = f[-1, ]
  	if(!(o2_unit %in% names(conv_o2()))) stop('the o2_unit argument is not an acceptable unit', call. = FALSE)
  	main_cols = which(colnames(f) %in% c('Date', 'Time', 'deltat', 'delta_t', 'Value', 'Phase', 'Amplitude', 'Temp', 'patm', 'Salinity', 'Error'))
  	colnames(f)[main_cols] = c('DATE', 'TIME', 'DURATION', 'O2', 'PHASE', 'AMPLITUDE', 'TEMP', 'ATM_PRES', 'SAL', 'ERROR_CODE')  	
  	o2_string_options = list(
  		'%a.s.' = 'percent_a.s.',
  		'%O2' = 'percent_o2',
  		'hPa' = 'hPa',
  		'Torr' = 'torr',
  		'mg/L' = 'mg_per_l',
  		'umol/L' = 'umol_per_l'
  	)
  	f[, main_cols[-c(1:2)]] = sapply(main_cols[-c(1:2)], function(i) as.numeric(f[, i]))
  	n_o2_units = length(unique(f[, which(colnames(f) == 'O2') + 1]))
  	f$O2 = sapply(1:nrow(f), function(i){
  		conv_o2(o2 = f[i, 'O2'], from = as.character(o2_string_options[f[i, which(colnames(f) == 'O2') + 1]]), to = o2_unit, temp = f[i, 'TEMP'], sal = f[i, 'SAL'], atm_pres = measurements::conv_unit(x = f[i, 'ATM_PRES'], from = tolower(f[i, which(colnames(f) == 'ATM_PRES') + 1]), to = 'mbar'))
  	})
  	f[, 'TIME'] = as.data.frame(strptime(paste(f$DATE, f$TIME), paste(date, '%T')))
  	f$DATE = NULL
  	if(any(is.na(f$TIME))) stop(paste('The time record does not match', date, 'on at least some of the lines between', range(which(is.na(f$TIME)))[1], 'and', range(which(is.na(f$TIME)))[2]), call. = FALSE)
  	if(n_o2_units > 1) message(paste('Multiple units of O2 were recorded. All values were converted to', o2_unit))
  	if(any(f$ERROR_CODE != '0')) warning('Errors recorded during trial! Check ERROR_CODE column', call. = FALSE)
  	row.names(f) = NULL
  	if(!all_cols) f = f[, c('TIME', 'DURATION', 'O2', 'PHASE', 'AMPLITUDE', 'TEMP', 'ATM_PRES', 'SAL', 'ERROR_CODE')]
  }
	
	
	
	
	
	
	
	
	########################## PLATE READER TYPE FILE ##################################
	
	if(length(grep('Measurement Name : ', raw[1])) == 1){ # this is a Plate Reader type file
		f = gsub(pattern = ' ', replacement = '', raw)
		f = strsplit(f, split = ';', fixed = TRUE)
		o2_unit_measured = gsub('Parameter:Oxygen', '', grep('Parameter:Oxygen', f[1:30], value = TRUE))
		if(length(f[[32]]) - 1 == unique(sapply(f[33:length(f)], length))) f[[32]] = utils::head(f[[32]], n = length(f[[32]]) - 1) # If there are no errors then remove error column
		f = as.data.frame(matrix(unlist(f[32:length(f)]), ncol = length(f[[32]]), byrow = TRUE), stringsAsFactors = FALSE)
		colnames(f) = as.character(f[1, ])
		f = f[-1, ]
		f = f[, -which(colnames(f) %in% c('', 'T_internal[C]'))]
		if(!(o2_unit %in% names(conv_o2()))) stop('the o2_unit argument is not an acceptable unit', call. = FALSE)
		change_cols = which(colnames(f) %in% c('Date/Time', 'Time/Min.', 'Tm[C]', 'p[mbar]', 'Salinity[g/1000g]'))
		colnames(f)[change_cols] = c('TIME', 'DURATION', 'TEMP', 'ATM_PRES', 'SAL')  	
		o2_string_options = list(
			'[[%]AirSaturation]' = 'percent_a.s.',
			'[[%]O2]' = 'percent_o2',
			'[hPa]' = 'hPa',
			'[Torr]' = 'torr',
			'[mg/L]' = 'mg_per_l',
			'[umol/L]' = 'umol_per_l'
		)
		f[, -1] = sapply(colnames(f)[-1], function(i) as.numeric(f[, i]))
		o2_cols = grep('\\d', colnames(f))
		f[, o2_cols] = sapply(o2_cols, function(i) conv_o2(o2 = f[, i], from = o2_string_options[[o2_unit_measured]], to = o2_unit, temp = f$TEMP, sal = f$SAL, atm_pres = f$ATM_PRES))
		f$TIME = strptime(f$TIME, date)
		if(any(is.na(f$TIME))) stop(paste('The time record does not match', date, 'on at least some of the lines between', range(which(is.na(f$TIME)))[1], 'and', range(which(is.na(f$TIME)))[2]), call. = FALSE)
		row.names(f) = NULL
		if(split_channels){
			channel_names = colnames(f)[o2_cols]
			f = lapply(channel_names, function(i){
				f_sub = f[, c(i, colnames(f)[-o2_cols])]
				colnames(f_sub)[colnames(f_sub) == i] = 'O2'
				f_sub = f_sub[, c('TIME', 'DURATION', 'O2', 'TEMP', 'SAL', 'ATM_PRES')]
				if(any(is.na(f_sub$O2))){
					f_sub = f_sub[birk::range_seq(which(is.na(f_sub$O2) == FALSE)), ]
					f_sub$DURATION = as.numeric(difftime(time1 = f_sub$TIME, time2 = f_sub[1, 'TIME'], units = 'mins'))
					row.names(f_sub) = NULL
				}
				return(f_sub)
			})
			names(f) = channel_names
		}
	}
	return(f)
}