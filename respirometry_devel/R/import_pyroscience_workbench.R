#' Import data from Pyroscience Workbench
#'
#' Imports the raw channel data from Pyroscience Workbench output files. This allows "live" analyses while the trial is still running. This does not utilize the ".pyr" file, nor the text file that is created once the trial is finished. This utilizes the raw channel data found within the "ChannelData" folder that the software makes when the trial starts.
#'
#' @param folder a character string. The filepath to the parent folder (directory) which contains ChannelData.
#' @param o2_unit a character string. The unit of O2 measurement to be output in the data frame. Options are described in \code{\link{conv_o2}}.
#' @param sal numeric. If \code{o2_unit} is a concentration rather than partial pressure, the salinity of the chamber in which measurements were made must be entered here.
#' @param keep_metadata logical. Should metadata from the file be returned as extra columns in the returned data frame? Default is \code{FALSE}.
#' @param split_channels logical. Should a list of data frames be returned with a separate data frame for each channel? Default is \code{FALSE}.
#' @param merge_close_measurements used only when \code{split_channels = FALSE} (the default). The frequency during which measurements are taken can be set uniquely for each channel in the Pyroscience Workbench software. When this happens, measurements may, at times, be nearly synchronized. When measurements are close together in time (even if not exactly at the same moment), it may be desirable to merge them together in the same row of the output dataframe and consider them to be the same timepoint. This parameter allows you to control whether that happens, and if so, how close is "close enough". Options are:
#' \describe{
#' \item{\code{0}: }{Do not merge close measurements no matter how close in time (even if 1 msec apart).}
#' \item{"min" (default): }{Merge measurements as close as the most frequently sampled channel (e.g. if channel 1 sampled every 5 seconds, channel 2 every 2 seconds, and channel 3 every 10 seconds, then any measurements within 2 seconds of each other will be merged on the same row in the output dataframe.)}
#' \item{"max": }{Merge measurements as close as the least frequently sampled channel (e.g. if channel 1 sampled every 5 seconds, channel 2 every 2 seconds, and channel 3 every 10 seconds, then any measurements within 10 seconds of each other will be merged on the same row in the output dataframe. Warning: this will duplicate more frequent channels. Do not let your downstream statistics be altered by artificially raising the number of observations.)}
#' \item{custom-set numeric value: }{A numeric value specifying how many seconds apart is "close enough" to merge measurements to the same timepoint. This may result in duplications of the same observations across multiple rows or not merging multiple observations as expected. Examine the output carefully.}
#' }
#'
#' @return A data frame (or list of data frames) is returned.
#' \describe{
#' \item{TIME}{Date and time, POSIXct format. If \code{split_channels = FALSE} (default), then the timestamp is the average of all the measurements that were merged. For details, see \code{merge_close_measurements}.}
#' \item{DURATION}{Duration of measurement trial (minutes).}
#' \item{CH_X_O2}{Oxygen measurement in desired unit as determined by \code{o2_unit}.}
#' \item{CH_X_TEMP}{Temperature recorded or defined at beginning of measurement trial.}
#' \item{CH_X_SAL}{Salinity (psu). Only displayed if \code{sal != NULL}.}
#' \item{CH_X_STATUS}{Warning or error messages from Pyroscience Workbench file.}
#' \item{...}{Channel columns (CH_...) are repeated for each channel.}
#' }
#' If \code{keep_metadata = TRUE}, then the following columns are appended to the returned data frame:
#' \describe{
#' \item{CH_X_PHASE}{Phase recorded. Phase is inversely related to O2.}
#' \item{CH_X_INTENSITY}{Intensity is an indicator of the quality of the signal.}
#' \item{CH_X_AMB_LIGHT}{Ambient light on the sensor. Expressed in mV.}
#' \item{CH_X_T_STATUS}{Warning or error messages from Pyroscience Workbench file's temperature measurement.}
#' \item{CH_X_ATM_PRES}{Atmospheric pressure (mbar).}
#' \item{CH_X_P_STATUS}{Warning or error messages from Pyroscience Workbench file's atmospheric pressure measurement.}
#' }
#' 
#' If \code{split_channels = TRUE}, then "\code{CH_X_}" is removed from the column names and multiple data frames are returned in a named list.
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @note Oxygen conversions are estimates based on the \code{\link[marelac]{marelac}} package.
#' @seealso \code{\link{import_presens}}, \code{\link{import_witrox}}, \code{\link{conv_o2}}
#' @examples
#' \dontrun{
#' folder <- system.file('extdata/pyro_wb/', package = 'respirometry')
#' import_pyroscience_workbench(folder = folder, o2_unit = 'umol_per_l', sal = c(0, 35))
#' 
#' # I want each channel as a separate data frame.
#' data_list <- import_pyroscience_workbench(folder = folder, split_channels = TRUE)
#' data_list$CH_2 # here's the channel 2 data frame.
#' }
#'
#' @encoding UTF-8
#' @export
#' @importFrom rlang .data


import_pyroscience_workbench = function(folder, o2_unit = 'percent_a.s.', sal = NULL, keep_metadata = FALSE, split_channels = FALSE, merge_close_measurements = 'min'){
	
	if(!(o2_unit %in% c(names(conv_o2()), 'dphi'))) stop('the o2_unit argument is not an acceptable unit', call. = FALSE)
	
	o2_string_options = list(
		'%air sat.' = 'percent_a.s.',
		'%O2' = 'percent_o2',
		'mL/L' = 'ml_per_l',
		'\xb5mol/L' = 'umol_per_l',
		'mg/L' = 'mg_per_l',
		'\xb5g/L' = 'ug_per_l',
		'hPa' = 'hPa',
		'Torr' = 'mmHg'	
	)
	
	if(!dir.exists(folder)) stop('the folder (directory) does not exist.')
	if(!any(grepl('ChannelData', list.files(folder)))) stop('make sure the "folder" argument contains the path to the parent folder (directory) that contains the "ChannelData" folder (directory).')
	
	o2_channels = list.files(paste0(folder, '/ChannelData/'), pattern = 'Oxygen', full.names = TRUE)
	temp_channel = list.files(paste0(folder, '/ChannelData/'), pattern = 'TempPT100Port', full.names = TRUE)
	
	
	if(length(o2_unit) == 1 & length(o2_channels) > 1) o2_unit = rep(o2_unit, times = length(o2_channels))
	if(length(sal) == 1 & length(o2_channels) > 1) sal = rep(sal, times = length(o2_channels))

	channel_numbers = as.numeric(gsub('.*Ch\\.(\\d+).*', '\\1', basename(o2_channels)))	
	
	d = lapply(o2_channels, function(i){
		
		channel_order = which(i == o2_channels) # needed, e.g., in case channel 1 is inactive.

		o2 = utils::read.table(i, sep = '\t')
		
		o2_main_data = o2[, c(1, 2, 3, 4, 12, 8)]
		orig_o2_unit = gsub('Oxygen \\((.+)\\).+', '\\1', o2_main_data[1, 4], useBytes = TRUE)
		
		if(orig_o2_unit == 'dphi') warning('The O2 values in channel ', channel_numbers[channel_order], ' are expressed in "dphi" and cannot be converted to ', o2_unit[channel_order])
		
		colnames(o2_main_data) = c('date', 
															 'TIME', 
															 'DURATION', 
															 'CH_X_O2',
															 'CH_X_TEMP',
															 'CH_X_STATUS'
															 )
		
		o2_main_data = o2_main_data[-1, ]
		rownames(o2_main_data) = NULL
		tmp1 = c('DURATION', 'CH_X_O2', 'CH_X_TEMP')
		o2_main_data[tmp1] = sapply(o2_main_data[tmp1], as.numeric)
		
		o2_main_data$DURATION = o2_main_data$DURATION / 60
		
		o2_main_data$TIME = strptime(paste(o2_main_data$date, o2_main_data$TIME), format = '%d-%m-%Y %H:%M:%OS')
		o2_main_data$date = NULL
		o2_main_data$CH_X_SAL = sal[channel_order]
		
		
		o2_meta_data = o2[, c(5, 6, 7, 13, 17, 18)]
		colnames(o2_meta_data) = c('CH_X_PHASE', 
															 'CH_X_INTENSITY', 
															 'CH_X_AMB_LIGHT',
															 'CH_X_T_STATUS',
															 'CH_X_ATM_PRES',
															 'CH_X_P_STATUS'
															 )
		o2_meta_data = o2_meta_data[-1, ]
		rownames(o2_meta_data) = NULL
		tmp1 = grep('STATUS', colnames(o2_meta_data), invert = TRUE, value = TRUE)
		o2_meta_data[tmp1] = sapply(o2_meta_data[tmp1], as.numeric)
		
		
		o2_main_data$CH_X_O2 = conv_o2(o2 = o2_main_data$CH_X_O2,
						from = o2_string_options[[orig_o2_unit]],
						to = o2_unit[channel_order],
						temp = o2_main_data$CH_X_TEMP,
						sal = sal[channel_order],
						atm_pres = o2_meta_data$CH_X_ATM_PRES
		)
		
		colnames(o2_main_data) = gsub('CH_X', paste0('CH_', channel_numbers[channel_order]), colnames(o2_main_data))
		colnames(o2_meta_data) = gsub('CH_X', paste0('CH_', channel_numbers[channel_order]), colnames(o2_meta_data))
		
		
		
		
		if(keep_metadata) return(cbind(o2_main_data, o2_meta_data)) else return(o2_main_data)
	})
	
	error_statuses = sapply(d, function(i){
		any(grepl('NotOK', i[, grep('STATUS', colnames(i))]))
	})
	
	if(any(error_statuses)){
		warning(paste('Some measurements in the following channel(s) had an error message:', paste0(channel_numbers[error_statuses], collapse = ', ')), call. = FALSE)
	}
	
	
	
	if(split_channels == FALSE & length(d) > 1){
		if(merge_close_measurements != 0){
			if(!(merge_close_measurements %in% c('min', 'max'))) warning('merge_close_measurements was custom-set and may result in duplications of the same observations across multiple rows or not merging multiple observations as expected. Examine the output carefully.', call. = FALSE)
			hertzs = round(sapply(d, function(i) stats::median(i$DURATION - dplyr::lag(i$DURATION), na.rm = TRUE) * 60), 1) # find the typical hertz of each channel. Median rather than mean to ignore times when measurements are paused.
			if(merge_close_measurements == 'min' ) merge_close_measurements = min(hertzs)
			if(merge_close_measurements == 'max' ) merge_close_measurements = max(hertzs)
				
				
			d = lapply(d, function(i){
				i$DURATION = plyr::round_any(i$DURATION, accuracy = merge_close_measurements / 60)
				return(i)
			})
		}
		 d_merged = d[[1]]
		 for(i in 2:length(d)){
		 	d_merged = dplyr::full_join(x = d_merged, y = d[[i]], by = 'DURATION', suffix = c('', i), relationship = 'many-to-many')
		 	d_merged = dplyr::arrange(d_merged, .data$DURATION)
		 }
		 
		 time_cols = grepl('TIME', colnames(d_merged))
		 time = apply(d_merged[, time_cols], 1, function(i){
		 	mean(as.POSIXct(i), na.rm = TRUE)
		 })
		 
		 d_merged = d_merged[, !time_cols]
		 d_merged$TIME = as.POSIXct(time)
		 d_merged = dplyr::relocate(d_merged, 'TIME')
		 
		return(d_merged)
	}
	
	if(split_channels == FALSE & length(d) == 1) return(d[[1]])
	
	if(split_channels == TRUE){
		d = lapply(d, function(i){
			colnames(i) = gsub('CH_\\d+_', '', colnames(i))
			return(i)
		})
		names(d) = paste0('CH_', channel_numbers)
		return(d)
	}
}
