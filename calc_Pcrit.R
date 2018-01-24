library(dplyr)
library(ggplot2)
library(segmented)
source('/Users/matthewbirk/Documents/WDs/R/Packages/birk/birk_devel/R/set_plot_grid.R')



### CREATE O2_DATA CLASS THAT WILL BE SPECIAL DF WITH O2 UNIT METADATA


calc_Pcrit = function(duration, o2, o2_unit = 'percent_a.s.', bin_widths, vol, mass, temp = 25, sal = 35, atm_pres = 1013.25, time, good_data = TRUE, r2_threshold = 0, bg_mo2 = 0, report_o2_unit = 'kPa', ignore_above = 10, plots = FALSE, MO2_data = NULL){
	if(!is.null(MO2_data)){
		duration = MO2_data$DURATION
		o2 = MO2_data$O2
		temp = MO2_data$TEMP
		sal = MO2_data$SAL
	}
	o2 = conv_o2(o2, from = o2_unit, to = report_o2_unit, temp = temp, sal = sal, atm_pres = atm_pres)
	mo2s = lapply(bin_widths, function(i){
		inter1 = calc_MO2(duration = duration, o2 = o2, o2_unit = o2_unit, bin_width = i, vol = vol, temp = temp, sal = sal, good_data = good_data)
		inter1$MO2 = inter1$MO2 - bg_mo2
		inter1$MS_MO2 = inter1$MO2 / mass
		inter1 = cbind(bin_width = i, inter1)
		#inter1 = dplyr::filter(inter1, MO2 > 0)
		inter1 = dplyr::filter(inter1, R2 > r2_threshold)
	})
	if(plots){
		r2_list = lapply(mo2s, function(i){
			if(nrow(i) == 0) ggplot(i) + geom_blank() + ggtitle(paste('bin_width', i$bin_width, sep = ' = ')) else {
				ggplot(i, aes(O2_MEAN, MO2, colour = R2)) + geom_point() + ggtitle(paste('bin_width', i$bin_width, sep = ' = ')) + scale_colour_gradientn(limits = c(r2_threshold, 1), colours = rainbow(7), guide = FALSE) + theme_bw()
			}
		})
		do.call(gridExtra::grid.arrange, r2_list)
	}
	seg_pcrits = list()
	bins_pcrits = list()
	if(plots) set_plot_grid(x = length(mo2s))
	for(i in 1:length(mo2s)){
		inter1 = mo2s[[i]]
		if(nrow(inter1) <= 1) next
		inter2 = lm(MS_MO2 ~ O2_MEAN, inter1)
		tryCatch({
			set.seed(8412)
			inter3 = segmented::segmented(inter2, seg.Z = ~O2_MEAN, psi = sort(inter1$O2_MEAN)[2])
			final_pcrit = inter3$psi[, 'Est.'] # extract the converged breakpoint
			bins_pcrits[[i]] = data.frame(mean = final_pcrit, bin = unique(inter1$bin_width))
			if(plots){
				segmented::plot.segmented(inter3, res = TRUE, shade = TRUE, rug = FALSE, conf.level = 0.95, pch = 16, xlab = paste0('O2 (', report_o2_unit, ')'), ylab = expression(paste(M[O[2]], ' (', mu, 'mol ', O[2], ' ', g^-1, ' hr'^-1, ')')), main = paste0('binwidth = ', unique(inter1$bin_width), '\t\tn = ', nrow(inter1), '\t\tn_hypox = ', table(inter3$id.group)['0'], '\n95% CI = ', paste(segmented::confint.segmented(inter3, digits = 3)$O2_MEAN[, 2:3], collapse = ' - '), '\nConverged = ', round(final_pcrit, 4)))
				segmented::lines.segmented(inter3, col = 'red')
			}
			seg_pcrits[[i]] = inter3
		}, error = function(e){
			if(plots) plot(inter1$O2_MEAN, inter1$MS_MO2, main = 'No convergence', pch = 16, xlab = paste0('O2 (', report_o2_unit, ')'), ylab = expression(paste(M[O[2]], ' (', mu, 'mol ', O[2], ' ', g^-1, ' hr'^-1, ')')))
			seg_pcrits[[i]] = NA
		})
	}
	bins_pcrits = do.call('rbind', bins_pcrits)
	
	inter4 = sapply(seg_pcrits, function(i) table(i$id.group)['0'])
	seg_pcrits[inter4 <= 2] = NA # remove Pcrit values where the number of hypoxic measurements is 2 or less
	if(length(seg_pcrits) != 0) lapply(1:length(seg_pcrits), function(i) if(!is.null(seg_pcrits[[i]]) && !is.na(seg_pcrits[[i]])) if(seg_pcrits[[i]]$psi[, 'Est.'] >= ignore_above) seg_pcrits[i] <<- NA) # remove Pcrits > "ignore above"
	if(length(seg_pcrits) != 0 && !all(is.na(unlist(seg_pcrits)))){
		inter5 = sapply(seg_pcrits, function(i) if('segmented' %in% class(i)) diff(range(segmented::confint.segmented(i))) else NA)
		best_bin = which.min(inter5) # pull out best Pcrit value
		#best_bin = c(best_bin, which(inter5 == sort(inter5, na.last = TRUE)[2])) # pull out 2nd best Pcrit value
		best_pcrit = segmented::confint.segmented(seg_pcrits[[best_bin]])$O2_MEAN
		best_pcrit = data.frame(mean = best_pcrit[, 'Est.'], sd = as.numeric(diff(best_pcrit[, c('Est.', 'CI(95%).u')]) / 1.96))
		best_pcrit = cbind(best_pcrit, bin_width = bins_pcrits[which.min(abs(as.numeric(best_pcrit['mean']) - bins_pcrits$mean)), 'bin'])
		return_list = list(Pcrit = best_pcrit, MO2_data = mo2s[[which(sapply(mo2s, function(i) unique(i$bin_width)) == best_pcrit[, 'bin_width'])]])
		return(return_list)
	}
	
	
}
