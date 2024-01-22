pcrit_internal = function(po2, mo2, avg_top_n = 1, level = 0.95, iqr = 1.5, NLR_m = 0.065, MR = NULL, mo2_threshold = Inf){
	df = data.frame(po2 = po2, mo2 = mo2)
	
	# BREAKPOINT METHOD
	
	set.seed(8412)
	seg = segmented::segmented(stats::lm(mo2 ~ po2, data = df), seg.Z = ~po2)
	breakpoint = seg$psi[, 'Est.']
	df_reg = df[which(po2 > breakpoint), ]
	df_reg = df_reg[which(abs(df_reg$mo2 - mean(df_reg$mo2, na.rm = TRUE)) < (stats::IQR(df_reg$mo2) * iqr)), ]
	reg_lm = stats::lm(mo2 ~ po2, data = df_reg)
	po2_range =  birk::range_seq(po2, by = 0.01)
	pi = stats::predict(reg_lm, newdata = data.frame(po2 = po2_range), interval = 'prediction', level = level)
	pi_ll = pi[, 'lwr']
	pi_ul = pi[, 'upr']
	
	conform_slope = segmented::slope(seg)$po2[1, 1]
	conform_int = segmented::intercept(seg)$po2[1, 1]
	conform = conform_slope * po2_range + conform_int
	
	sub_PI = po2_range[utils::tail(which(conform < pi_ll), 1)]
	
	
	
	
	
	


	# NONLINEAR REGRESSION METHOD (Marshall et al. 2013)

	normalize_factor = max(df$mo2, na.rm = TRUE)
	normalize_factor = as.numeric(stats::quantile(mo2, probs = 0.9, na.rm = TRUE))
	df$mo2_norm = df$mo2 / normalize_factor
	start_mo2 = stats::median(df$mo2_norm[df$po2 > (max(df$po2, na.rm = TRUE) / 4)], na.rm = TRUE)
	m = NLR_m
	
	MM = function(po2, a, b) a * po2 / (b + po2)
	MM_mod = tryCatch(stats::nls(mo2_norm ~ MM(po2, a, b), data = df, start = list(a = start_mo2, b = 1)), error = function(e) NA)
	if(!any(is.na(MM_mod))){
		MM_c = stats::coef(MM_mod)
		MM_pcrit = as.numeric(sqrt(MM_c['a'] * MM_c['b'] / m) - MM_c['b'])
	}else MM_pcrit = NA
	

	powr = function(po2, a, b) a * po2 ^ b
	powr_mod = tryCatch(stats::nls(mo2_norm ~ powr(po2, a, b), data = subset(df, po2 >= 0), start = list(a = start_mo2, b = 1)), error = function(e) NA)
	if(!any(is.na(powr_mod))){
		powr_c = stats::coef(powr_mod)
		powr_pcrit = as.numeric((m / powr_c['a'] * powr_c['b']) ^ (1 / (powr_c['b'] - 1)))
	}else powr_pcrit = NA
	

	hyperbola = function(po2, a, b, c) a * po2 / (b + po2) + c
	hyperbola_mod = tryCatch(stats::nls(mo2_norm ~ hyperbola(po2, a, b, c), data = df, start = list(a = start_mo2, b = 1, c = 0)), error = function(e) NA)
	if(!any(is.na(hyperbola_mod))){
		hyperbola_c = stats::coef(hyperbola_mod)
		hyperbola_pcrit = as.numeric(sqrt(hyperbola_c['a'] * hyperbola_c['b'] / m) - hyperbola_c['b'])
	}else hyperbola_pcrit = NA
	
	
	pareto = function(po2, a, b) 1 - (a * po2) ^ b
	pareto_mod = tryCatch(stats::nls(mo2_norm ~ pareto(po2, a, b), data = subset(df, po2 >= 0), start = list(a = start_mo2, b = -1)), error = function(e) NA)
	if(!any(is.na(pareto_mod))){
		pareto_c = stats::coef(pareto_mod)
		pareto_pcrit = as.numeric(abs(m / (pareto_c['a']^pareto_c['b'] * pareto_c['b'])) ^ (1 / (pareto_c['b'] - 1)))
	}else pareto_pcrit = NA
	

	weibull = function(po2, a, b, c, d) a * (1 - exp(-(po2 / b) ^ c)) + d
	weibull_mod = tryCatch(minpack.lm::nlsLM(mo2_norm ~ weibull(po2, a, b, c, d), data = subset(df, po2 >= 0), start = list(a = 30, b = 0.0013, c = 0.2, d = -30), control = list(maxiter = 500)), error = function(e) NA)
	if(!any(is.na(weibull_mod))){
		weibull_c = stats::coef(weibull_mod)
		po2_vec = seq(0, 21, by = 0.001)
		weibull_pcrit = po2_vec[birk::which.closest(weibull_c['a'] * weibull_c['c'] / weibull_c['b'] * (po2_vec / weibull_c['b']) ^ (weibull_c['c'] - 1) * exp(-(po2_vec / weibull_c['b']) ^ weibull_c['c']), m)]
	}else weibull_pcrit = NA

	
	mods = list(MM_mod, powr_mod, hyperbola_mod, pareto_mod, weibull_mod)
	mod_names = c('MM_mod' = 'Michaelis-Menten', 'powr_mod' = 'Power', 'hyperbola_mod' = 'Hyperbola', 'pareto_mod' = 'Pareto', 'weibull_mod' = 'Weibull with intercept')
	aic_vec <- sapply(mods, FUN = AIC)
	best_mod = mod_names[which.min(aic_vec)]

	
	nlr_pcrits = list('Michaelis-Menten' = MM_pcrit, 'Power' = powr_pcrit, 'Hyperbola' = hyperbola_pcrit, 'Pareto' = pareto_pcrit, 'Weibull with intercept' = weibull_pcrit)
	
	nlr_mods = list('Michaelis-Menten' = MM_mod, 'Power' = powr_mod, 'Hyperbola' = hyperbola_mod, 'Pareto' = pareto_mod, 'Weibull with intercept' = weibull_mod)
	
	
	
	
	# LLO METHOD (Reemeyer and Rees 2019)
	
	if(is.null(MR)){
		LLO_pcrit = NA
		df_LLO = NULL
		message('"MR" must be defined for LLO calculation.')
	}else{
		df_ordered = df[order(po2, decreasing = TRUE), ]
		df_LLO = df_ordered[(utils::tail(which(df_ordered$mo2 >= MR), 1) + 1):nrow(df_ordered), ]
		lm_LLO = stats::lm(mo2 ~ po2, data = df_LLO)
		LLO_pcrit = as.numeric((MR - stats::coef(lm_LLO)['(Intercept)']) / stats::coef(lm_LLO)['po2'])
	}
	
	
	# ALPHA METHOD
	
	if(is.null(MR)) MR_mean_reg = mean(df_reg$mo2, na.rm = TRUE)
	alpha = calc_alpha(po2 = po2, mo2 = mo2, avg_top_n = avg_top_n, mo2_threshold = mo2_threshold, MR = ifelse(is.null(MR), MR_mean_reg, MR))
	
	if(!is.null(MR)) MR_mean_reg = 'Not used.'
	
	
	
	
	# OUTPUT
	
	list(model = seg, 
			 breakpoint = breakpoint, 
			 sub_PI = sub_PI, 
			 pcrit_alpha = alpha$pcrit_alpha,
			 pcrit_LLO = LLO_pcrit,
			 alpha = alpha$alpha, 
			 alpha_obs = alpha$alpha_obs, 
			 MR = MR,
			 MR_mean_reg = MR_mean_reg,
			 reg_data = df_reg, 
			 po2_range = po2_range, 
			 pi_ll = pi_ll, 
			 pi_ul = pi_ul, 
			 conform = conform, 
			 nlr_pcrits = nlr_pcrits, 
			 nlr_mods = nlr_mods, 
			 best_mod = best_mod, 
			 nlr_normalize_factor = normalize_factor, 
			 df_LLO = df_LLO[, c('po2', 'mo2')])
}


#' @title Calculate Pcrit
#' 
#' @description Calculates Pcrit (commonly understood as the threshold below which oxygen consumption rate can no longer be sustained) based on paired PO2 and MO2 values. Five Pcrit metrics are returned using many of the popular techniques for Pcrit calculation: the traditional breakpoint metric (broken stick regression), the nonlinear regression metric (Marshall et al. 2013), the sub-prediction interval metric (Birk et al. 2019), the alpha-based Pcrit method (Seibel et al. 2021), and the linear low O2 (LLO) method (Reemeyer & Rees 2019). To see the Pcrit values plotted, see \code{\link{plot_pcrit}}.
#' 
#' @details
#' \describe{
#' \item{Alpha Pcrit}{Alpha is calculated from \code{\link{calc_alpha}} and the Pcrit corresponding to \code{MR} is returned. This determine's the animal's oxygen supply capacity and calculates the Pcrit at any given metabolic rate of interest. If no \code{MR} is provided, then it defaults to the mean MO2 value from the oxyregulating portion of the curve (as defined by the broken-stick regression).}
#' \item{Breakpoint Pcrit}{Data are fit to a broken-stick regression using \code{\link[segmented]{segmented}}.}
#' \item{LLO Pcrit}{A subset of observations are chosen only from those with an MO2 < \code{MR}. Then, a linear model is fit through the observations and Pcrit is calculated as the PO2 at which the line reaches \code{MR}.}
#' \item{NLR Pcrit}{Data are fit to the following functions: Michaelis-Menten, Power, Hyperbola, Pareto, and Weibull with intercept. Following the method developed by Marshall et al. 2013, the function that best fits the data (smallest AIC) is chosen and the Pcrit is determined as the PO2 at which the slope of the function is \code{NLR_m} (by default = 0.065 following the authors' suggestion).}
#' \item{Sub_PI Pcrit}{This metric builds off the \code{Breakpoint} metric and results in a systematically lower Pcrit value. This is useful for applications where it is important to ensure that Pcrit is not being overestimated. It represents a reasonable lower bounded estimate of the Pcrit value for a given trial. Once the \code{Breakpoint} Pcrit is calculated, a 95\% prediction interval (can be changed with the \code{level} argument) is calculated around the oxyregulating region (i.e. using PO2 values > breakpoint Pcrit). By default, \code{iqr} provides some filtering of abberant observations to prevent their influence on the calculated prediction interval. Finally, the Sub_PI Pcrit value is returned at the intersection of the oxyconforming line and the lower limit of the oxyregulating prediction interval.}
#' }
#' 
#' @param po2 a vector of PO2 values. Any unit of measurement should work, but the NLR calculation was optimized using kPa. If the NLR metric is giving you trouble, try converting to kPa using \code{\link{conv_o2}}.
#' @param mo2 a vector of metabolic rate values. Must be the same length and corresponding to \code{po2}.
#' @param avg_top_n applies to the \code{alpha} metric only. A numeric value representing the number of top \eqn{\alpha0} (MO2/PO2) values to average together to estimate \eqn{\alpha}. Default is 1. We recommend no more than 3 to avoid diminishing the \eqn{\alpha} value with sub-maximal observations.
#' @param level applies to the \code{Sub_PI} metric only. Percentage at which the prediction interval should be constructed. Default is 0.95.
#' @param iqr applies to the \code{Sub_PI} metric only. Removes \code{mo2} observations that are this many interquartile ranges away from the mean value for the oxyregulating portion of the trial. If this filtering is not desired, set to infinity. To visualize which observations will be removed by this parameter, use \code{\link{plot_pcrit}}. Default is 1.5.
#' @param NLR_m applies to the \code{NLR} metric only. Pcrit is defined as the PO2 at which the slope of the best fitting function equals \code{NLR_m} (after the MO2 data are normalized to the 90\% quantile). Default is 0.065.
#' @param MR applies to the \code{alpha} and \code{LLO} metrics only. A numeric value for the metabolic rate at which \code{pcrit_alpha} and \code{pcrit_LLO} should be returned. If not supplied by the user, then the mean MO2 of the "oxyregulating" portion of the curve is applied for \code{pcrit_alpha} and \code{NA} is returned for \code{pcrit_LLO}.
#' @param mo2_threshold applies to the \code{alpha} metric only. A single numeric value above which \code{mo2} values are ignored for \code{alpha} Pcrit estimation. Useful to removing obviously erroneous values. Default is \code{Inf}.
#' @param return_models logical. Should a list of model parameters be returned along with the converged Pcrit values? Default is \code{FALSE}.
#'
#' @return If \code{return_models} is \code{FALSE} (default), a named numeric vector of Pcrit values calculated using the \code{Alpha}, \code{Breakpoint}, \code{LLO}, \code{NLR}, and \code{Sub_PI} metrics. If \code{return_models} is \code{TRUE}, then a list of converged Pcrit values, along with breakpoint function parameters, the \code{MR} value used for calculating Pcrit-alpha, a data frame of the "oxyregulating" portion of the curve, and NLR parameters are returned.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references 
#' Birk, Matthew A., K.A.S. Mislan, Karen F. Wishner, and Brad A. Seibel. 2019. “Metabolic Adaptations of the Pelagic Octopod Japetella Diaphana to Oxygen Minimum Zones.” Deep-Sea Research Part I 148: 123–31.
#' 
#' Marshall, Dustin J., Michael Bode, and Craig R. White. 2013. “Estimating Physiological Tolerances - a Comparison of Traditional Approaches to Nonlinear Regression Techniques.” Journal of Experimental Biology 216(12): 2176–82.
#' 
#' Reemeyer, Jessica E., and Bernard B. Rees. 2019. “Standardizing the Determination and Interpretation of Pcrit in Fishes.” Journal of Experimental Biology 222(18): jeb210633.
#' 
#' Seibel, B. A., A. Andres, M. A. Birk, A. L. Burns, C. T. Shaw, A. W. Timpe, C. J. Welsh. 2021. “Oxygen supply capacity breathes new life into the critical oxygen partial pressure (Pcrit).” Journal of Experimental Biology.
#' 
#' 
#' @seealso \code{\link{plot_pcrit}}, \code{\link{calc_MO2}}, \code{\link{conv_o2}}, \code{\link{calc_alpha}}
#' 
#' @examples
#' mo2_data <- read.csv(system.file('extdata', 'mo2_v_po2.csv', package = 'respirometry'))
#' calc_pcrit(po2 = mo2_data$po2, mo2 = mo2_data$mo2)
#' @encoding UTF-8
#' @export
#' @importFrom stats AIC
#' @import PKNCA

calc_pcrit = function(po2, mo2, avg_top_n = 1, level = 0.95, iqr = 1.5, NLR_m = 0.065, MR = NULL, mo2_threshold = Inf, return_models = FALSE){
	l = pcrit_internal(po2, mo2, avg_top_n, level, iqr, NLR_m, MR = MR, mo2_threshold = mo2_threshold)
	tmp = c(Alpha = l$pcrit_alpha, Breakpoint = l$breakpoint, LLO = l$pcrit_LLO, NLR = ifelse(length(l$best_mod) > 0, l$nlr_pcrits[[l$best_mod]], NA), Sub_PI = l$sub_PI)
	if(return_models){
		breakpoint_params = c(stats::coef(l$model)[1:3], tmp['Breakpoint'])
		breakpoint_params[3] = breakpoint_params[3] + breakpoint_params[2]
		names(breakpoint_params) = c('MO2 intercept', 'Oxyconforming slope', 'Oxyregulating slope', 'PO2 breakpoint')
		oxyreg_df = l$reg_data
		NLR_params = list(
			'Michaelis-Menten (a * po2 / (b + po2))' = if(!is.na(l$nlr_pcrits$`Michaelis-Menten`)) stats::coef(l$nlr_mods$`Michaelis-Menten`),
			'Power (a * po2 ^ b)' = if(!is.na(l$nlr_pcrits$Power)) stats::coef(l$nlr_mods$Power),
			'Hyperbola (a * po2 / (b + po2) + c)' = if(!is.na(l$nlr_pcrits$Hyperbola)) stats::coef(l$nlr_mods$Hyperbola),
			'Pareto (1 - (a * po2) ^ b)' = if(!is.na(l$nlr_pcrits$Pareto)) stats::coef(l$nlr_mods$Pareto),
			'Weibull with intercept (a * (1 - exp(-(po2 / b) ^ c)) + d)' = if(!is.na(l$nlr_pcrits$`Weibull with intercept`)) stats::coef(l$nlr_mods$`Weibull with intercept`)
		)
		tmp = list(converged_Pcrit_values = tmp, breakpoint_params = breakpoint_params, MR_input = l$MR, oxyreg_df = oxyreg_df, NLR_params = NLR_params, LLO_obs_used = l$df_LLO)
	}
	return(tmp)
}







#' @title Plot Pcrit
#' 
#' @description Creates a Pcrit plot (the threshold below which oxygen consumption rate can no longer be sustained) based on paired PO2 and MO2 values. Five Pcrit metrics are plotted: the traditional breakpoint metric (broken stick regression, black), the nonlinear regression metric (Marshall et al. 2013, green), the sub-prediction interval metric (Birk et al. 2019, red), the alpha-based Pcrit method (Seibel et al., 2021, blue), and the linear low O2 (LLO) method (Reemeyer & Rees 2019, purple). For details on how the Pcrit values are calculated, see \code{\link{calc_pcrit}}.
#' 
#' @inheritParams calc_pcrit
#' @param showNLRs logical. Should all the NLR functions be plotted in a second plot? If \code{FALSE} then only the best fit NLR function will be plotted.
#' @param ... arguments to be passed to \code{\link[segmented]{plot.segmented}}.
#'
#' @return A base graphic plot is created. The alpha, breakpoint, LLO, NLR, and sub-PI Pcrit values are shown in the title and on the plot by inverted triangles. 
#' 
#' The broken-stick regression is shown by black lines. 
#' 
#' The dashed red curves signify the prediction interval used for the sub-PI Pcrit metric. 
#' 
#' Black circles represent oxyregulating observations used in the generation of the prediction interval, while transparent circles represent both the oxyconforming observations and those observations outside the IQR threshold (defined by \code{iqr}). 
#' 
#' The gray bands represent the confidence interval (defaults to 95\% but will change with \code{level}). 
#' 
#' The green curve represents the best fitting NLR function and the green inverted triangle represents the NLR Pcrit (modified by \code{NLR_m}) 
#' 
#' The blue line represents alpha, which was fit based on the blue circle observation(s).
#' 
#' If \code{showNLRs = TRUE}, then a second plot is generated which shows all the NLR functions that converged. Vertical lines represent the Pcrit values corresponding to each curve.
#' 
#'     Black = Michaelis-Menten
#' 
#'     Red = Power
#' 
#'     Green = Hyperbola
#' 
#'     Blue = Pareto
#' 
#'     Cyan = Weibull with intercept.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{calc_pcrit}}, \code{\link{calc_alpha}}
#' 
#' @examples
#' mo2_data <- read.csv(system.file('extdata', 'mo2_v_po2.csv', package = 'respirometry'))
#' plot_pcrit(po2 = mo2_data$po2, mo2 = mo2_data$mo2, avg_top_n = 3, MR = 2.2)
#' 
#' par(mfrow = c(2, 1))
#' plot_pcrit(po2 = mo2_data$po2, mo2 = mo2_data$mo2, showNLRs = TRUE)
#'
#' @encoding UTF-8
#' @inherit calc_pcrit details references
#' @export
#' @importFrom stats AIC
#' @import PKNCA



plot_pcrit = function(po2, mo2, avg_top_n = 1, level = 0.95, iqr = 1.5, NLR_m = 0.065, MR = NULL, mo2_threshold = Inf, showNLRs = FALSE, ...){
	tryCatch({
		l = pcrit_internal(po2, mo2, avg_top_n, level, iqr, NLR_m, MR = MR, mo2_threshold = mo2_threshold)
		inter1 = level
		title_values = list(alpha_MR = round(ifelse(is.null(MR), l$MR_mean_reg, l$MR), 2),
												alpha = round(l$pcrit_alpha, 3),
												breakpoint = round(l$breakpoint, 3),
												LLO_MR = ifelse(is.null(MR), NA, round(l$MR, 2)),
												LLO = round(l$pcrit_LLO, 3),
												NLR = round(ifelse(length(l$best_mod) > 0, l$nlr_pcrits[[l$best_mod]], NA), 3),
												subPI = round(l$sub_PI, 3))
		plot_pcrit_internal = function(l, level = inter1, shade = TRUE, ...){
			mar = graphics::par()$mar
			mar[3] = 6.1
			graphics::par(mar = mar)
			graphics::plot(l$model, res = TRUE, shade = shade, rug = FALSE, conf.level = level, ...)
			graphics::title(main = paste0('Alpha @ MR of ', title_values$alpha_MR,' = ', title_values$alpha), col.main = 'blue', line = 5)
			graphics::title(main = paste0('Breakpoint = ', title_values$breakpoint), col.main = 'black', line = 4)
			graphics::title(main = paste0('LLO @ MR of ', title_values$LLO_MR,' = ', title_values$LLO), col.main = 'purple', line = 3)
			graphics::title(main = paste0('NLR (', l$best_mod, ') = ', title_values$NLR), col.main = 'green', line = 2)
			graphics::title(main = paste0('Sub-PI = ', title_values$subPI), col.main = 'red', line = 1)
		}
		plot_pcrit_internal(l, ...)
		graphics::points(l$reg_data$po2, l$reg_data$mo2, pch = 16, ...)
		graphics::points(x = l$breakpoint, y = stats::coef(l$model)[1] + stats::coef(l$model)[2] * l$breakpoint, bg = 'black', pch = 25)
		graphics::lines(l$po2_range, l$pi_ll, lty = 2, col = 'red', ...)
		graphics::lines(l$po2_range, l$pi_ul, lty = 2, col = 'red', ...)
		graphics::points(x = l$sub_PI, y = l$conform[utils::tail(which(l$conform < l$pi_ll), 1)], bg = 'red', col = 'red', pch = 25, ...)
		graphics::abline(a = 0, b = l$alpha, lty = 3, col = 'blue')
		graphics::points(x = l$pcrit_alpha, y = l$pcrit_alpha * l$alpha, bg = 'blue', col = 'blue', pch = 25)
		graphics::points(x = l$pcrit_LLO, y = l$MR, bg = 'purple', col = 'purple', pch = 25)
		for(i in l$alpha_obs) graphics::points(x = i['po2'], y = i['mo2'], col = 'blue', pch = 16)
		if(length(l$best_mod) > 0) graphics::lines(po2, stats::predict(l$nlr_mods[[l$best_mod]], newdata = list(po2 = po2)) * l$nlr_normalize_factor, col = 'green', ...)
		if(length(l$best_mod) > 0) graphics::points(x = l$nlr_pcrits[[l$best_mod]], y = stats::predict(l$nlr_mods[[l$best_mod]], newdata = data.frame(po2 = l$nlr_pcrits[[l$best_mod]])) * l$nlr_normalize_factor, bg = 'green', col = 'green', pch = 25, ...)
		if(showNLRs){
			graphics::plot(po2, mo2, main = paste(sum(!is.na(l$nlr_mods)), 'of', length(l$nlr_mods), 'NLR models fit.'))
			sapply(names(l$nlr_pcrits), function(i){
				if(any(!is.na(l$nlr_mods[[i]]))) graphics::lines(po2, stats::predict(l$nlr_mods[[i]], newdata = list(po2 = po2)) * l$nlr_normalize_factor, col = which(names(l$nlr_pcrits) == i))
			})
			if(length(l$best_mod) > 0) graphics::lines(po2, stats::predict(l$nlr_mods[[l$best_mod]], newdata = list(po2 = po2)) * l$nlr_normalize_factor, lwd = 2, ...)
			sapply(l$nlr_pcrits, function(i) graphics::abline(v = i, col = which(l$nlr_pcrits == i)))
		}
	}, error = function(e){
		graphics::plot(po2, mo2, main = 'Could not calculate a Pcrit. Plotting just the values...')
	})
	
}
