#' @title Mean pH by [H+]
#'
#' @description Calculates mean pH from a vector of pH values by averaging [H+] rather than numerical pH values.
#'
#' @details
#' Since pH is on a logarithmic scale, averaging pH values directly does not provide the true arithmetic mean of what is likely truly important to the organism, [H+] (however, see Boutilier and Shelton 1980). Thus, the pH values are converted to [H+] then averaged and converted back to a mean pH value.
#'
#' @param pH a numeric vector of pH values.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#'
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @references Boutilier RG, Shelton G. 1980. The statistical treatment of hydrogen ion concentration and pH. J Exp Biol. 84:335–339.
#' @examples
#' mean_pH(c(7, 8)) # 7.26 rather than 7.5!
#'
#' @encoding UTF-8
#' @export

mean_pH = function(pH, na.rm = FALSE, ...){
	if(is.null(pH)){
		warning('pH is NULL. Returning NA')
		return(NA)
	}
	-log10(mean(10^-pH, na.rm = na.rm, ...))
}