#' @title Calculate peristaltic pump RPM
#' 
#' @description Given the number of moles of a gas, calculates the liters to run through a peristaltic pump.
#' 
#' @details
#' Most mass flow controllers are programmed with a "standard condition" something like 0 *C and 1013 mbar for which they account for the pressure and temperature of an incoming gas source. For setups without expensive mass flow controllers, a more affordable alternative is to use a peristaltic pump. These do not account for variations in incoming gas pressure and temperature and thus, it must be calculated to set the peristaltic pump to the correct RPM.
#' 
#' @param mol number of moles to go through a peristaltic pump.
#' @param species character string describing the gas species. Options are available from \code{\link{marelac::molvol}}. Default is "O2".
#' @param temp temperature (°C). Default is 25 °C.
#' @param reg_pres gauge pressure from the gas regulator into the peristaltic pump.
#' @param reg_unit unit used in \code{reg_pres}. Default is "psi".
#' @param atm_pres atmospheric pressure (mbar). Default is 1013.25 mbar.
#' 
#' @author Matthew A. Birk, \email{matthewabirk@@gmail.com}
#' @seealso \code{\link{co2_rate}}, \code{\link{co2_add}}
#'
#' @examples
#' peri_pump(mol = 0.5, species = 'O2', temp = 10, reg_pres = 5, reg_unit = "kPa") # To flow 0.5 moles of O2, then flow 11.1 L.
#'
#' @encoding UTF-8
#' @export
#' @import birk
#' @import marelac

peri_pump = function(mol, species = "O2", temp = 25, reg_pres, reg_unit = "psi", atm_pres = 1013.25){
	total_P = birk::conv_unit(atm_pres, 'mbar', 'bar') + birk::conv_unit(reg_pres, reg_unit, 'bar')
	unname(marelac::molvol(t = temp, P = total_P, species = species, quantity = mol))
}