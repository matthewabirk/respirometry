pkgname <- "respirometry"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "respirometry-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('respirometry')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Q10")
### * Q10

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Q10
### Title: Parameters of Q10 Temperature Coefficient
### Aliases: Q10

### ** Examples

Q10(R1 = 5, R2 = 10, T1 = 10, T2 = 20) # Returns Q10; = 2
Q10(Q10 = 2.66, R1 = 5, T1 = 10, T2 = 20) # Returns R2; = 13.3

# My species has an MO2 of 9.5 umol/g/h at 10 *C. What MO2 should I expect at 13 *C?
Q10(Q10 = 2, R1 = 9.5, T1 = 10, T2 = 13) # expect ~11.7 umol/g/h at 13 *C.

# A 100 g individual at 10 *C has an MO2 of 1270 umol/h. How much
# would a 250 g individual likely consume at 14 *C?
Q10(Q10 = 2, R1 = scale_MO2(mass_1 = 100, MO2_1 = 1270, mass_2 = 250), T1 = 10, T2 = 14)

# Visualize MO2 scaling by mass and temperature:
mass <- seq(10, 200, 10)
temp <- 10:25
base_mass <- 50
base_temp <- 20
base_MO2 <- 750
mo2 <- outer(mass, temp, function(mass, temp){
	scale_MO2(mass_1 = base_mass, mass_2 = mass, MO2_1 = Q10(Q10 = 2, R1 = base_MO2,
	 T1 = base_temp, T2 = temp))
})
persp(mass, temp, mo2, xlab = 'Mass (g)', ylab = 'Temperature (*C)', zlab = 'MO2 (umol / hr)',
 theta = 35, phi = 15, expand = 0.5, ticktype = 'detailed', nticks = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Q10", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RQ")
### * RQ

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RQ
### Title: Calculate respiratory quotient
### Aliases: RQ

### ** Examples

o2_observations <- c(21, 18, 14.5, 7)
pH_observations <- c(8.05, 7.98, 7.86, 7.65)
TA_observations <- c(2222, 2219, 2208, 2214)

RQ(o2 = o2_observations, o2_unit = 'kPa', pH = pH_observations,
TA = TA_observations, temp = 20, sal = 33)

DIC_observations <- c(2222, 2250, 2284, 2355)
RQ(o2 = o2_observations, o2_unit = 'kPa', DIC = DIC_observations)

RQ(o2 = o2_observations, o2_unit = 'kPa', pH = pH_observations, TA = 2032)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RQ", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_MO2")
### * calc_MO2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_MO2
### Title: Calculate metabolic rate
### Aliases: calc_MO2

### ** Examples

# get O2 data
file <- system.file('extdata', 'witrox_file.txt', package = 'respirometry')
o2_data <- na.omit(import_witrox(file, split_channels = TRUE)$CH_4)

# calculate MO2
(mo2_5_min <- calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
bin_width = 5, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL))

# what if measurements from the 10 to 12 minute marks can't be trusted?
bad_data = o2_data$DURATION >= 10 & o2_data$DURATION <= 12
(mo2_5_min <- calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
bin_width = 5, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL, good_data = !bad_data))

# easily make a Pcrit plot
plot(mo2_5_min$MEAN_O2, mo2_5_min$MO2)

# I want to express MO2 in mg per min instead.
(mo2_5_min$MO2 <- conv_resp_unit(value = mo2_5_min$MO2, from = 'umol_O2 / hr', to = 'mg_O2 / min'))

# just endpoint measurement:
calc_MO2(duration = o2_data$DURATION, o2 = o2_data$O2,
bin_width = Inf, vol = 10, temp = o2_data$TEMP, sal = o2_data$SAL)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_MO2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_b")
### * calc_b

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_b
### Title: Calculate the metabolic scaling coefficient, b
### Aliases: calc_b

### ** Examples

# Simple example
mass <- c(1, 10, 100, 1000, 40, 4, 400, 60, 2, 742, 266, 983) # made up values
MO2 <- mass ^ 0.65 + rnorm(n = length(mass)) # make up some data
calc_b(mass = mass, MO2 = MO2)

# How about some mass-specific MO2s?
msMO2 <- mass ^ -0.25 + rnorm(n = length(mass), sd = 0.05)
calc_b(mass = mass, MO2 = msMO2)
calc_b(mass = mass, MO2 = msMO2, plot = "log")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_b", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("closed")
### * closed

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: closed
### Title: Closed respirometry
### Aliases: closed

### ** Examples

# I've read in the literature that my animal has an SMR of 200 umol/h. How large of a
# respirometer do I want if I want it to breathe down to 80% air saturation in 30 minutes?
closed(MO2 = 200, delta_pO2 = 100 - 80, duration = 30) # returns respirometer volume

# I've read in the literature that my animal has an SMR of 1000 umol/h. How long will it take to
# breathe down a 50 L respirometer by 10% air saturation?
closed(MO2 = 1000, delta_pO2 = 10, vol = 50) # returns the duration to breathe down the O2

# How does animal size affect how long my measurement periods last?
mass_range <- seq(100, 400, 50)
dur_range <- (closed(MO2 = scale_MO2(mass_1 = 100, MO2_1 = 400, mass_2 = mass_range),
 delta_pO2 = 20, vol = 10))
plot(mass_range, dur_range, type = 'b')

# What is the MO2 if O2 drops 0.44 mg/l in 33 minutes when the respirometer volume is 30 L?
closed(delta_pO2 = conv_o2(o2 = 0.44, from = 'mg_per_l', to = 'percent_a.s.'), duration = 33,
 vol = 30)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("closed", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("co2_add")
### * co2_add

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: co2_add
### Title: Calculate CO2 to add to water
### Aliases: co2_add

### ** Examples

# I want the 50 L reservoir to have a pCO2 = 1000 uatm. It currently has a pH of 7.88.
# How many moles of CO2 gas should be added to the water to reach my desired pCO2?
co2_add(goal_pco2 = 1000, start_pH = 7.88, vol = 50)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("co2_add", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("co2_flush")
### * co2_flush

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: co2_flush
### Title: Calculate CO2 to add to flush reservoir
### Aliases: co2_flush

### ** Examples

# I want the respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.6 and is 90 L.
# If I have a 200 L reservoir with pH = 7.9 seawater, how much CO2 do I need
# to add to the flush reservoir?
co2_flush(goal_pco2 = 1000, resp_pH = 7.6, resp_vol = 90, flush_pH = 7.9, flush_vol = 200)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("co2_flush", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("co2_rate")
### * co2_rate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: co2_rate
### Title: Calculate CO2 to add to a respirometer intake flow
### Aliases: co2_rate

### ** Examples

# I want the respirometer to have a pCO2 = 1000 uatm. How much CO2 per minute do I need
# to add to the intake flow if the ambient pH is 8.1 and it is flowing at 3 LPM?
co2_rate(goal_pco2 = 1000, init_pH = 8.1, flow_rate = 3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("co2_rate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("conv_NH4")
### * conv_NH4

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: conv_NH4
### Title: Convert between units of ammonium
### Aliases: conv_NH4

### ** Examples

conv_NH4(n_waste = 100)
conv_NH4(n_waste = 100, from = 'mg_N')
conv_NH4(n_waste = 100, from = 'mg_N', to = 'umol_NH4')




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("conv_NH4", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("conv_o2")
### * conv_o2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: conv_o2
### Title: Convert between units of oxygen partial pressure and
###   concentration
### Aliases: conv_o2

### ** Examples

conv_o2(o2 = 50)
conv_o2(o2 = 1:50, from = "umol_per_l", to = "ml_per_l", temp = 10, sal = 0,
	atm_pres = 1100)
conv_o2()[c('mmHg','kPa')]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("conv_o2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("conv_resp_unit")
### * conv_resp_unit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: conv_resp_unit
### Title: Convert units related to respirometry
### Aliases: conv_resp_unit

### ** Examples

# I read that an animal's MO2 is 1.92 ml O2/kg/min. What is this MO2 in umol O2/g/h?
conv_resp_unit(value = 1.92, from = "ml_O2 / kg / min", to = "umol_O2 / g / hr")

# Krogh's diffusion coefficient for oxygen through gills can be expressed as ml O2 / mm2 (gill
# surface area) / um (gill thickness) / torr (seawater pO2 - blood pO2) / minute at a given
# temperature.
# To convert to another unit:
conv_resp_unit(value = 1e-6, from = "ml_O2 / mm2 / um / torr / min",
to = "umol_O2 / cm2 / um / kPa / hr", temp = 20)

# Now, with a knowledge of gill morphometrics, seawater pO2, and blood pO2, I can compare
# gill diffusion with whole animal MO2.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("conv_resp_unit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("correct_bubble")
### * correct_bubble

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: correct_bubble
### Title: Adjust respirometer volume for bubbles
### Aliases: correct_bubble

### ** Examples

correct_bubble(resp_vol = 50, bubble_vol = 10) # a 10 mL bubble makes a huge difference!

correct_bubble(resp_vol = 50, bubble_vol = 1, temp = 10, sal = 0) 
# in calculating MO2, a volume of 63.8 L should be used rather than the true 50 L.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("correct_bubble", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flush_carb")
### * flush_carb

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flush_carb
### Title: Estimate carbonate chemistry after a flush
### Aliases: flush_carb

### ** Examples

flush_carb(resp_vol = 90, flow_rate = 10, duration = 3, resp_pH = 7.8, flush_pH = 8.1)

# What will be the pH in the respirometer after this flush?
flush_carb(resp_vol = 90, flow_rate = 10, duration = 3, resp_pH = 7.8, flush_pH = 8.1)$pH




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flush_carb", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flush_o2")
### * flush_o2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flush_o2
### Title: Estimate dissolved O2 after a flush
### Aliases: flush_o2

### ** Examples

# What will be the pO2 in the respirometer after this flush?
flush_o2(resp_vol = 90, flow_rate = 10, duration = 3, resp_o2 = 15, flush_o2 = 21)

# I want to bring the pO2 back up to 95% air saturation. How long do I need to flush?
flush_o2(resp_vol = 20, flow_rate = 2, resp_o2 = 75, flush_o2 = 99, final_o2 = 95)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flush_o2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flush_water")
### * flush_water

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flush_water
### Title: Find percent of water exchanged after a flush
### Aliases: flush_water

### ** Examples

# What proportion of a 90 L respirometer is exchanged after 20 minutes of flow at 2 LPM?
flush_water(vol = 90, flow_rate = 2, duration = 20)

# Would it be worth it to extend the flush another five minutes? How much would that
# improve the exchange?
flush_water(vol = 90, flow_rate = 2, duration = 20, plot = TRUE)
# Another five minutes would increase exchange by nearly 10%.
# Perhaps that's worth the extra time...

# Visualize flushing
vol = 150
flow_rate = seq(0, 10, by = 0.5)
duration = 0:60
perc_fresh = outer(flow_rate, duration, function(flow_rate, duration){
	flush_water(vol = vol, flow_rate = flow_rate, duration = duration)
})
persp(flow_rate, duration, perc_fresh, xlab = 'Flow rate (LPM)', ylab = 'Duration (min)',
zlab = '% exchange', theta = 45, phi = 15, expand = 0.5, ticktype = 'detailed', nticks = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flush_water", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("goal_flush_pH")
### * goal_flush_pH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: goal_flush_pH
### Title: Calculate goal pH of a flush reservoir to achieve the post-flush
###   goal pCO2
### Aliases: goal_flush_pH

### ** Examples

# I want the respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.6 and is 90 L.
# If I have a 200 L reservoir which will be drained completely, what do I want
# the pH of the reservoir to be?
goal_flush_pH(goal_pco2 = 1000, resp_pH = 7.6, resp_vol = 90, flush_vol = 200)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("goal_flush_pH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("guess_TA")
### * guess_TA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: guess_TA
### Title: Estimate total alkalinity from salinity
### Aliases: guess_TA

### ** Examples

guess_TA(temp = 22, sal = 33)
guess_TA(temp = 12, sal = 33, region = "North Atlantic")
guess_TA(temp = 20, sal = 31:35)

guess_TA(sal = 31) # salinity is within bounds
guess_TA(sal = 30) # salinity is outside the bounds and TA is extrapolated
guess_TA(sal = 30, extend = FALSE) # do not extrapolate TA
guess_TA(sal = 25, extend = TRUE) # will not extrapolate with sal > 5 psu out of bounds




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("guess_TA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("guess_when")
### * guess_when

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: guess_when
### Title: Estimate when the O2 level will reach a defined level
### Aliases: guess_when

### ** Examples

guess_when(past_o2 = rnorm(n = 10, mean = 100:91), past_time = 1:10, goal_o2 = 75, plot = FALSE)
guess_when(past_o2 = rnorm(n = 10, mean = 100:91, sd = 5), past_time = 1:10, goal_o2 = 75)
# Viewing the plot can be helpful to see how trustworthy the prediction is
# when signal:noise is low.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("guess_when", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_firesting")
### * import_firesting

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_firesting
### Title: Import data from a FireSting O2 transmitter
### Aliases: import_firesting

### ** Examples

## Not run: 
##D file <- system.file('extdata', 'firesting_file.txt', package = 'respirometry')
##D import_witrox(file, o2_unit = 'umol_per_l')
##D 
##D # Oops. I forgot to change the salinity value when I calibrated
##D # the instrument. Override the values in the file for 35 psu.
##D import_witrox(file, o2_unit = 'umol_per_kg', overwrite_sal = 35)
##D 
##D # I want each channel as a separate data frame.
##D data_list <- import_witrox(file, split_channels = TRUE)
##D data_list$CH_3 # here's the channel 3 data frame.
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_firesting", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_presens")
### * import_presens

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_presens
### Title: Import data from a PreSens O2 transmitter
### Aliases: import_presens

### ** Examples

## Not run: 
##D 
##D # Import a Fibox 3 file.
##D file <- system.file('extdata', 'fibox_3_file.txt', package = 'respirometry')
##D import_presens(file, o2_unit = 'umol_per_l', sal = 25)
##D 
##D # Import a Fibox 4 file.
##D file <- system.file('extdata', 'fibox_4_file.csv', package = 'respirometry')
##D import_presens(file = file, date = '%d-%b-%Y')
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_presens", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_witrox")
### * import_witrox

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_witrox
### Title: Import data from a Loligo Systems Witrox O2 transmitter
### Aliases: import_witrox

### ** Examples

## Not run: 
##D file <- system.file('extdata', 'witrox_file.txt', package = 'respirometry')
##D import_witrox(file, o2_unit = 'umol_per_l')
##D 
##D # Oops. I forgot to change the salinity value when I calibrated
##D # the instrument. Override the values in the file for 35 psu.
##D import_witrox(file, o2_unit = 'umol_per_kg', overwrite_sal = 35)
##D 
##D # I want each channel as a separate data frame.
##D data_list <- import_witrox(file, split_channels = TRUE)
##D data_list$CH_3 # here's the channel 3 data frame.
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_witrox", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("max_MO2")
### * max_MO2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Encoding: UTF-8

### Name: max_MO2
### Title: Maximum MO2 supported by flow rate
### Aliases: max_MO2

### ** Examples

max_MO2(flow_rate = 1)

# What is the maximum MO2 organism I can place in my respirometer and still maintain at
# least 75% air saturation when the intake fresh water is 1.5 LPM, 10 °C and 90% air saturated?
(max_mo2 <- max_MO2(flow_rate = 1.5, min_pO2 = 75, pO2_in = 90, temp = 10, sal = 0))

# If a 300 g individual has an MO2 of 2000 umol/hr, how big of an animal can I use?
scale_MO2(mass_1 = 300, MO2_1 = 2000, MO2_2 = max_mo2) # I can almost support a 1 kg individual!




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("max_MO2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mean_pH")
### * mean_pH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mean_pH
### Title: Mean pH by [H+]
### Aliases: mean_pH

### ** Examples

mean_pH(c(7, 8)) # 7.26 rather than 7.5!




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mean_pH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("min_flow")
### * min_flow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Encoding: UTF-8

### Name: min_flow
### Title: Minimum flow rate to support MO2
### Aliases: min_flow

### ** Examples

min_flow(MO2 = 1000)

# What is the minimum flow rate required to maintain at least 75% air saturation in a
# respirometer with an organism(s) with an oxygen consumption rate of 1000 umol/h
# when the intake fresh water is 10 °C and 90% air saturated?
min_flow(MO2 = 1000, min_pO2 = 75, pO2_in = 90, temp = 10, sal = 0)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("min_flow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("peri_pump")
### * peri_pump

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: peri_pump
### Title: Calculate peristaltic pump gaseous flow rate
### Aliases: peri_pump

### ** Examples

peri_pump(mol = 0.5, species = 'O2', temp = 10, reg_pres = 5, reg_unit = "kPa")
# To flow 0.5 moles of O2, then flow 11.1 L.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("peri_pump", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict_NH4")
### * predict_NH4

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_NH4
### Title: Predict NH4 concentration post-respiration
### Aliases: predict_NH4

### ** Examples

predict_NH4(o2_drop = 25, o2_nh4_ratio = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_NH4", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict_pH")
### * predict_pH

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict_pH
### Title: Predict pH post-respiration
### Aliases: predict_pH

### ** Examples

predict_pH(end_o2 = 75, start_pH = 8.1)
predict_pH(start_o2 = 75, end_o2 = 50, start_pH = 7.96, temp = 15, sal = 33, RQ = 0.88)

# I know pH at the end was 7.8, but what was pH at the beginning?
predict_pH(start_o2 = 75, end_o2 = 100, start_pH = 8.013536) # reverse the order




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict_pH", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scale_MO2")
### * scale_MO2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scale_MO2
### Title: Scale metabolic rate by size
### Aliases: scale_MO2

### ** Examples

# I know a species has an SMR of 800 umol O2/h at 200 g.
# What would be a likely SMR for a 300 g individual?
scale_MO2(mass_1 = 200, MO2_1 = 800, mass_2 = 300)

# Some squids have a much higher scaling coefficient:
scale_MO2(mass_1 = 200, MO2_1 = 800, mass_2 = 300, b = 0.92)

# A 100 g individual at 10 *C has an MO2 of 1270 umol/h. How much
# would a 250 g individual likely consume at 14 *C?
Q10(Q10 = 2, R1 = scale_MO2(mass_1 = 100, MO2_1 = 1270, mass_2 = 250), T1 = 10, T2 = 14)

# Visualize MO2 scaling by mass and temperature:
mass <- seq(10, 200, 10)
temp <- 10:25
base_mass <- 50
base_temp <- 20
base_MO2 <- 750
mo2 <- outer(mass, temp, function(mass, temp){
	scale_MO2(mass_1 = base_mass, mass_2 = mass, MO2_1 = Q10(Q10 = 2, R1 = base_MO2,
	 T1 = base_temp, T2 = temp))
})
persp(mass, temp, mo2, xlab = 'Mass (g)', ylab = 'Temperature (*C)', zlab = 'MO2 (umol / hr)',
 theta = 35, phi = 15, expand = 0.5, ticktype = 'detailed', nticks = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scale_MO2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
