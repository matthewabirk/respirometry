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

Q10(R2 = 10, R1 = 5, T2 = 20, T1 = 10) # Returns Q10; = 2
Q10(Q10 = 2.66, R1 = 5, T2 = 20, T1 = 10) # Returns R2; = 13.3




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

o2_observations = c(21, 18, 14.5, 7)
pH_observations = c(8.05, 7.98, 7.86, 7.65)
TA_observations = c(2222, 2219, 2208, 2214)

RQ(o2 = o2_observations, o2_unit = 'kPa', pH = pH_observations,
TA = TA_observations, temp = 20, sal = 33)

DIC_observations = c(2222, 2250, 2284, 2355)
RQ(o2 = o2_observations, o2_unit = 'kPa', DIC = DIC_observations)

RQ(o2 = o2_observations, o2_unit = 'kPa', pH = pH_observations, TA = 2032)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RQ", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("co2_add")
### * co2_add

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: co2_add
### Title: Calculate CO2 to add to water
### Aliases: co2_add

### ** Examples

# I want the 50 L respirometer to have a pCO2 = 1000 uatm. It currently has a pH of 7.88.
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
guess_when(past_o2 = rnorm(n = 10, mean = 100:91, sd = 10), past_time = 1:10, goal_o2 = 75)
# Viewing the plot can be helpful to see how trustworthy the prediction is
# when signal:noise is low.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("guess_when", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
max_MO2(flow_rate = 1.5, min_pO2 = 75, pO2_in = 90, temp = 10, sal = 0)




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
