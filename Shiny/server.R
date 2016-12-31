library(shiny)
library(respirometry)
library(measurements)
library(ggplot2)

comma_num_vec = function(x) as.numeric(unlist(strsplit(x, '\\s*,\\s*')))

function(input, output){

# conv_o2 -----------------------------------------------------------------

	output$conv_o2 = renderTable({
  	x = do.call(rbind.data.frame, conv_o2(o2 = input$conv_o2.o2, from = input$conv_o2.from, temp = input$conv_o2.temp, sal = input$conv_o2.sal, atm_pres = input$conv_o2.atm_pres))
  	colnames(x) = 'Value'
  	x$Unit = names(o2_options)
  	rownames(x) = NULL
  	x = x[, c('Unit', 'Value')]
  }, include.rownames = FALSE)

# conv_NH4 -----------------------------------------------------------------
	
	output$conv_NH4 = renderTable({
		x = do.call(rbind.data.frame, conv_NH4(n_waste = input$conv_NH4.n_waste, from = input$conv_NH4.from))
		colnames(x) = 'Value'
		x$Unit = names(nh4_options)
		rownames(x) = NULL
		x = x[, c('Unit', 'Value')]
	}, include.rownames = FALSE)
	

# Q10 ---------------------------------------------------------------------

	#output$Q10 = renderText()
	
# correct_bubble ----------------------------------------------------------
	
	cb = reactive(round(correct_bubble(resp_vol = input$bubble.resp_vol, bubble_vol = input$bubble.bubble_vol, temp = input$bubble.temp, sal = input$bubble.sal, atm_pres = input$bubble.atm_pres), 2))
	output$correct_bubble.orig_vol = renderText(input$bubble.resp_vol)
	output$correct_bubble.vol_equiv = renderText(round(cb() - input$bubble.resp_vol, 2))
	output$correct_bubble.final_vol = renderText(cb())
	
	
	
	
	# Respirometer size -------------------------------------------------------
	
	size = reactive({
		known_mo2 = input$size.mo2
		temps = known_temp = input$size.temp
		sals = known_sal = input$size.sal
		masses = known_mass = input$size.mass
		
		if('Temperature' %in% input$size_treatments) temps = comma_num_vec(input$size.temp_treats)
		if('Salinity' %in% input$size_treatments) sals = comma_num_vec(input$size.sal_treats)
		if('Animal mass' %in% input$size_treatments) masses = comma_num_vec(input$size.mass_treats)
		
		options = expand.grid(list(temp = temps, sal = sals, mass = masses))
		options$MO2 = Q10(Q10 = input$size.Q10, R1 = scale_MO2(mass_1 = known_mass, MO2_1 = known_mo2, mass_2 = options$mass, b = input$size.b), T1 = known_temp, T2 = options$temp)
		options$resp_size = closed(MO2 = options$MO2, delta_pO2 = 20, duration = 60, temp = options$temp, sal = options$sal)
		options
		
	})
	
	output$size = renderTable(size())
	
	
	
	
# Flush parameters --------------------------------------------------------	
	
	output$flush_plot.dur = renderPlot({
		dur_range = seq(input$flush.duration - input$flush.duration / 2, input$flush.duration + input$flush.duration / 2, length.out = 100)
		qplot(dur_range, flush_water(vol = input$flush.vol, flow_rate = conv_unit(input$flush.flow_rate, from = input$flush.flow_rate.unit, to = 'l_per_min'), duration = conv_unit(dur_range, from = input$flush.duration.unit, to = 'min')) * 100, geom = 'line') + geom_vline(xintercept = input$flush.duration, linetype = 'dashed') + labs(x = paste0('Duration (', names(which(dur_options == input$flush.duration.unit)), ')'), y = 'Water exchanged (%)', title = paste('Flow rate =', input$flush.flow_rate, names(which(flow_options == input$flush.flow_rate.unit))))
	})
	output$flush_plot.flow = renderPlot({
		FR_range = seq(input$flush.flow_rate - input$flush.flow_rate / 2, input$flush.flow_rate + input$flush.flow_rate / 2, length.out = 100)
		qplot(FR_range, flush_water(vol = input$flush.vol, flow_rate = conv_unit(FR_range, from = input$flush.flow_rate.unit, to = 'l_per_min'), duration = conv_unit(input$flush.duration, from = input$flush.duration.unit, to = 'min')) * 100, geom = 'line') + geom_vline(xintercept = input$flush.flow_rate, linetype = 'dashed') + labs(x = paste0('Flow rate (', names(which(flow_options == input$flush.flow_rate.unit)), ')'), y = 'Water exchanged (%)', title = paste('Duration =', input$flush.duration, names(which(dur_options == input$flush.duration.unit))))
	})
	output$flush_water.perc_fresh = renderText(round(flush_water(vol = input$flush.vol, flow_rate = conv_unit(input$flush.flow_rate, from = input$flush.flow_rate.unit, to = 'l_per_min'), duration = conv_unit(input$flush.duration, from = input$flush.duration.unit, to = 'min')) * 100, 1))
	
	
	
	output$flush_o2_plot.dur = renderPlot({
		dur_range = seq(input$flush.duration - input$flush.duration / 2, input$flush.duration + input$flush.duration / 2, length.out = 100)
		qplot(dur_range, conv_o2(flush_o2(resp_vol = input$flush.vol, flow_rate = conv_unit(input$flush.flow_rate, from = input$flush.flow_rate.unit, to = 'l_per_min'), duration = conv_unit(dur_range, from = input$flush.duration.unit, to = 'min'), resp_o2 = conv_o2(input$flush.o2_out, from = input$flush.o2_out.unit, to = 'percent_a.s.'), flush_o2 = conv_o2(input$flush.o2_in, from = input$flush.o2_in.unit, to = 'percent_a.s.')), from = 'percent_a.s.', to = input$flush.o2_out.unit), geom = 'line') + geom_vline(xintercept = input$flush.duration, linetype = 'dashed') + labs(x = paste0('Duration (', names(which(dur_options == input$flush.duration.unit)), ')'), y = paste0('Final respirometer O2 (', names(which(o2_options == input$flush.o2_out.unit)), ')'), title = paste('Flow rate =', input$flush.flow_rate, names(which(flow_options == input$flush.flow_rate.unit))))
	})
		
	output$flush_o2_plot.flow = renderPlot({
		FR_range = seq(input$flush.flow_rate - input$flush.flow_rate / 2, input$flush.flow_rate + input$flush.flow_rate / 2, length.out = 100)
		qplot(FR_range, conv_o2(flush_o2(resp_vol = input$flush.vol, flow_rate = conv_unit(FR_range, from = input$flush.flow_rate.unit, to = 'l_per_min'), duration = conv_unit(input$flush.duration, from = input$flush.duration.unit, to = 'min'), resp_o2 = conv_o2(input$flush.o2_out, from = input$flush.o2_out.unit, to = 'percent_a.s.'), flush_o2 = conv_o2(input$flush.o2_in, from = input$flush.o2_in.unit, to = 'percent_a.s.')), from = 'percent_a.s.', to = input$flush.o2_out.unit), geom = 'line') + geom_vline(xintercept = input$flush.flow_rate, linetype = 'dashed') + labs(x = paste0('Flow rate (', names(which(flow_options == input$flush.flow_rate.unit)), ')'), y = paste0('Final respirometer O2 (', names(which(o2_options == input$flush.o2_out.unit)), ')'), title = paste('Duration =', input$flush.duration, names(which(dur_options == input$flush.duration.unit))))
	})
	output$flush_water.final_o2 = renderText(round(conv_o2(flush_o2(resp_vol = input$flush.vol, flow_rate = conv_unit(input$flush.flow_rate, from = input$flush.flow_rate.unit, to = 'l_per_min'), duration = conv_unit(input$flush.duration, from = input$flush.duration.unit, to = 'min'), resp_o2 = conv_o2(input$flush.o2_out, from = input$flush.o2_out.unit, to = 'percent_a.s.'), flush_o2 = conv_o2(input$flush.o2_in, from = input$flush.o2_in.unit, to = 'percent_a.s.')), from = 'percent_a.s.', to = input$flush.o2_out.unit), 1))
	output$flush_water.o2_unit = renderText(names(which(o2_options == input$flush.o2_out.unit)))
	
	
}