library(shiny)
library(respirometry)
o2_units = data.frame(code = names(conv_o2()), pretty = c('% air saturation', '% O2', 'hPa', 'kPa', 'torr', 'mmHg', 'inHg', 'mg O2/L', 'umol O2/L', 'ml O2/L', 'mg O2/kg', 'umol O2/kg', 'ml O2/kg'))

function(input, output){
	output$intro = renderUI(HTML(paste(gsub('\n\\s+', ' ', packageDescription(pkg = 'respirometry', fields = 'Description')), 'This software runs off the R package "respirometry" which can be accessed at http://cran.r-project.org/package=respirometry.', 'For issues or suggestions, contact Matthew A. Birk at matthewabirk@gmail.com. This software can be referenced as:', paste0('Birk, Matthew A. respirometry: ', packageDescription(pkg = 'respirometry', fields = 'Title'), ' (version ', packageDescription(pkg = 'respirometry', fields = 'Version'), ').'), sep = '<br><br>')))

# conv_o2 -----------------------------------------------------------------

	output$conv_o2 = renderTable({
  	x = do.call(rbind.data.frame, conv_o2(o2 = input$conv_o2.o2, from = input$conv_o2.from, temp = input$conv_o2.temp, sal = input$conv_o2.sal, atm_pres = input$conv_o2.atm_pres))
  	colnames(x) = 'Value'
  	x$Unit = o2_units[which(rownames(x) == o2_units$code), 'pretty']
  	rownames(x) = NULL
  	x = x[, c('Unit', 'Value')]
  }, include.rownames = FALSE)

# correct_bubble ----------------------------------------------------------
	
	output$correct_bubble = renderUI({
		x = round(correct_bubble(resp_vol = input$bubble.resp_vol, bubble_vol = input$bubble.bubble_vol, temp = input$bubble.temp, sal = input$bubble.sal, atm_pres = input$bubble.atm_pres), 2)
		HTML(paste('Original volume:', input$bubble.resp_vol, 'L<br>',
			'Bubble volume equivalent:', round(x - input$bubble.resp_vol, 2), 'L<br>',
			'Adjusted respirometer volume:', x, 'L'
		))
	})
	
	
	
}