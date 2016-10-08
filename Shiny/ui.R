library(shiny)

o2_options = list('% air saturation' = 'percent_a.s.', '% O2' = 'percent_o2', 'hPa', 'kPa', 'torr', 'mmHg', 'inHg', 'mg O2/L' = 'mg_per_l', 'umol O2/L' = 'umol_per_l', 'ml O2/L' = 'ml_per_l', 'mg O2/kg' = 'mg_per_kg', 'umol O2/kg' = 'umol_per_kg', 'ml O2/kg' = 'ml_per_kg')

fluidPage(
	titlePanel(title = paste('respirometry:', packageDescription(pkg = 'respirometry', fields = 'Title')), windowTitle = paste('respirometry: version', packageDescription(pkg = 'respirometry', fields = 'Version'))),
	navbarPage('',

tabPanel('Introduction',
	htmlOutput(outputId = 'intro')
),

tabPanel('General',

# conv_o2 -----------------------------------------------------------------

	titlePanel(title = HTML('Convert between O<sub>2</sub> units of partial pressure and concentration')),
	sidebarLayout(
		sidebarPanel(
			numericInput(inputId = 'conv_o2.o2', label = HTML('O<sub>2</sub> value'), value = 100),
			selectInput(inputId = 'conv_o2.from', label = 'Measurement unit', choices = o2_options),
			numericInput(inputId = 'conv_o2.temp', label = HTML('Temperature (&deg;C)'), value = 25),
			numericInput(inputId = 'conv_o2.sal', label = 'Salinity (psu)', value = 35),
			numericInput(inputId = 'conv_o2.atm_pres', label = 'Atmospheric pressure (mbar)', value = 1013.25)
		), mainPanel(
			tableOutput(outputId = 'conv_o2')
	)),

# correct_bubble ----------------------------------------------------------

	titlePanel(title = HTML('Adjust calculated respirometer volume due to bubble(s)')),
	sidebarLayout(
		sidebarPanel(
			numericInput(inputId = 'bubble.resp_vol', label = 'Respirometer volume (L)', value = 100),
			numericInput(inputId = 'bubble.bubble_vol', label = 'Bubble volume (mL)', value = 1),
			numericInput(inputId = 'bubble.temp', label = HTML('Temperature (&deg;C)'), value = 25),
			numericInput(inputId = 'bubble.sal', label = 'Salinity (psu)', value = 35),
			numericInput(inputId = 'bubble.atm_pres', label = 'Atmospheric pressure (mbar)', value = 1013.25)
		), mainPanel(
			htmlOutput(outputId = 'correct_bubble')
		))
),

tabPanel('Flushing and flow'),

tabPanel('Ammonium'),

tabPanel('Simulating OA')
))