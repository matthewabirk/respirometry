library(shiny)

h4_lined = function(...){
HTML(paste0('<hr/><h4 align="center">', ..., '</h4><hr/>'))
}

fluidPage(
	titlePanel(title = paste('respirometry:', packageDescription(pkg = 'respirometry', fields = 'Title')), windowTitle = paste('respirometry: version', packageVersion(pkg = 'respirometry'))),
	navbarPage('',

tabPanel('Introduction',
	
	p(gsub('\n\\s+', ' ', packageDescription(pkg = 'respirometry', fields = 'Description'))),
	
	p('This software runs off the R package "respirometry" which can be accessed at', a(href = 'http://cran.r-project.org/package=respirometry', 'cran.r-project.org/package=respirometry'), '. For issues or suggestions, contact Matthew A. Birk at matthewabirk@gmail.com.'),
	
	p('This software can be referenced as:', br(), strong('Birk, Matthew A.'), em('respirometry:', packageDescription(pkg = 'respirometry', fields = 'Title')), '(version', packageDescription(pkg = 'respirometry', fields = 'Version'), ').'),
	
	img(height = 140, width = 160, src = 'Rlogo.png')
),

tabPanel('General',

# conv_o2 -----------------------------------------------------------------

	h1('Convert between O', tags$sub(2), 'units of partial pressure and concentration'),
	sidebarLayout(
		sidebarPanel(
			numericInput('conv_o2.o2', label = HTML('O<sub>2</sub> value'), value = 100),
			selectInput('conv_o2.from', label = 'Measurement unit', choices = o2_options),
			numericInput('conv_o2.temp', label = HTML('Temperature (&deg;C)'), value = 25),
			numericInput('conv_o2.sal', label = 'Salinity (psu)', value = 35),
			numericInput('conv_o2.atm_pres', label = 'Atmospheric pressure (mbar)', value = 1013.25),
			h4_lined('OR'),
			fileInput('conv_o2.excel_file', label = HTML('Upload an Excel file containing O<sub>2</sub> values')),
			fileInput('conv_o2.txt_file', label = HTML('Upload a text file containing O<sub>2</sub> values'))
		), mainPanel(
			tableOutput('conv_o2')
	)),
	hr(),

# Q10 ---------------------------------------------------------------------

	h1('Calculate the Q', tags$sub(10), 'temperature coefficient'),
	sidebarLayout(
		sidebarPanel(
			numericInput('Q10.Q10', label = HTML('Q<sub>10</sub> value'), value = NA),
			numericInput('Q10.R1', label = 'Initial rate', value = NA),
			numericInput('Q10.R2', label = 'Second rate', value = NA),
			numericInput('Q10.T1', label = HTML('Initial temperature (&deg;C)'), value = NA),
			numericInput('Q10.T2', label = HTML('Second temperature (&deg;C)'), value = NA)
		), mainPanel(
			
		)
	),
	hr(),

# correct_bubble ----------------------------------------------------------

	h1('Adjust calculated respirometer volume due to bubble(s)'),
	p('Caution: allowing air bubbles into a respirometer is not recommended, even with this post-measurement adjustment. A small error in bubble volume estimation can lead to a large error in calculated metabolic rate.'),
	sidebarLayout(
		sidebarPanel(
			numericInput('bubble.resp_vol', label = 'Respirometer volume (L)', value = 100),
			numericInput('bubble.bubble_vol', label = 'Bubble volume (mL)', value = 1),
			numericInput('bubble.temp', label = HTML('Temperature (&deg;C)'), value = 25),
			numericInput('bubble.sal', label = 'Salinity (psu)', value = 35),
			numericInput('bubble.atm_pres', label = 'Atmospheric pressure (mbar)', value = 1013.25)
		), mainPanel(
			p('Original volume:', textOutput('correct_bubble.orig_vol', inline = TRUE), 'L', br(), 'Bubble volume equivalent:', textOutput('correct_bubble.vol_equiv', inline = TRUE), 'L', br(), 'Adjusted respirometer volume:', textOutput('correct_bubble.final_vol', inline = TRUE), 'L')
		)
	)
),

tabPanel('Flushing and flow',


# Flush -------------------------------------------------------------------

	h1('Flushing the respirometer'),
	sidebarLayout(
		sidebarPanel(
			h4_lined('Water flow'),
			numericInput('flush.vol', label = 'Respirometer volume (L)', value = 1),
			fluidRow(
				column(8, numericInput('flush.flow_rate', label = 'Flow rate', value = 1)),
				column(4, selectInput('flush.flow_rate.unit', label = 'Unit', choices = flow_options))
			),
			fluidRow(
				column(8, numericInput('flush.duration', label = 'Flush duration', value = 1)),
				column(4, selectInput('flush.duration.unit', label = 'Unit', selected = 'min', choices = dur_options))
			),
			h4_lined('Oxygen'),
			fluidRow(
				column(6, numericInput('flush.o2_out', label = 'Respirometer water O2', value = 80)),
				column(6, selectInput('flush.o2_out.unit', label = 'Unit', choices = o2_options))
			),
			fluidRow(
				column(6, numericInput('flush.o2_in', label = 'Flush water O2', value = 100)),
				column(6, selectInput('flush.o2_in.unit', label = 'Unit', choices = o2_options))
			),
			h4_lined('pH'),
			numericInput('flush.ph_out', label = 'Respirometer water pH (total scale)', value = 7.9),
			numericInput('flush.ph_in', label = 'Flush water pH (total scale)', value = 8.1),
			numericInput('flush.temp', label = HTML('Temperature (&deg;C)'), value = 25),
			numericInput('flush.sal', label = 'Salinity (psu)', value = 35),
			numericInput('flush.ta', label = HTML('Seawater alkalinity (if known) (&micro;mol/kg)'), value = 2300),
			numericInput('flush.atm_pres', label = 'Atmospheric pressure (mbar)', value = 1013.25)
		), mainPanel(
			h4_lined('Water flow'),
			h5(textOutput('flush_water.perc_fresh', inline = TRUE), '% exchange of water volume', align = 'center', style = 'border-style: double'),
			splitLayout(
				plotOutput('flush_plot.dur'),
				plotOutput('flush_plot.flow')
			),
			h4_lined('Oyxgen'),
			h5('New respirometer O2 content:', textOutput('flush_water.final_o2', inline = TRUE), textOutput('flush_water.o2_unit', inline = TRUE), align = 'center', style = 'border-style: double'),
			splitLayout(
				plotOutput('flush_o2_plot.dur'),
				plotOutput('flush_o2_plot.flow')
			),
			h4_lined('pH')
		)
	)
),

tabPanel('Ammonium',

# conv_NH4 -----------------------------------------------------------------

	h1('Convert between units of ammonium'),
	sidebarLayout(
		sidebarPanel(
			numericInput('conv_NH4.n_waste', label = 'Nitrogen value', value = 1),
			selectInput('conv_NH4.from', label = 'Measurement unit', choices = nh4_options)
		), mainPanel(
			tableOutput('conv_NH4')
		)
	)
),

tabPanel('Simulating OA')
))