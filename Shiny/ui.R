library(shiny)
fluidPage(
	titlePanel(title = 'respirometry: Tools for Conducting Respirometry Experiments', windowTitle = 'v 0.1.0'),
	navbarPage('',

tabPanel('Introduction',
	htmlOutput(outputId = 'O_intro')
),

tabPanel('General',
	titlePanel(title = 'Convert between O2 units of partial pressure and concentration'),
	numericInput(inputId = 'I_o2', label = 'O2 value', value = 100),
	selectInput(inputId = 'I_from', label = 'Measurement unit', choices = c('% air saturation' = 'percent_a.s.', '% O2' = 'percent_o2', 'hPa', 'kPa', 'torr', 'mmHg', 'inHg', 'mg O2 / l' = 'mg_per_l', 'umol O2 / l' = 'umol_per_l', 'ml O2 / l' = 'ml_per_l', 'mg O2 / kg' = 'mg_per_kg', 'umol O2 / kg' = 'umol_per_kg', 'ml O2 / kg' = 'ml_per_kg')),
	sliderInput(inputId = 'i1', label = 'how many?', min = 1, max = 50, value = 30),
	plotOutput(outputId = 'o1')
),

tabPanel('Flushing and flow'),

tabPanel('Ammonium'),

tabPanel('Simulating OA')
))