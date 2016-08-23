library(shiny)
function(input, output){
	output$O_intro = renderUI(HTML(paste(gsub('\n\\s+', ' ', packageDescription(pkg = 'respirometry', fields = 'Description')), 'This software runs off the R package "respirometry" which can be accessed at http://cran.r-project.org/package=respirometry.', 'For issues or suggestions, contact Matthew A. Birk at matthewabirk@gmail.com. This software can be referenced as:', paste0('Birk, Matthew A. respirometry: ', packageDescription(pkg = 'respirometry', fields = 'Title'), ' (version ', packageDescription(pkg = 'respirometry', fields = 'Version'), ').'), sep = '<br/><br/>')))
	
	output$o1 = renderPlot({
    x = faithful[, 2]
    bins = seq(min(x), max(x), length.out = input$i1 + 1)
    hist(x, breaks = bins, col = 'blue', border = 'yellow')
  })
}