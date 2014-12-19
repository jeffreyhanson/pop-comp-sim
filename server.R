
shinyServer(function(input, output, session) {
	output$mainPlot=renderPlot({
		par(mfrow=c(1,3))
		
		plot1(
			input$r0,
			input$E,
			input$timestep,
			input$totaltime,
			input$N1,
			input$V1,
			input$d1,
			input$N2,
			input$V2,
			input$d2
		)
		
		plot2(
			input$r0,
			input$E,
			input$timestep,
			input$totaltime,
			input$N1,
			input$V1,
			input$d1,
			input$N2,
			input$V2,
			input$d2
		)
		
		
		
	})
})