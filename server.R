
shinyServer(function(input, output, session) {
	output$mainPlot=renderPlot({
		makePlot(
			input$r0,
			input$E,
			input$K,
			input$timestep,
			input$totaltime,
			input$N1,
			input$V1,
			input$d1 * input$V1,
			input$N2,
			input$V2,
			input$d2 * input$V2
		)
	})
})