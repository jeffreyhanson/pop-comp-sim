shinyUI(fluidPage(
	sidebarLayout(
		sidebarPanel(
			wellPanel(
				h5("Universal Parameters"),
				numericInput("r0", "Initial resource amount", 1500),
				numericInput("E", "Amount of resources used per cell per hour", 400),
				numericInput("K", "Amount of resources when growth rate half of maximum", 1500),
				numericInput("timestep", "Length of each timestep (hours)", 1/60),
				numericInput("totaltime", "Total length of time to be simulated", 24)
			),
			wellPanel(
				h5("Strain 1 Parameters"),
				numericInput("N1", "Initial population size", 100000),
				numericInput("V1", "Maximum growth rate (per hour)", 1.6),
				numericInput("d1", "Death rate (relative to growth rate per hour)", -0.005)
			),
			wellPanel(
				h5("Strain 2 Parameters"),
				numericInput("N2", "Initial population size", 100000),
				numericInput("V2", "Maximum growth rate (per hour)", 1.6),
				numericInput("d2", "Death rate (relative to growth rate per hour)", -0.01)
			)
		),
		mainPanel(
			plotOutput("mainPlot")
		)
	)
))
