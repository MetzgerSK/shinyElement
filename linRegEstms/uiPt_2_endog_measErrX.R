tabsetPanel(     
    tabPanel("Main", value="main_endMeasErrX",     
        titlePanel("Simulation Setup: Measurement Error in \\( x \\)"),
                
        nObsBsTooltip("endMeasErrX"), 
		canLoadBsTooltip("endMeasErrX"),
         
        sidebarLayout(
            sidebarPanel(
                copyBtn("endMeasErrX"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("noiseX_endMeasErrX", label = "Amt. of Measurement Error (as proportion of \\(x\\)'s standard deviation)", 
                                min = 0, max = 3, step = .25, value = 1.5)
                ),
                numericInput("seed_endMeasErrX", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_endMeasErrX", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_endMeasErrX", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_endMeasErrX", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_endMeasErrX", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_endMeasErrX", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_endMeasErrX", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_endMeasErrX", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_endMeasErrX", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_endMeasErrX", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_endMeasErrX", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("endMeasErrX")
            )
        )
    ),
    tabPanel("Raw Simulation Output", 
        rawSimBuild("endMeasErrX")
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("endMeasErrX")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("endMeasErrX")
    )
) 