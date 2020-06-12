tabsetPanel(     
    tabPanel("Main", value="main_endMeasErrY",     
        titlePanel("Simulation Setup: Measurement Error in \\( y \\)"),
               
        nObsBsTooltip("endMeasErrY"),
		canLoadBsTooltip("endMeasErrY"),
		
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_endMeasErrY", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_endMeasErrY", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("bonusNoise_endMeasErrY", label = "Amt. of Measurement Error", 
                                min = -10, max = 10, step = .5, value = -4)
                ),
                numericInput("seed_endMeasErrY", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_endMeasErrY", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_endMeasErrY", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_endMeasErrY", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_endMeasErrY", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_endMeasErrY", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_endMeasErrY", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_endMeasErrY", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                                br(),
                sliderInput("ltsAlpha_endMeasErrY", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_endMeasErrY", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_endMeasErrY", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("endMeasErrY")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("endMeasErrY")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("endMeasErrY")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("endMeasErrY")
    )
) 