tabsetPanel( 
    tabPanel("Main", value="main_hetero",     
        titlePanel("Simulation Setup: Heteroscedasticity"),
                        
        nObsBsTooltip("hetero"),
        canLoadBsTooltip("hetero"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("hetero"),
                numericInput("seed_hetero", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_hetero", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_hetero", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_hetero", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_hetero", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_hetero", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_hetero", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_hetero", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_hetero", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_hetero", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_hetero", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("hetero")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("hetero")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("hetero")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("hetero")
    )
  
)