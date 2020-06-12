tabsetPanel( 
    tabPanel("Main", value="main_nlin",       
        titlePanel("Simulation Setup: Non-Linear in Parameters"),
		        
        nObsBsTooltip("nlin"),
		canLoadBsTooltip("nlin"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("nlin"),
                numericInput("seed_nlin", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_nlin", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_nlin", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_nlin", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_nlin", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_nlin", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_nlin", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_nlin", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_nlin", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_nlin", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_nlin", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("nlin")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("nlin")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("nlin")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("nlin")
    )
)