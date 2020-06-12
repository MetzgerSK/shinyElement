tabsetPanel( 
    tabPanel("Main", value="main_ar",      
        titlePanel("Simulation Setup: \\( E \\left( u \\right) \\neq 0 \\)"),
                
        nObsBsTooltip("ar"),
		canLoadBsTooltip("ar"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("ar"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("meanU", label = HTML("Mean of <em>u</em>"), 
                                min = 0, max = 10, step = 0.5, value = 5)
                ),
                numericInput("seed_ar", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_ar", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_ar", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_ar", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_ar", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_ar", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_ar", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_ar", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_ar", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_ar", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_ar", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("ar")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("ar")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("ar")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("ar")
    )
    
)