tabsetPanel(     
    tabPanel("Main", value="main_end",     
        titlePanel("Simulation Setup: Omitted Variable"),
                
        nObsBsTooltip("end"),
		canLoadBsTooltip("end"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("end"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("corrXZ", label = HTML("Correlation between <em>x</em> and <em>z</em> \\(\\left( \\text{Corr} \\left( x,z \\right) \\right) \\) "),
                                min = -1, max = 1, step = 0.1, value = 0.5)
                ) ,
                numericInput("seed_end", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_end", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_end", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_end", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_end", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_end", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_end", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_end", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_end", label = "LTS: \\(n\\) to Keep", min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_end", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")), 
                 
                actionButton("goButton_end", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                bsCallout("Multicollinearity",
                          "Can also demonstrate multicollinearity's effect.  
                           Set \\\\(| \\\\text{Corr} \\\\left( x,z \\\\right) | \\\\) to 0.85&ndash;0.99."
                ),
                bsCallout("No Correlation with <em>x</em>",
                          "Can also demonstrate effect of omitting a relevant variable, but one uncorrelated with <em>x</em>.  
                           Set \\\\( \\\\text{Corr} \\\\left( x,z \\\\right) \\\\) to 0."
                ), # need quadruples to escape the intermediate paste
                mainTabChunkBuild("end")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("end")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("end")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("end")
    )
)