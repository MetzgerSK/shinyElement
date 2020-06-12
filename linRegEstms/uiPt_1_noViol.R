tabsetPanel(     
    tabPanel("Main", value="main_true",  
        titlePanel("Simulation Setup: No Violations"),
                
        nObsBsTooltip(""),
        canLoadBsTooltip(""),
		
        sidebarLayout(
            sidebarPanel(
                numericInput("seed", label = "Random Seed", value = 110116, min = 1),
                div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 5, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                actionButton("randPars", "Shuffle", icon("random"), 
                             style="color: #999; background-color: #333"),
                bsTooltip("randPars", "Pick a different set of arbitrary parameter values",
                          placement="bottom", trigger="hover"),
                actionButton("reset", "Reset", icon("repeat"), 
                             style="color: #000; background-color: #999"),
                bsTooltip("reset", "Reset back to default parameter values",
                          placement="bottom", trigger="hover"),
                br(),br(),
                actionButton("goButton", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                bsCallout('No Correlation with <em>x</em> or <em>y</em>',
                          'Can also demonstrate effect of including an irrelevant variable.  
                          Set \\\\( \\\\beta_2 \\\\) to 0 (\\\\( \\\\text{Corr}(x,z) = 0 \\\\) 
                          already for this scenario).' # need quadruples to escape the intermediate paste
                ),
                mainTabChunkBuild("reg")
            )
        )
    ),
    tabPanel("Raw Simulation Output", 
        rawSimBuild("reg")
    ),
    tabPanel("Estimates: Distribution Plots", 
        estDistBuild("reg")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("reg")
    )

)