tabsetPanel( 
    tabPanel("Main", value="main_heavy",     
        titlePanel("Simulation Setup: Heavy-Tailed Errors"),
                
        nObsBsTooltip("heavy"),
		canLoadBsTooltip("heavy"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("heavy"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    
                    sliderInput("tDFVal", label = "\\( t \\)'s degrees of freedom ", 
                                min = 3, max = 35, step = 1, value = 4),
                    div(class="inline-block-center",
                        div(
                            p("Lower d.f. = heavier tailed.",
                              class="descTxt")
                        ),
                        div(style=".fa{padding-right: 0px !important;}",
                                dropdownButton(
                                    plotOutput("errVsNorm_heavy"),
                                    circle = TRUE,
                                    status = "info",
                                    size = "sm",
                                    icon = "?",
                                    label = "View selected distribution vs. normal",
                                    tooltip = TRUE,
                                    right = FALSE,
                                    width="400px",
                                    margin = "10px"
    
                                )
                        )
                    )
                ),
                numericInput("seed_heavy", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_heavy", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_heavy", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_heavy", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_heavy", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_heavy", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_heavy", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_heavy", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_heavy", label = "LTS: \\(n\\) to Keep", min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_heavy", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_heavy", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("heavy")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("heavy")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("heavy")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("heavy")
    )
)