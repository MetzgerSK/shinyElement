tabsetPanel( 
    tabPanel("Main", value="main_skewed",     
        titlePanel("Simulation Setup: Skewed Errors"),
                
        nObsBsTooltip("skewed"),
		canLoadBsTooltip("skewed"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("skewed"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    div(class="inline-block-center",
                        div(
                            radioGroupButtons(
                               inputId = "distChoice",
                               label = "Error's Distribution",
                               choiceNames = c("\\( \\chi^2 \\)", "\\( \\mathcal{N}_{\\text{skew}}\\)"),
                               choiceValues= c("chi2", "sknorm"),
                               selected="chi2",
                               status="primary"
                            )
                        ) 
                    ),
                    sliderInput("snShape", label = "Skew-normal's shape parameter", min = -7, max = 7, step = .25, value = 6),
                    # NOTE: Slider value updater located in "serverPt_dgps.R"
                    
                    div(class="inline-block-center",
                        div(
                            shinyjs::hidden(
                                div(id="snSh_skewed",
                                    p("0 = standard normal; >0, left-skew; <0, right-skew",
                                          class="descTxt")
                                )
                            ),
                            div(id="chi2_skewed",
                                p("Always right skewed; higher d.f. = smaller variance",
                                      class="descTxt")
                            )
                        ),
                    
                        div(style=".fa{padding-right: 0px !important;}",
                            dropdownButton(
                                plotOutput("errVsNorm_skewed"),
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
                numericInput("seed_skewed", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_skewed", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_skewed", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_skewed", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_skewed", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_skewed", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_skewed", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_skewed", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_skewed", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_skewed", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_skewed", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("skewed")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("skewed")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("skewed")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("skewed")
    )
)