tabsetPanel( 
    tabPanel("Main", value="main_ac",     
        titlePanel("Simulation Setup: Autocorrelation"),
        
        nObsBsTooltip("ac"),
        canLoadBsTooltip("ac"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("ac"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    shinyjs::hidden(div(class="inline-block-center",  # Doesn't disable as it should and can't see an obvious reason why, so just hide it
                        div(
                            shinyjs::disabled(
                              radioGroupButtons(
                               inputId = "acChoice",
                               label = "AC Type",
                               choiceNames = c("\\( AR \\)", "\\( MA \\)"),
                               choiceValues= c("ar", "ma"),
                               selected="ar"
                            )) ,
                            bsTooltip("acChoice", "Restricted to AR for speed purposes", placement="right", trigger="hover")
                        )
                    )),
                    sliderInput("arVal", label = "Autocorrelation Value \\(\\left[AR(1) \\right] \\)", min = -.9, max = .9, step = 0.1, value = 0.9),
                    div(class="inline-block-center",
                            div(
                                p("Restricted to AR only for speed purposes.",
                                  class="descTxt")
                            )
                    )
                ),
                numericInput("seed_ac", label = "Random Seed", value = 110116, min = 1),
				div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_ac", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_ac", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_ac", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_ac", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_ac", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_ac", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_ac", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                br(),
                sliderInput("ltsAlpha_ac", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_ac", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                
                actionButton("goButton_ac", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                mainTabChunkBuild("ac")
            )
        )
    ),
    tabPanel("Raw Simulation Output", 
        rawSimBuild("ac")
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("ac")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("ac")
    )
)