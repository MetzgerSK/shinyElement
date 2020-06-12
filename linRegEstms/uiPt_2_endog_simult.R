tabsetPanel(   
    tabPanel("Main", value="main_endSim",     
        titlePanel("Simulation Setup: Simultaneity"),
                
        nObsBsTooltip("endSim"),
		canLoadBsTooltip("endSim"),
		
        sidebarLayout(
            sidebarPanel(
                copyBtn("endSim"),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    HTML("<p><strong><i class='fas fa-angle-double-right fa-lg'></i> <em>x</em>'s Equation</strong></p>"),
                    sliderInput("alpha2_endSim", label = HTML("<em>y</em>'s effect on <em>x</em>"), 
                                min = -0.95, max = 0.95, step = 0.05, value = -0.35),
                    sliderInput("aHatX_endSim", label = "Intercept \\(\\left(\\alpha_{x} \\right) \\)", 
                                min = -5, max = 5, step = 0.25, value = 0.5),
                    sliderInput("b2HatX_endSim", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_{2_x} \\right) \\)"), 
                                min = -5, max = 5, step = 0.25, value = -1.25),
                    disabled(sliderInput("noiseX_endSim", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma_{x} \\right) \\)"), 
                                         min = 1, max = 10, step = 1, value = 1)),
                    bsTooltip("noiseX_endSim", noise_endSim,
                                  placement="bottom", trigger="hover"),
                    
                    HTML("<p><strong><i class='fas fa-angle-double-right fa-lg'></i> Instruments</strong></p>"),
                    sliderInput("xInstr_endSim", label = HTML("For <em>x</em>'s Eq. \\(\\left(\\beta_{3_x} \\right) \\)"), 
                                min = -5, max = 5, step = 0.25, value = -0.5)
                ),  
                numericInput("seed_endSim", label = "Random Seed", value = 110116, min = 1),
                div(class="inline-block-center", div(class="loadSaved", 
                    checkboxInput("canLoad_endSim", label=loadSavedTxt, value=FALSE)
                )),
                br(),
                sliderInput("nObs_endSim", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 25, max = 500, step = 25, value = 50),
                sliderInput("sims_endSim", label = "# of Simulations", 
                            min = 5, max = 1000, step = 50, value = 1000),
                br(),
                
                HTML("<p><strong><i class='fas fa-angle-double-right fa-lg'></i> <em>y</em>'s Equation</strong></p>"),
                sliderInput("aHat_endSim" , label = "Intercept \\(\\left(\\alpha_{y} \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_endSim", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_{1_y} \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_endSim", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_{2_y} \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                disabled(sliderInput("noise_endSim", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma_{y} \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1)),
                bsTooltip("noise_endSim", noise_endSim,
                                  placement="bottom", trigger="hover"),
                br(),
                sliderInput("ltsAlpha_endSim", label = "LTS: \\(n\\) to Keep", 
                            min = 50, max = 100, step = 1, value = 50,
                            post="%"),
                bsTooltip("ltsAlpha_endSim", HTML("Percentage of observations whose uHat<sup>2</sup> will be minimized by least trimmed squares.")),  
                actionButton("goButton_endSim", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                # Load up the modal expl
                source("uiPt_2_endog_simult__popupDGP.R", local=TRUE)$value,
                actionButton("explain_simultDGP", "Why are there two equations?", icon("question-circle")),
                
                mainTabChunkBuild("endSim")
            )
        )
    ),
    tabPanel("Raw Simulation Output",
        rawSimBuild("endSim")     
    ),
    tabPanel("Estimates: Distribution Plots",
        estDistBuild("endSim")
    ),
    tabPanel("Efficiency Check",
        effChkBuild("endSim")
    )
)  