noise_endSim <- "Constrained to 1 to help simplify the data generation."

tabsetPanel(   
    
    tabPanel("Main", value="main_endSim",     
        titlePanel("Simulation Setup: Simultaneity"),
         
        nObsBsTooltip("endSim"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_endSim", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_endSim", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
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
                    #sliderInput("yInstr_endSim", label = HTML("For <em>y</em>'s Coeff \\(\\left(\\beta_{3_y} \\right) \\)"), 
                    #            min = -5, max = 5, step = 0.25, value = 2.25),
                    sliderInput("xInstr_endSim", label = HTML("For <em>x</em>'s Eq. \\(\\left(\\beta_{3_x} \\right) \\)"), 
                                min = -5, max = 5, step = 0.25, value = -0.5)
                ),  
                numericInput("seed_endSim", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_endSim", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_endSim", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 400),
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
                actionButton("goButton_endSim", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                actionButton("explain_simultDGP", "Why are there two equations?", icon("question-circle")),
                h4("True DGP:", align = "left"),
                uiOutput("equation_endSim"), 
     
                # Load up the modal expl
                source("uiPt_2_endog_simult__popupDGP.R", local=TRUE)$value,
                
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_endSim") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_endSim", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_endSim", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_endSim",   
        mainPanel(
            h4(class="simFyiHdr_endSim", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_endSim", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_endSim", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ),  
            DT::dataTableOutput("table_endSim") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_endSim" ,   
        sidebarPanel(
            selectInput("paramG_endSim", "Estimate",
                        c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_endSim", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_endSim") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_endSim" ,   
        sidebarPanel(
            selectInput("plotErrDist_endSim", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_endSim", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_endSim", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_endSim",
                            label = "Quantity for Horizontal Axis", 
                            choiceNames = c("\\(x \\)", "\\(z \\)", "\\(\\hat{y}\\)"),
                            choiceValues = c("x", "z", "yHat")
                        )
                    )
                ),
                column(4,
                    div()
                )
            ),
            div(class="inline-block-center",
                radioGroupButtons(
                    inputId = "residSel_endSim",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            
            uiOutput("distPlotErr_endSimUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_endSim" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_endSim", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_endSim", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_endSim") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ) ,
    tabPanel("What should I see?", value="expl_endSim",
        mainPanel(
            HTML("<h4>What happens to OLS estimates if <em>x</em> affects <em>y</em>'s value, but <em>y</em> affects <em>x</em>'s values?</h4>"),
            HTML("<p>OLS requires that a regression's covariates are exogenous to the process being studied.  Practically, this means
                <em>y</em> cannot affect <em>x</em>'s value.  If it does, we have a violation of exogeneity from simultaneity bias.
                 Our coefficient estimates will be biased.</p>"),
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            HTML("<p><strong>Biased:</strong> Top two rows won't match for all coefficients; top row won't
                                    fall in between values in 3rd and 4th rows for this column.</p>"),
            HTML("<p><strong>Efficient:</strong> 5th row's values should match/be close to No Violation's 5th row (provided
                                    all common slider/field values are identical).</p>") #,
        )
    )
)  