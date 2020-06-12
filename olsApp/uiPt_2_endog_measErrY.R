tabsetPanel(     
    tabPanel("Main", value="main_endMeasErrY",     
        titlePanel("Simulation Setup: Measurement Error in \\( y \\)"),
         
        nObsBsTooltip("endMeasErrY"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_endMeasErrY", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_endMeasErrY", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("bonusNoise_endMeasErrY", label = "Amt. of Measurement Error", 
                                min = -10, max = 10, step = .5, value = -4)
                ),
                numericInput("seed_endMeasErrY", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_endMeasErrY", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_endMeasErrY", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_endMeasErrY", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_endMeasErrY", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_endMeasErrY", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_endMeasErrY", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                 
                actionButton("goButton_endMeasErrY", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_endMeasErrY"), 
                h4("Estimated Model:", align = "left"),
                uiOutput("equationEst_endMeasErrY"),  
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_endMeasErrY") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_endMeasErrY", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_endMeasErrY", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_endMeasErrY",   
        mainPanel(
            h4(class="simFyiHdr_endMeasErrY", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_endMeasErrY", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_endMeasErrY", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ),  
            DT::dataTableOutput("table_endMeasErrY") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_endMeasErrY" ,   
        sidebarPanel(
            selectInput("paramG_endMeasErrY", "Estimate",
                        c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_endMeasErrY", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_endMeasErrY") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_endMeasErrY" ,   
        sidebarPanel(
            selectInput("plotErrDist_endMeasErrY", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_endMeasErrY", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_endMeasErrY", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_endMeasErrY",
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
                    inputId = "residSel_endMeasErrY",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            uiOutput("distPlotErr_endMeasErrYUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_endMeasErrY" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_endMeasErrY", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_endMeasErrY", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_endMeasErrY") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ) ,
    tabPanel("What should I see?", value="expl_endMeasErrY",
        mainPanel(
            HTML("<h4>What happens to OLS estimates when there is measurement error in <em>y</em>?</h4>"),
            HTML("<p>If <em>y</em> is measured with error, we can rearrange the equation's terms such that <em>y</em>'s measurement
                 error will become a part of the equation's overall error term.  There are no violations to OLS's underlying assumptions about the true DGP.</p>"),
            HTML("<p>The OLS estimates should be <em>unbiased</em> and <em>efficient</em>.
                However, compared to a situation where <em>y</em> is measured without error,
                the <em>y</em>-with-error model's standard errors will be larger, as <em>y</em>'s
                mismeasurement is adding to the data's noisiness.</p>"),

            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            HTML("<p><strong>Unbiased:</strong> Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns.</p>"),
            HTML("<strong>Efficient:</strong> Mathematical in nature, because 'efficiency' technically refers to 'OLS's estimates will have the smallest possible SEs, compared to all other tools we could use.'
            For the larger standard errors relative to no measurement in error in <em>y</em>, the SE's average value (5th row) will be larger than the 'No Violations' scenario's 5th row.")
        )
    )
) 