tabsetPanel(     
    tabPanel("Main", value="main_endMeasErrX",     
        titlePanel("Simulation Setup: Measurement Error in \\( x \\)"),
         
        nObsBsTooltip("endMeasErrX"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_endMeasErrX", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_endMeasErrX", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("noiseX_endMeasErrX", label = "Amt. of Measurement Error (as proportion of \\(x\\)'s standard deviation)", 
                                min = 0, max = 3, step = .25, value = 1.5)
                ),
                numericInput("seed_endMeasErrX", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_endMeasErrX", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_endMeasErrX", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_endMeasErrX", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_endMeasErrX", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_endMeasErrX", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_endMeasErrX", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                 
                actionButton("goButton_endMeasErrX", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_endMeasErrX"),  
                h4("Estimated Model:", align = "left"),
                uiOutput("equationEst_endMeasErrX"),  
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_endMeasErrX") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_endMeasErrX", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_endMeasErrX", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_endMeasErrX",   
        mainPanel(
            h4(class="simFyiHdr_endMeasErrX", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_endMeasErrX", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_endMeasErrX", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ),  
            DT::dataTableOutput("table_endMeasErrX") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_endMeasErrX" ,   
        sidebarPanel(
            selectInput("paramG_endMeasErrX", "Estimate",
                        c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_endMeasErrX", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_endMeasErrX") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_endMeasErrX" ,   
        sidebarPanel(
            selectInput("plotErrDist_endMeasErrX", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_endMeasErrX", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
             fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_endMeasErrX", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_endMeasErrX",
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
                    inputId = "residSel_endMeasErrX",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            uiOutput("distPlotErr_endMeasErrXUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_endMeasErrX" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_endMeasErrX", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_endMeasErrX", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_endMeasErrX") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ),
    tabPanel("What should I see?", value="expl_endMeasErrX",
        mainPanel(
            HTML("<h4>What happens to OLS estimates when there is measurement error in <em>x</em>?</h4>"),
            HTML("<p>OLS assumes that all covariates are measured without error.  If <em>x</em> has measurement error, we can rearrange the equation to show
                 it will enter into the equation's overall error term as (<em>x</em>'s estimated slope * measurement error).  A covariate and <em>u</em>.
                 are now correlated--a violation of the exogeneity assumption.</p>"),
            HTML("<p>The OLS estimates will be <em>biased</em> but <em>efficient</em>.  This will
                 manifest in our estimate of <em>x</em>'s effect on <em>y</em> because <em>x</em>
                 is the covariate correlated with <em>u</em>, compliments of <em>x</em>'s mismeasurement.</p>"),
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            HTML("<p>
            <strong>Biased:</strong> Top two rows won't match for <em>x</em>'s coefficient; top row won't
                                     fall in between values in 3rd and 4th rows for this column.</p>"),
            HTML("<p>
            <strong>Efficient:</strong> 5th row's values should match/be close to No Violation's 5th row (provided
                        all common slider/field values are identical).</p>")#,
        )
    )
) 