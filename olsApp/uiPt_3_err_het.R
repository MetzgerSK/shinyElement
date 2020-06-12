tabsetPanel( 
    tabPanel("Main", value="main_hetero",     
        titlePanel("Simulation Setup: Heteroscedasticity"),
        
        nObsBsTooltip("hetero"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_hetero", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_hetero", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                numericInput("seed_hetero", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_hetero", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_hetero", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_hetero", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_hetero", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_hetero", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_hetero", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                
                actionButton("goButton_hetero", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_hetero"), 
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_hetero") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_hetero", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_hetero", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_hetero",   
         mainPanel(
            h4(class="simFyiHdr_hetero", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_hetero", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_hetero", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ), 
            DT::dataTableOutput("table_hetero") %>% withSpinner(., type = 6, color = "#33b5e5")
         )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_hetero" ,   
        sidebarPanel(
            selectInput("paramG_hetero", "Estimate",
                       c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_hetero", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_hetero") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_hetero" ,   
        sidebarPanel(
            selectInput("plotErrDist_hetero", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_hetero", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_hetero", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_hetero",
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
                    inputId = "residSel_hetero",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            uiOutput("distPlotErr_heteroUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_hetero" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_hetero", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_hetero", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_hetero") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ),
    tabPanel("What should I see?", value="expl_hetero" ,   
        mainPanel(
            h4("What happens to OLS estimates when the error's heteroscedastic?"),
            p("OLS assumes that the random error is random.  That means there's no way for us to predict the error's values.  
                If errors are heteroscedastic, it means that their dispersion (how tightly they cluster around the regression line) 
                changes as a function of a covariate's values."),
            HTML("<p>The OLS estimates will be <em>unbiased</em> but <em>inefficient</em>.  This will manifest in our SE estimate of 
                the variable with which the errors are heteroscedastic (here, <em>x</em>), since we could use information about 
                <em>x</em>'s values to tell us something about the (not-so-)random errors' values.</p>"),
         
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns."),
            p(strong("Inefficient:"), HTML("For <em>x</em>, 5th row's values won't match No Violations' 5th row (provided all common slider/field values are identical)."))
        )
    )
                )