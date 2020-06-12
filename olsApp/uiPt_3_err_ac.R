tabsetPanel( 
    tabPanel("Main", value="main_ac",     
        titlePanel("Simulation Setup: Autocorrelation"),
        
        nObsBsTooltip("ac"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_ac", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_ac", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    div(class="inline-block-center",
                        div(
                            radioGroupButtons(
                               inputId = "acChoice",
                               label = "AC Type", 
                               choiceNames = c("\\( AR \\)", "\\( MA \\)"),
                               choiceValues= c("ar", "ma"),
                               selected="ar",
                               status="primary"
                            )
                        )
                    ),
                    sliderInput("arVal", label = "Autocorrelation Value \\(\\left[AR(1) \\right] \\)", 
                                min = -.9, max = .9, step = 0.1, value = 0.9)
                ),
                numericInput("seed_ac", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_ac", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_ac", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_ac", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_ac", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_ac", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_ac", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                
                actionButton("goButton_ac", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_ac"),  
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_ac") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_ac", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_ac", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_ac",   
        mainPanel(
            h4(class="simFyiHdr_ac", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_ac", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_ac", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ), 
            DT::dataTableOutput("table_ac") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_ac" ,   
        sidebarPanel(
            selectInput("paramG_ac", "Estimate",
                       c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_ac", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_ac") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_ac" ,   
        sidebarPanel(
            selectInput("plotErrDist_ac", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_ac", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_ac", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_ac",
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
                    inputId = "residSel_ac",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            uiOutput("distPlotErr_acUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_ac" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_ac", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_ac", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_ac") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ),
    tabPanel("What should I see?", value="expl_ac" ,   
        mainPanel(
            h4("What happens to OLS estimates when the error's autocorrelated?"),
            p("OLS assumes that the random error is random.  That means there's no way for us to predict the error's values.  
                If errors exhibit autocorrelation, it means that we can use one observation's error value to predict the value 
                of another observation's error."),
            p("The OLS estimates will be ", em("unbiased"), "but ", em("inefficient."),"  This will manifest in our SE 
                estimates for all variables."),
            
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns."),
            p(strong("Inefficient:"), "5th row's values won't match No Violations' 5th row (provided all common slider/field values are identical).")
        )
    )
)