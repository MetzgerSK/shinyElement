tabsetPanel( 
    tabPanel("Main", value="main_heavy",     
        titlePanel("Simulation Setup: Heavy-Tailed Errors"),
        
        nObsBsTooltip("heavy"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_heavy", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_heavy", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    
                    sliderInput("tDFVal", label = "\\( t \\)'s degrees of freedom ", min = 3, max = 35, step = 1, value = 4),
                    div(class="inline-block-center",
                        div(
                            p("Lower d.f. = heavier tailed.",
                              style="font-size: 75%; font-style: italic; color: #777777;")
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
                br(),
                sliderInput("nObs_heavy", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_heavy", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_heavy", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_heavy", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_heavy", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_heavy", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                
                actionButton("goButton_heavy", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_heavy"),  
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_heavy") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_heavy", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_heavy", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_heavy",   
         mainPanel(
            h4(class="simFyiHdr_heavy", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
                div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_heavy", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_heavy", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ), 
            DT::dataTableOutput("table_heavy") %>% withSpinner(., type = 6, color = "#33b5e5")
         )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_heavy" ,   
        sidebarPanel(
            selectInput("paramG_heavy", "Estimate",
                       c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_heavy", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_heavy") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_heavy" ,   
        sidebarPanel(
            selectInput("plotErrDist_heavy", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_heavy", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_heavy", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_heavy",
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
                    inputId = "residSel_heavy",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            
            uiOutput("distPlotErr_heavyUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_heavy" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_heavy", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_heavy", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_heavy") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ) ,
    tabPanel("What should I see?", value="expl_heavy" ,
        nonNormErrExpl
    )
)