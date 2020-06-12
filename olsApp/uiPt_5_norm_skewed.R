tabsetPanel( 
    tabPanel("Main", value="main_skewed",     
        titlePanel("Simulation Setup: Skewed Errors"),
        
        nObsBsTooltip("skewed"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_skewed", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_skewed", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
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
                    sliderInput("snShape", label = "Skew-normal's shape parameter", 
                                min = -7, max = 7, step = .25, value = 6),
                    # For reference: shape parameter of VGAM::skewnormal() induces skew
                    div(class="inline-block-center",
                        div(
                            shinyjs::hidden(
                                div(id="snSh_skewed",
                                    p("0 = standard normal; >0, left-skew; <0, right-skew",
                                          style="font-size: 75%; font-style: italic; color: #777777;")
                                )
                            ),
                            div(id="chi2_skewed",
                                p("Always right skewed; higher d.f. = smaller variance",
                                      style="font-size: 75%; font-style: italic; color: #777777;")
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
                                margin = "5px"
    
                            )
                        )
                    )
                ),
                numericInput("seed_skewed", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_skewed", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_skewed", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_skewed", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_skewed", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_skewed", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_skewed", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                
                actionButton("goButton_skewed", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_skewed"),  
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_skewed") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_skewed", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    )
                ) 
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_skewed",   
         mainPanel(
            h4(class="simFyiHdr_skewed", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_skewed", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_skewed", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ), 
            DT::dataTableOutput("table_skewed") %>% withSpinner(., type = 6, color = "#33b5e5")
         )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_skewed" ,   
        sidebarPanel(
            selectInput("paramG_skewed", "Estimate",
                       c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_skewed", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_skewed") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_skewed" ,   
        sidebarPanel(
            selectInput("plotErrDist_skewed", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_skewed", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_skewed", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_skewed",
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
                    inputId = "residSel_skewed",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            
            uiOutput("distPlotErr_skewedUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_skewed" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_skewed", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_skewed", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_skewed") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ) ,
    tabPanel("What should I see?", value="expl_skewed" ,
        nonNormErrExpl
    )
)