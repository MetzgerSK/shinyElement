tabsetPanel( 
    tabPanel("Main", value="main_ar",      
        titlePanel("Simulation Setup: \\( E \\left( u \\right) \\neq 0 \\)"),
        
        nObsBsTooltip("ar"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_ar", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_ar", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("meanU", label = HTML("Mean of <em>u</em>"), 
                                min = 0, max = 10, step = 0.5, value = 5)
                ),
                numericInput("seed_ar", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_ar", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_ar", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_ar", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_ar", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_ar", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_ar", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                
                actionButton("goButton_ar", "Simulate!", icon("rocket"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_ar"), 
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_ar") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                shinyjs::hidden(
                    downloadButton("datDwn_ar", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_ar", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_ar",   
        mainPanel(
            h4(class="simFyiHdr_ar", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_ar", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_ar", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ), 
            DT::dataTableOutput("table_ar") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_ar" ,   
        sidebarPanel(
            selectInput("paramG_ar", "Estimate",
                       c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_ar", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_ar") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_ar" ,   
        sidebarPanel(
            selectInput("plotErrDist_ar", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_ar", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_ar", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_ar",
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
                    inputId = "residSel_ar",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            uiOutput("distPlotErr_arUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_ar" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_ar", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_ar", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_ar") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ),
    tabPanel("What should I see?", value="expl_ar" ,   
        mainPanel(
            h4("What happens to OLS estimates if the error doesn't average to zero?"),
            p("The random error will sometimes be high in value and sometimes be low in value, but overall, 
            these high and lows should cancel each other out.  Otherwise, there's something systematically 
            affecting the error's value.  (Which makes the random error not random.)  For this reason, OLS 
            assumes that the random error averages to zero.  If it doesn't, we've violated this assumption."),
            p("The OLS estimates will be ", em("biased"), "but ", em("efficient."),"  This will manifest in our 
                estimate of the intercept, since a non-zero error would appear as another constant term in the DGP."),
            
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Biased:"), "Top two rows won't match for intercept; top row won't fall in between values in 3rd and 4th rows for this column."),
            p(strong("Efficient:"), "5th row's values should match/be close to No Violations' 5th row (provided all common slider/field values are identical)."),
            br(),br(),
            p(techSideNoteEu0)
        )
    )
)