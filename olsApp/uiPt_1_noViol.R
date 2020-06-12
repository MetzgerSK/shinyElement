tabsetPanel(     
    tabPanel("Main", value="main_true",  
        titlePanel("Simulation Setup: No Violations"),
        
        nObsBsTooltip(""),
        
        sidebarLayout(
            sidebarPanel(
                numericInput("seed", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                
                actionButton("randPars", "Shuffle", icon("random"), 
                             style="color: #999; background-color: #333"),
                bsTooltip("randPars", "Pick a different set of arbitrary parameter values",
                          placement="bottom", trigger="hover"),
                actionButton("reset", "Reset", icon("repeat"), 
                             style="color: #000; background-color: #999"),
                bsTooltip("reset", "Reset back to default parameter values",
                          placement="bottom", trigger="hover"),
                br(),br(),
                actionButton("goButton", "Simulate!", icon("rocket"), 
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                div(class="bs-callout bs-callout-info",
                    div(class="bs-close", icon("times-circle")),
                    HTML("<h4>No Correlation with <em>x</em> or <em>y</em></h4>"),
                    HTML("<strong>Can also demonstrate effect of including an irrelevant variable.  
                          Set \\( \\beta_2 \\) to 0 (\\( \\text{Corr}(x,z) = 0 \\) already for this scenario).</strong>")
                ),
                h4("True DGP:", align = "left"),
                uiOutput("equation"),
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_reg") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_reg", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_reg", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_true",   
         mainPanel(
            h4(class="simFyiHdr_reg", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_reg", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_reg", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ),
            DT::dataTableOutput("table_reg") %>% withSpinner(., type = 6, color = "#33b5e5")
         )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_true",   
        sidebarPanel(
            selectInput("paramG_true", "Estimate",
                        c("aHat", "b1Hat", "b2Hat")) 
        ),
        mainPanel(
           h4(class="simFyiHdr_reg", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
           plotOutput("distPlot_true") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_reg" ,   
        sidebarPanel(
            selectInput("plotErrDist_reg", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_reg", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_reg", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_reg",
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
                    inputId = "residSel_reg",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            
            uiOutput("distPlotErr_regUI")  %>% withSpinner(., type = 6, color = "#33b5e5") 
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_reg" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_reg", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_reg", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_reg") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ),
    tabPanel("What should I see?", value="expl_true",   
        mainPanel(
            h4("What happens to OLS estimates if there are no violations?"),
            p("There are no violations to OLS's underlying assumptions about the true DGP."),
            p("The OLS estimates should be ", em("unbiased"), "and ", em("efficient.")),

            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns."),
            p(strong("Efficient:"), "Mathematical in nature, because 'efficiency' technically refers to 'OLS's estimates will have the smallest possible SEs, compared 
              to all other tools we could use.'  The SE's average value (5th row) from this scenario will serve as a rough reference for all the other scenarios' SEs, 
              to give us a *very* imperfect heuristic.")
        )
    )
)