tabsetPanel( 
    tabPanel("Main", value="main_nlin",       
        titlePanel("Simulation Setup: Non-Linear in Parameters"),
        
        nObsBsTooltip("nlin"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_nlin", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_nlin", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                numericInput("seed_nlin", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_nlin", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_nlin", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_nlin", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_nlin", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_nlin", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_nlin", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                 
                actionButton("goButton_nlin", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("equation_nlin"),  
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_nlin") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_nlin", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_nlin", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_nlin",   
        mainPanel(
            h4(class="simFyiHdr_nlin", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
                div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_nlin", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_nlin", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ), 
            DT::dataTableOutput("table_nlin") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_nlin" ,   
        sidebarPanel(
            selectInput("paramG_nlin", "Estimate",
                       c("aHat", "b1Hat", "b2Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_nlin", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_nlin") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
     tabPanel("Residuals: Distribution Plots", value="graphQQ_nlin" ,   
        sidebarPanel(
            selectInput("plotErrDist_nlin", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_nlin", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            fluidRow(
                column(4,
                    sliderInput("selectSim_plotErrDist_nlin", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1)
                        # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
                ),
                column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_nlin",
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
                    inputId = "residSel_nlin",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            uiOutput("distPlotErr_nlinUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_nlin" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_nlin", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_nlin", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_nlin") %>% withSpinner(., type = 6, color = "#33b5e5"),
            yVsyHatCapt
        )
    ),
    tabPanel("What should I see?", value="expl_nlin" ,   
        mainPanel(
            h4("What happens to OLS estimates when true DGP isn't linear in parameters?"),
            HTML("<p>OLS makes an assumption about the DGP's functional form.  Specifically, it 
                 assumes that <em>y</em> is produced by a linear function whose parameters are not 
                 transformed in any way (i.e., no exponentiation, no logging, no raising 
                 to any power other than 1, no multiplying or dividing by other parameters, 
                 no trig functions).  We use the phrase 'linear in parameters' to describe 
                 a DGP with these properties.  For these simulations, <em>y</em> is generated by an 
                 exponential function.  The DGP's parameters are therefore non-linear.</p>"),
            p("The OLS estimates will be ", em("biased"), "and ", em("inefficient."),"  Violations of this assumption can also 
                induce other violations (e.g., heteroscedasticity).  This is why many texts encourage practitioners to check for 
                functional form violations first, before checking for evidence of other assumption violations."),
            
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Biased:"), "Top two rows won't match for all columns; top row won't fall in between values in 3rd and 4th rows for all columns."),
            p(strong("Inefficient:"), "5th row's values won't match No Violations' 5th row (provided all common slider/field values are identical).")
        )
    )
)