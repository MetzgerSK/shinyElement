tabsetPanel(     
    tabPanel("Main", value="main_end",     
        titlePanel("Simulation Setup: Omitted Variable"),
        
        nObsBsTooltip("end"),
        
        sidebarLayout(
            sidebarPanel(
                div(class="inline-block-center",
                    div(
                        actionButton("copyButton_end", "No Violation Scenario",icon("copy")), 
                        bsTooltip("copyButton_end", noViolTooltip,
                                  placement="bottom", trigger="hover")
                    )
                ),
                div(class="violSpec",
                    h4("Violation-Specific Options:"),
                    sliderInput("corrXZ", label = HTML("Correlation between <em>x</em> and <em>z</em>"), 
                                min = -1, max = 1, step = 0.1, value = 0.5) #,
                ) ,
                numericInput("seed_end", label = "Random Seed", value = 110116, min = 1),
                br(),
                sliderInput("nObs_end", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 500),
                sliderInput("sims_end", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 1000),
                br(),
                sliderInput("aHat_end", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_end", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_end", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_end", label = HTML("<em>u</em>'s Noise \\(\\left(\\sigma \\right) \\)"), 
                            min = 1, max = 10, step = 1, value = 1),
                 
                actionButton("goButton_end", "Simulate!", icon("rocket"), 
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
                div(class="bs-callout bs-callout-info",
                    div(class="bs-close", icon("times-circle")),
                    h4("Multicollinearity"),
                    HTML("<strong>Can also demonstrate multicollinearity's effect.  
                            Set \\(| \\text{Corr} \\left( x,z \\right) | \\) to 0.85&ndash;0.99.</strong>") 
                ),
                div(class="bs-callout bs-callout-info",
                    div(class="bs-close", icon("times-circle")),
                    HTML("<h4>No Correlation with <em>x</em></h4>"),
                    HTML("<strong>Can also demonstrate effect of omitting a relevant variable, but one uncorrelated with <em>x</em>.  
                          Set \\( \\text{Corr} \\left( x,z \\right) \\) to 0.</strong>")
                ),
                h4("True DGP:", align = "left"),
                uiOutput("equation_end"),  
                h4("Estimated Model:", align = "left"),
                '\\(y= \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\sigma} u\\)',
                br(),br(),br(),
                h4("Simulation Results"),
                "Set values at left, click 'Simulate!' button to run, and wait 3-15 seconds for results to appear.",
                tableOutput("data_end") %>% withSpinner(., type = 6, color = "#33b5e5"),
                br(),
                
                shinyjs::hidden(
                    downloadButton("datDwn_end", "Download a Fake Dataset", icon("file-download"),
                              style="color:#EEE; background-color:#718C6A;"
                    ),
                    bsTooltip("datDwn_end", "Save an example dataset from one draw of these simulations (CSV)",
                          placement="bottom", trigger="hover")
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_end",   
        mainPanel(
            h4(class="simFyiHdr_end", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_end", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#384635;"
                        ),
                        bsTooltip("rawDwn_end", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ),  
            DT::dataTableOutput("table_end") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Estimates: Distribution Plots", value="graph_end",   
        sidebarPanel(
            selectInput("paramG_end", "Estimate",
                        c("aHat", "b1Hat"))
        ),
        mainPanel(
            h4(class="simFyiHdr_end", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_end") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("Residuals: Distribution Plots", value="graphQQ_end" ,   
        sidebarPanel(
            selectInput("plotErrDist_end", "Plot Type",
                       plotErrDistChoices)
        ),
        mainPanel(
            h4(class="simFyiHdr_end", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
			fluidRow(
				column(4,
					sliderInput("selectSim_plotErrDist_end", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
						# ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
				),
				column(4,
                    shinyjs::hidden(
                        radioGroupButtons(
                            inputId = "plotErrDist_heteroPlot_end",
                            label = "Quantity for Horizontal Axis", 
                            choiceNames = c("\\(x \\)", "\\(\\hat{y}\\)"),
                            choiceValues = c("x", "yHat")
                        )
                    )
                ),
                column(4,
                    div()
                )
            ),
            div(class="inline-block-center",
                radioGroupButtons(
                    inputId = "residSel_end",
                    label = "Select Residual Type", 
                    choices = residChoices,
                    selected = "reg",
                    status = "primary"
                )
            ),
            
            uiOutput("distPlotErr_endUI") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("True \\(y \\) vs. \\( \\hat{y} \\)", value="graphYHat_end" ,   
        fluidPage(align="center",
            h4(class="simFyiHdr_end", "NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            sliderInput("selectSim_plotYHat_end", label = "Select Simulation Draw", min = 1, max = 1000, step = 1, value = 1),
                # ^ The max value for this updates in serverPt_graphs.R, based on the # of rows in the sim results in memory
            plotlyOutput("distPlotYHat_end") %>% withSpinner(., type = 6, color = "#33b5e5")
        )
    ),
    tabPanel("What should I see?", value="expl_end",   
        mainPanel(
            h4("What happens to OLS estimates if we omit a relevant variable?"),
            HTML("<p>OLS requires that any variable correlated with both <em>x</em> and <em>y</em> is 
                 included in the model, if we are to recover <em>x</em>'s effect on <em>y</em>.  
                 Since we omit <em>z</em> in our specification, we violate this assumption.</p>"),
            HTML("<p>The OLS estimates will be <em>biased</em> but <em>efficient</em>.  This will 
                 manifest in our estimate of <em>x</em>'s effect on <em>y</em> because the omitted 
                 variable is correlated with <em>x</em>.</p>"),
            
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Biased:"), HTML("Top two rows won't match for <em>x</em>'s coefficient; top row won't 
                                    fall in between values in 3rd and 4th rows for this column.")),
            p(strong("Efficient:"), "5th row's values should match/be close to No Violations' 5th row (provided 
                                    all common slider/field values are identical)."),
            br(),br(),
            techSideNoteEu0
        )
    )
)