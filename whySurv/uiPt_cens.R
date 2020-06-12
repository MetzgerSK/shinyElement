tabsetPanel( 
    tabPanel("Main", value="main_cen",       
        titlePanel("Simulation Setup: Right Censoring"),
           
        sidebarLayout(
            sidebarPanel(
                numericInput("seed_cens", label = "Random Seed", value = 130117, min = 1),
                br(),
                div(id="rcSlider_cens",
                    sliderInput("perc_cens", label = "% Subjects w/Censoring", 
                                min = 0, max = 95, step = 1, value = 50)
                ),
                br(),
                sliderInput("nObs_cens", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 400),
                sliderInput("sims_cens", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 500),
                br(),
                sliderInput("aHat_cens", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_cens", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_cens", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_cens", label = "Shape \\(\\left(p \\right) \\)", 
                            min = 0.25, max = 5, step = 0.25, value = 0.5),
             
                actionButton("randPars_cens", "Shuffle", icon("random"), 
                              style="color: #999 !important; background-color: #333 !important"),
                bsTooltip("randPars_cens", "Pick a different set of arbitrary parameter values",
                          placement="bottom", trigger="hover"),
                actionButton("reset_cens", "Reset", icon("repeat"), 
                              style="color: #000 !important; background-color: #999 !important"),
                bsTooltip("reset_cens", "Reset back to default parameter values",
                          placement="bottom", trigger="hover"),
                br(),br(),
                actionButton("goButton_cens", "Simulate!", icon("rocket"), 
                              style="color: #fff !important; background-color: #337ab7 !important; border-color: #2e6da4 !important")
                 
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("eq_true_cens"),  
                bsTooltip("eq_true_cens",    
                            "TIEV = Type I Extreme Value",
                            placement="left", trigger="hover"
                ),
                
                div(id="instr_cens",
                    br(),
                    h4("Simulation Results"),
                    simInstrs,
                    br(),
                ),
                shinyjs::hidden(
                    div(id="all_cens",
                        h4("Selected Model:", align = "left"),
                        uiOutput("eq_estm_cens"),
                        br(),br(),
                        
                        div(class="inline-block-center",
                            div(
                                radioGroupButtons(
                                    "model_cens", label = "Selected Model",
                                    choiceNames = list(HTML("OLS, DV = <em>t</em>"), 
                                                       HTML("OLS, DV = ln(<em>t</em>)"), 
                                                       HTML("CensReg, DV = ln(<em>t</em>)"),
                                                       HTML("Weibull, DV = <em>t</em>")
                                                       ),
                                    choiceValues = list(1, 2, 4, 3), selected = 1, status="primary"
                                )
                            ),
                            br(),
                            div(
                                br(),
                                tableOutput("res_reg_cens") %>% withSpinner(., type = 6, color = "#33b5e5")
                            )
                        ),
                        br(),br(),
                        
                        # Start of explanations
                        h4("Why does right-censored data matter?"),
                        paste(censText),
                        br(),br(),
                        source("uiPt__expls_cens.R", local=TRUE)$value
                    )
                )
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_cens",   
        mainPanel(
            h4("NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            DT::dataTableOutput("table_rawOutpt_cens")
        )
    ),
    tabPanel("Distribution Graph", value="graph_cens" ,   
        sidebarPanel(
            selectInput("paramG_cens", "Estimate",
                      c("aHat", "b1Hat", "b2Hat", "Shape (pHat)"))
           
        ),
        mainPanel(
            h4("NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_cens")
        )
    )
)