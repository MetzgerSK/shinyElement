tabsetPanel( 
    tabPanel("Main", value="main_nnorm",       
        titlePanel("Simulation Setup: Non-Normally Distributed Errors"),
         
        sidebarLayout(
            sidebarPanel(
                numericInput("seed_nnorm", label = "Random Seed", value = 130117, min = 1),
                br(),
                sliderInput("nObs_nnorm", label = "# of Subjects \\(\\left(n \\right) \\)", 
                            min = 50, max = 1000, step = 50, value = 400),
                sliderInput("sims_nnorm", label = "# of Simulations", 
                            min = 200, max = 1000, step = 50, value = 500),
                br(),
                sliderInput("aHat_nnorm", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                            min = -5, max = 5, step = 0.25, value = 3.5),
                sliderInput("b1Hat_nnorm", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_1 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = -0.75),
                sliderInput("b2Hat_nnorm", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_2 \\right) \\)"), 
                            min = -5, max = 5, step = 0.25, value = 0.25),
                sliderInput("noise_nnorm", label = "Shape \\(\\left(p \\right) \\)", 
                            min = 0.25, max = 5, step = 0.25, value = 0.5),
                br(),             
                actionButton("randPars_nnorm", "Shuffle", icon("random"), 
                              style="color: #999 !important; background-color: #333 !important"),
                bsTooltip("randPars_nnorm", "Pick a different set of arbitrary parameter values",
                          placement="bottom", trigger="hover"),
                actionButton("reset_nnorm", "Reset", icon("repeat"), 
                              style="color: #000 !important; background-color: #999 !important"),
                bsTooltip("reset_nnorm", "Reset back to default parameter values",
                          placement="bottom", trigger="hover"),
                br(),br(),
                actionButton("goButton_nnorm", "Simulate!", icon("rocket"), 
                              style="color: #fff !important; background-color: #337ab7 !important; border-color: #2e6da4 !important")
                 
            ),
            mainPanel(
                h4("True DGP:", align = "left"),
                uiOutput("eq_true_nnorm"), 
                bsTooltip("eq_true_nnorm",    
                            "TIEV = Type I Extreme Value",
                            placement="left", trigger="hover"
                ),
                
                div(id="instr_nnorm",
                    br(),
                    h4("Simulation Results"),
                    simInstrs,
                    br(),
                ),
                
                shinyjs::hidden(
                    div(id="all_nnorm",
                        h4("Selected Model:", align = "left"),
                        uiOutput("eq_estm_nnorm"),
        
                        br(),br(),
                        div(class="inline-block-center",
                            div(
                                radioGroupButtons(
                                    "model_nnorm", label = "Model Choice",
                                    choiceNames = list(HTML("OLS, DV = <em>t</em>"), 
                                                       HTML("OLS, DV = ln(<em>t</em>)"), 
                                                       HTML("Weibull, DV = <em>t</em>")),
                                    choiceValues = list(1, 2, 3), selected = 1, status="primary"
                                )
                            ),
                            br(),
                            div(
                                br(),
                                tableOutput("res_reg_nnorm") %>% withSpinner(., type = 6, color = "#33b5e5")  
                            )
                        ),
                        br(),br(),

                        # Start of explanations
                        h4("Why do non-normally distributed errors matter?"),
                        paste(nonNormText),
                        br(),br(),
                        source("uiPt__expls_nnorm.R", local=TRUE)$value
                    )    
                ) 
            )
        )
    ),
    tabPanel("Raw Simulation Output", value="output_nnorm",   
        mainPanel(
            h4("NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            DT::dataTableOutput("table_rawOutpt_nnorm")
        )
    ),
    tabPanel("Distribution Graph", value="graph_nnorm" ,   
        sidebarPanel(
            selectInput("paramG_nnorm", "Estimate",
                      c("aHat", "b1Hat", "b2Hat", "Shape (pHat)"))
        ),
        mainPanel(
            h4("NOTE: must click 'Simulate!' on 'Main' tab first.", align = "left"),
            plotOutput("distPlot_nnorm")
        )
    )
)
