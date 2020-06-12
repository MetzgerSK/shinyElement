ui <- fluidPage(
    
    # Loading up the CSS
    includeCSS("style.css"),
    includeCSS("bsCallout.css"),
    
    # To force the MathJax to wrap
    tags$head(tags$script(type = "text/x-mathjax-config", 
                            'MathJax.Hub.Config({
      "HTML-CSS": { linebreaks: { automatic: true } },
             SVG: { linebreaks: { automatic: true } }
    });')),

    titlePanel(HTML("MLE: The Intuition - Logit")), 
    theme = shinythemes::shinytheme("sandstone"),  

    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on
    
    tabsetPanel(     
        tabPanel("Main", value="main_true",
            sidebarLayout(
                sidebarPanel(
                    # Generate fake data
                    h3("Start by generating some fake data", style="margin-top:0px;"),
                    numericInput("seed", label = "Random Seed", 
                                 min=1, value = 18),
                    sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)",
                                min = 25, max = 500, step = 25, value = 150),
                    actionButton("dataGenButton", "Generate Fake Data", icon("table"), 
                                 style=""),
                    
                    # Layout the victory conditions
                    conditionalPanel("input.dataGenButton != 0",
                        helpText("Refresh page to reset (can then set seed and generate new dataset)"),        
                        div(id="bHalf",
                            hidden(h3(id="inf_instrText", "Now: find the slope and intercept that gives you the largest \"point\" value")),
                            h3(id="formal_instrText", "Now: find the slope and intercept that gives you the largest log-likelihood value"),
                            sliderInput("aHat", label = "Intercept \\(\\left(\\alpha \\right) \\)", 
                                        min = -6, max = 6, step = 0.25, value = 3.5),
                            sliderInput("b1Hat", label = "x's Coeff \\(\\left(\\beta_1 \\right) \\)",
                                        min = -6, max = 6, step = 0.25, value = -0.75),
                            
                            # Reset/giving up
                            actionButton("resetButton", "Reset Sliders", icon("repeat"), 
                                         style="color: #000; background-color: #999; margin-bottom: 5px;"),
                            br(),
                            actionButton("solnButton", "Show Answer", icon("exclamation-triangle"), 
                                         style="color: #e5e5e5; background-color: #960000;")
                        )
                    )
                ),

        
                mainPanel(
                    absolutePanel(top = -50, right = 75, width = 75,
                        shinyWidgets::switchInput(inputId = "lang", label="Prose Style", value = TRUE, 
                                                  size="small", onLabel="Informal", offLabel="Formal",  
                                                  inline=TRUE)    
                    ),
                    h4("Proposed line's equation:"),
                    uiOutput("eq_logit"),
  
                    conditionalPanel("input.dataGenButton != 0",
                        div(id="wrapper_llhInfo",
                            conditionalPanel("input.solnButton==0",
                                div(id="inside_llh", class="ptTotal_llh",
                                    h4("Current point total:", id="inf_ptTotal"),
                                    h4("Current log-likelihood value:", id="formal_ptTotal"),
                                    uiOutput("llh")
                                )
                            ),
                            conditionalPanel("input.solnButton !=0",
                                br(),
                                HTML("<span class='resJump', style='font-size:1.3em;'>
                                        <i class='fas fa-angle-double-right'></i> Jump to 
                                        <a href='#wrapper_rslts'>actual results table</a>
                                     </span>")
                            ),
                            div(id="inside_restore", class="llhBtn",
                                circleButton("fullLLHButton", class="pulse", icon = icon("eye"), status = "default", size = "sm",
                                            style="background-color:#eee; color:#000;"),
                                bsTooltip("fullLLHButton", "Full LLH expression <br/> (NOTE: lots of math incoming)", placement = "right", trigger = "hover",
                                            options = list(container = "body", style = "font-variant: normal !important; text-transform: none !important;")),
                                
                                # Load up the modal expl
                                source("ui_llh__popup.R", local=TRUE)$value
                
                            )
                        ),
                        hr(),
                        div(id="wrapper_llhInfo",                
                            div(id="inside_llh", class="allTime",
                                hidden(h4("Current all-time best guess:", id="inf_bestGuess")),
                                hidden(h4("Last best guess:", id="inf_bestGuess_ans")),  
                                
                                h4("Maximum log-likelihood value (so far):", id="formal_bestGuess"),
                                hidden(h4("Best guess for maximum log-likelihood value:", id="formal_bestGuess_ans")), 
                                uiOutput("bestGuess"),
                                
                                conditionalPanel("input.solnButton!=0",
                                    uiOutput("bestGuess_ests"),
                                    hidden(h4("Actual answer:", id="inf_actual", style="padding-top:3px;")),
                                    h4("Actual log-likelihood value:", id="formal_actual"),
                                    uiOutput("trueLLH"),
                                    uiOutput("trueEsts")
                                )
                            ),
                            div(id="inside_restore", style="margin-left: 5px;",
                                actionButton("restoreButton", "Restore", icon("upload"))
                            )
                        ),
                        br(),
                        plotOutput("gph"),
                        
                        div(id="inside_explain",
                            actionButton("explainButton", "What's going on here?", icon("question-circle"))
                        ),
                        
                        br(),
                        
                        # Pop-up window with explanation about what's going on
                        source("ui_expl__popup.R", local=TRUE)$value,
                        
                        # The actual results 
                        hidden(
                            div(id="wrapper_rslts",
                                tabsetPanel(
                                    tabPanel("Table",
                                        uiOutput("sgzTable") %>% withSpinner(., type = 7, color = "#325D88"),
                                    ),
                                    tabPanel("R Output",
                                        verbatimTextOutput("modObj")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        tabPanel("Data", value="data_true",
            h4(class="simFyiHdr", "NOTE: Must generate data first on 'Main' tab."),
            div(class="inline-block-center rawRes", 
                div(
                    shinyjs::hidden(downloadButton("downloadData", "Download Dataset"))
                )
            ),
            div(class="ibcOuter", # (keeping as sep ibc divs in case you revert DT in future)
                div(class="ibcInner", style="width:60%;",
                    DT::dataTableOutput("data_table")
                )
            )
        )
    ),
    fluidRow(align="center", 
        br(), 
        HTML(paste0(strong("Author: "), a(href="http://www.shawnakmetzger.com", "Shawna K. Metzger"), ", ",
                  a(href="mailto:shawna@shawnakmetzger.com", "shawna@shawnakmetzger.com"))), br(), 
        HTML("<em>Using Shiny to Teach Econometric Models</em>, Cambridge University Press")
    )
)