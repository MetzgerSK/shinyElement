ui <- fluidPage(

    # To force the MathJax to wrap 
    tags$head(tags$script(type = "text/x-mathjax-config", 
                            'MathJax.Hub.Config({
      "HTML-CSS": { linebreaks: { automatic: true } },
             SVG: { linebreaks: { automatic: true } }
    });')),

    # Custom CSS
    includeCSS("style.css"),
    includeCSS("bsCallout.css"),

    titlePanel(HTML("LASSO: The Intuition")),   
    theme = shinythemes::shinytheme("sandstone"),  
    
    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on
    
    # Add the FYI about double-clicking on n
    tags$script("
        $(document).ready(function() {
            setTimeout(function() {
                shinyBS.addTooltip(\'nObs\', \'tooltip\', 
                    {\'placement\': \'bottom\', \'trigger\': \'hover\', \'title\': 
                     \'Double click to change min/max.\'
                })
            }
            , 500)
        });
    "),

    # Start the tabs
    tabsetPanel(     
        tabPanel("Main", value="main_true",
            sidebarLayout(
                # Left sidebar
                sidebarPanel(
                   # Generate fake data
                    h3("Start by generating some fake data", style="margin-top:0px;"),
                    numericInput("seed", label = "Random Seed", 
                                 min=1, value = 18),
                    sliderInput("nObs", label = "# of Subjects \\(\\left(n \\right) \\)",
                                min = 3, max = 150, step = 3, value = 9),
                    actionButton("nextButton", "Generate Fake Data", icon("table"), 
                                 style=""),
                    
                    conditionalPanel("input.nextButton != 0", 
                        helpText("Refresh page to reset (can then set seed and generate new dataset)"),  
                        
                        div(class="lambCl",
                            h3(id="lamInstr", style="margin-top:10px !important", "Next, choose \\(\\lambda \\)'s value"),
                            sliderInput("lambda", "\\(\\lambda\\)", min=0, max=0.04, step=0.001, value=0.02),
                            
                            div(class="inline-block-center", div(
                                shinyjs::hidden(uiOutput("lamVal")),
                                HTML(" <span style='font-size: 80%;'>
                                (<a id='benchLink' href='' style='color:#960000;'>Wait, what's this?</a>)</span>")
                            )),
                            
                            actionButton("dataGenButton", "Next", icon("arrow-alt-circle-right"), 
                                         style=""),
                    
                            # (SOURCE THE LAMBDA BSMODAL)
                            source("ui_lam__popup.R", local=TRUE)$value
                        ),
                        
                        # Layout the victory conditions
                        conditionalPanel("input.dataGenButton != 0",
                                  
                            div(id="bHalf",
                                hidden(h3(id="inf_instrText", "Now: find the two slopes that give you the smallest \"point\" value")),
                                h3(id="formal_instrText", "Now: find the two slopes that give you the smallest penalized sum of squares"),
                                sliderInput("b1Hat", label = HTML("<em>x</em>'s Coeff \\(\\left(\\beta_x \\right) \\)"),
                                            min = -6, max = 6, step = 0.25, value = 0.25),
                                sliderInput("b2Hat", label = HTML("<em>z</em>'s Coeff \\(\\left(\\beta_z \\right) \\)"),
                                            min = -6, max = 6, step = 0.25, value = 0),
                                helpText("No true intercept; all variables are standardized."),
                                
                                # Reset/giving up btns
                                actionButton("resetButton", "Reset Sliders", icon("repeat"), 
                                             style="color: #000; background-color: #999; margin-bottom: 5px;"),
                                br(),
                                actionButton("solnButton", "Show Answer", icon("exclamation-triangle"), 
                                             style="color: #e5e5e5; background-color: #960000;")
                            )
                        )
                    )
                ),

                # Main panel
                mainPanel(
                    absolutePanel(top = -50, right = 75, width = 75,
                        switchInput("lang", "Prose Style", value = TRUE, 
                                    onLabel="Informal", offLabel="Formal",  
                                    size="small", inline=TRUE)    
                    ),
                    
  
                    conditionalPanel("input.dataGenButton != 0",
                        h4("Proposed line's equation:"),
                        uiOutput("eq_lm"),
                                     
                        div(id="wrapper_llhInfo",
                            
                            div(id="inside_llh", class="ptTotal_llh",
                                conditionalPanel("input.solnButton==0",
                                    h4("Size of current penalty:"),
                                    uiOutput("lassoPenTerm"),
                                    h4("Current point total:", id="inf_ptTotal"),
                                    h4("Current penalized sum of squared residuals value:", id="formal_ptTotal"),
                                    uiOutput("ssr")
                                ),                            
                                conditionalPanel("input.solnButton!=0",
                                    br(),
                                    HTML("<span class='resJump', style='font-size:1.3em;'>
                                            <i class='fas fa-angle-double-right'></i> Jump to 
                                            <a href='#wrapper_rslts'>actual results table</a>
                                         </span>")
                                ),
                            ),
                            div(id="inside_restore", class="llhBtn",
                                circleButton("fullSSRButton", class="pulse", icon = icon("eye"), status = "default", size = "sm",
                                            style="background-color:#eee; color:#000;"),
                                bsTooltip("fullSSRButton", "Full pen. SSR expression <br/> (NOTE: math incoming)", 
                                           placement = "right", trigger = "hover", 
                                            options = list(
                                                container = "body" 
                                            )
                                          ),
                        
                                # Pop-up window with explanation about what's going on
                                source("ui_llh__popup.R", local=TRUE)$value
                            )
                        ),
                        hr(),
                        div(id="wrapper_llhInfo",                
                            div(id="inside_llh", class="allTime",
                                hidden(h4("Current all-time best guess:", id="inf_bestGuess", class="bestGuess")),
                                hidden(h4("Last best guess:", id="inf_bestGuess_ans", class="bestGuess_ans")),  
                                h4("Smallest penalized SSR value (so far):", id="formal_bestGuess", class="bestGuess"),
                                hidden(h4("Best guess for smallest penalized sum of squares value:", 
                                          id="formal_bestGuess_ans", class="bestGuess_ans")), 
                                uiOutput("bestGuess"),
                                
                                conditionalPanel("input.solnButton!=0",
                                    uiOutput("bestGuess_ests"),
                                    hidden(h4("Actual answer:", id="inf_actual", style="padding-top:3px;")),
                                    h4("Actual Penalized SSR value:", id="formal_actual"),
                                    uiOutput("trueSSR"),
                                    uiOutput("trueEsts")
                                )
                            ),
                            div(id="inside_restore", style="margin-left: 5px;",
                                actionButton("restoreButton", "Restore", icon("upload"), 
                                      style="color: #000; background-color: #FF8C00; height: 30px; 
                                             padding: 0px 10px 0px 10px; font-size:80%;
                                             display: -webkit-box; display: -ms-flexbox; display: flex; 
                                            -webkit-box-align: center; -ms-flex-align: center; align-items: center;
                                            -webkit-box-pack: center; -ms-flex-pack: center; justify-content: center;
                                            ")
                            )
                        ),
                        br(),

                        fluidRow(
                            column(6, 
                                    div(class="steps",
                                        "\\(z=0 \\text{ observations}\\)"
                                    ), 
                                    plotOutput("gph0") 
                            ),
                            column(6, 
                                    div(class="steps",
                                       "\\(z=1  \\text{ observations}\\)"
                                    ), 
                                    plotOutput("gph1") 
                            ),
                        ),
                        
                        div(id="inside_explain",
                            actionButton("explainButton", "What's going on here?", icon("question-circle"))
                        ),
                        
                        br(),
                        
                        # Pop-up window with explanation about what's going on
                        source("ui_expl__popup.R", local=TRUE)$value,
                        
                        hidden(
                            div(id="wrapper_rslts",
                                HTML("<a id='resultsOutput'></a>"),
                                tabsetPanel(
                                    tabPanel("Table",
                                        uiOutput("sgzTable"),
                                        helpText("No standard errors in table because SEs are not available from LASSO.")
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
            h4(class="simFyiHdr", "NOTE: Must generate data first on Main tab."),
            div(class="inline-block-center rawRes", 
                div(
                    shinyjs::hidden(downloadButton("downloadData", "Download Dataset"))
                )
            ),
            div(class="ibcOuter",
                p("** All variables are standardized **"),
                div(class="ibcInner", style="width:60%",
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