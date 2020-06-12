# To save time down the road (can't be inside fluidPage(), or else it'll print)
linCombo <- "\\text{intercept}_k  + \\beta_{\\text{ed lvl=pmry?}_k}*\\left( \\text{ed lvl=pmry?} \\right)
                                  + \\beta_{\\text{ed lvl=lw. sec?}_k}*\\left( \\text{ed lvl=lw. sec?} \\right)
                                  + \\beta_{\\text{ed lvl=up. sec?}_k}*\\left( \\text{ed lvl=up. sec?} \\right)
                                  + \\beta_{\\text{ed lvl=college?}_k}*\\left( \\text{ed lvl=college?} \\right)
                                  + \\beta_{\\text{relig?}_k}*\\left( \\text{relig?} \\right)
                                  + \\beta_{\\text{ideol.}_k}*\\left( \\text{ideol.} 
            "

# > UI START ----
ui <- fluidPage(
    # Custom CSS
    tags$style(type = "text/css", "
        /* For the button font, JIC. */
        @import url('//fonts.googleapis.com/css?family=News+Cycle');

        /* for equation buttons */
        #eqBtns .btn{
            font-family: 'News Cycle', 'Arial Narrow Bold', sans-serif;
            font-size: 110%;
            font-weight: 700;
        }
    "),

    # Overall UI properties
    titlePanel(HTML("Predicted Probs: Vote Choice in the 2012 Mexican Presidential Election")), # title for app
    theme = shinythemes::shinytheme("journal"),                                                 # quick arbitrary theme
    
    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on
    
    # For main content (fluidRow() only needed b/c of CUP footer)
    fluidRow(style="margin:0px;",
        sidebarLayout(
            # Defines left sidebar's contents
            sidebarPanel(
                # Left sidebar's contents start
                div(
                    actionButton("estmButton", " Estimate Model", icon("calculator"), 
                                 style="color:#fff; background-color:#337ab7; border-color:#2e6da4;
                                        box-shadow:3px 3px 35px 0px rgba(153,153,153,0.45); margin-bottom: 8px;"), 
                    style="text-align: center;"  # Wrapped in div to center the button.  
                                                 # Also added bottom margin to get spacing between button and rest (instead of br(),br(),)
                ),
                div(
                    selectInput("reflevel", "Reference Category", 
                                choices = list("PRI", "PAN", "PRD", "PNA", "None/Blank Ballot" = "None/Blank"), 
                                selected = "None/Blank", multiple = FALSE),
                    hidden(
                        p(id = "refreshInstr", "**To change the reference category, refresh the page.**", 
                          style="text-align: center; font-style: italic; font-size: 75%;")
                    ),
                    style="margin-bottom: 35px;"             
                ),
                p(id="instrText", "Must estimate model first before calculating any predicted probabilities.", 
                  style="text-align:center; color:#FF5252; font-weight:bold;"),

                disabled(
                    selectInput("coeffEduc", "Voter's Education Level",
                                choices = list("None/Early Childhood Only" = 0, "Primary" = 1, 
                                               "Lower Secondary" = 2, "Upper Secondary" = 3, "College" = 4),
                                selected = 1, multiple = FALSE),
                    radioButtons("coeffRelig", label = "Is voter religious?",
                                choices = list("Somewhat/Very" = 1, "No Beliefs/Not Very" = 0),
                                selected = 1),
                    sliderInput("coeffIdeo", label = "Voter's Ideology",
                                min = 0, max = 10, step = 1, value = 5),
                    p(em("(0 = Left, 10 = Right)"), 
                      style = "font-color: #999; font-size: 75%; margin-top: 0px;")

                )
            ),
            # Defines main frame's contents
            mainPanel(
                # Main frame's contents start

                # EQUATION: Generic expression
                h2("The generic expression for the calculation"),

                p(paste0(
                  '\\(\\widehat{\\Pr(y_k = 1)} = \\frac{\\exp \\left(', linCombo, ' \\right) \\right)}
                                                       {1 + \\sum_{k=1}^{K}\\exp \\left(', linCombo, ' \\right) \\right)} \\\\
                      \\text{    for } \\forall~k \\in K.
                  \\)'
                )),

                # EQUATION: with values inserted (hidden until model's estimated)
                conditionalPanel("input.estmButton != 0",
                    uiOutput("choices"),

                    span(title="What you're setting in the left sidebar", h2("Inserting your covariate values")),

                    # Creating show/hide buttons
                    checkboxGroupButtons(
                          inputId = "eqBtns", 
                          choices = c("Loading..."),
                          selected = "1", 
                          status = "info",  size='sm'
                    ),

                    # The divs to show/hide
                    div(id="eqWrapper",
                        hidden(div(id = "kFake",
                               p("Select one or more parties using the buttons above.")
                        )),
                        div(id="k1", 
                            uiOutput("k1Eq"),
                            br()
                        ),
                        hidden(div(id="k2", 
                            uiOutput("k2Eq"),
                            br()
                        )),
                        hidden(div(id="k3", 
                            uiOutput("k3Eq"),
                            br()
                        )),
                        hidden(div(id="k4", 
                            uiOutput("k4Eq"),
                            br()
                        )),
                        hidden(div(id="k5", 
                            uiOutput("k5Eq"),
                            br()
                        ))
                    )
                ),

                # Model output (hidden until model's estimated)
                conditionalPanel("input.estmButton != 0",
                    span(title="Where the beta values in the equations come from", h2("Estimated Multinomial Logit Model")),

                    tabsetPanel(
                        tabPanel("Table",
                            uiOutput("sgzTable") %>% withSpinner(., type = 7, color = "#325D88"),
                        ),
                        tabPanel("R Output",
                            verbatimTextOutput("modObj")
                        ),
                        tabPanel("Stata Output",
                            verbatimTextOutput("modObj_stata")
                        )
                    ),
                    br()

                )


            )
        ),
        fluidRow(
            div(id="footer", style="text-align:center;",
                HTML("<strong>Data Source</strong>: 
                      <a href='http://www.cses.org/datacenter/module4/module4.htm'</a>CSES 2011&ndash;2016 survey</a>"
                )
            )
        )    
    ), # end of main content fluidRow
    
    # Footer
    fluidRow(align="center", 
        br(), 
        HTML(paste0(strong("Author: "), a(href="http://www.shawnakmetzger.com", "Shawna K. Metzger"), ", ",
                  a(href="mailto:shawna@shawnakmetzger.com", "shawna@shawnakmetzger.com"))), br(), 
        HTML("<em>Using Shiny to Teach Econometric Models</em>, Cambridge University Press")
    )         
)
