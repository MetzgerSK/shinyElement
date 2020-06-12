# To save time down the road (can't be inside fluidPage(), or else it'll print)
linCombo <- "\\text{intercept}  + \\beta_\\text{mar?}*\\left( \\text{mar?} \\right)
                                + \\beta_\\text{riv?}*\\left( \\text{riv?} \\right)
                                + \\beta_\\text{WI salience}*\\left( \\text{WI salience} \\right)
                                + \\beta_\\text{# past MIDs}*\\left( \\text{# past MIDs} \\right)
                                + \\beta_\\text{# past failed pcful atts}*\\left( \\text{# past failed pcful atts} \\right)
                                + \\beta_\\text{democ dyad?}*\\left( \\text{democ dyad?} \\right)
                                + \\beta_\\text{cap imbal}*\\left( \\text{cap imbal}  \\right)
            "

# > UI OBJECT ----
ui <- fluidPage(
    # Custom CSS
    ## (HTML wrapper needed to prevent > from becoming &gt;)
    tags$style(type = "text/css", 
        HTML(" 
        /* Lato's italic font isn't loading, so do it manually. */
        @import url('//fonts.googleapis.com/css?family=Lato:400i');
      
        /* Reversing action button's color on hover, overriding Superhero theme's default */
        .btn:hover{
            background-color: #FFFFFF !important;
            color: #337ab7 !important;
        }

        /* Tweaking hover behavior for reset button, to make it clear the button now says something new */
        #resetSldrBtn:hover{
            background-color: #000000 !important;
            color: #ffffff !important;
        }

        /* Making the min/max labels more legible than Superhero theme's default */
        .irs-min, .irs-max {
            color: #999999;
        }

        /* Making MathJax hover text black so it's readable */
        #MathJax_Zoom{
            color: #000000;
        }

        /* Styling explanation text as MathJax */
        #interp{
            font-family: MathJax_Main;
            font-size: 110%;
        }
        
        /* For model results tabset, make background different color */
          /* TAB HEADER */
          #wrapper_rslts .nav-tabs>li.active>a{
              background-color: #405c76;
          }
          /* TAB BODY */
          #wrapper_rslts .tab-content{
              background-color: #405c76;
              padding: 10px;  /* to give some cushion from the tab page's edges */
          } 
    ")), 

    titlePanel(HTML("Hensel, Mitchell, Sowers, and Thyne (2008) Table 3")), # title for app
    theme = shinythemes::shinytheme("superhero"),                           # quick arbitrary theme
    
    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on
    
    # For main content (fluidRow() only needed b/c of CUP footer)
    fluidRow(style="margin:0px;",
        sidebarLayout(
            # Defines left sidebar's contents
            sidebarPanel(
                div(
                    textOutput("modNum"),
                    style="text-align:center; font-style: italic;"
                ),
                div(
                    actionButton("estmButton", " Estimate Model", icon("calculator"), 
                                 style="color:#fff; background-color:#337ab7; border-color:#2e6da4;
                                        box-shadow:3px 3px 35px 0px rgba(153,153,153,0.45);"), 
                    style="text-align: center; margin-bottom: 8px; "  # Wrapped in div to center the button.  
                                                                      # Also added bottom margin to get spacing between button and rest (instead of br(),br(),)
                ),
                div(
                    selectInput("dv", "Dependent Variable", 
                                choices = list("Militarized Settlement Att."="MID", "Peaceful Settlement Att."="peaceful"), 
                                selected = "MID", multiple = FALSE),
                    hidden(
                        p(id="reestText", "**To reestimate with the other dependent variable, refresh the page.**", 
                          style="text-align: center; font-style: italic;")
                    ),
                    style="margin-bottom: 35px;"             
                ),
                p(id="instrText", "Must estimate model first before calculating any predicted probabilities.", 
                  style="text-align: center; color: #FF5252; font-weight: bold; margin-bottom: 20px;"), 
                disabled(
                    hidden(
                    div(id = "setMeanDiv",
                        actionButton("setMeanBtn", "Set All* at Mean/Mode",
                                     style="text-align: center; color:#fff; background-color:#337ab7; border-color:#2e6da4;
                                            box-shadow:3px 3px 35px 0px rgba(153,153,153,0.45);"),
                        p(em("* Except issue type"), 
                            style = "font-color:#999; font-size:75%; margin-top:0px;"),
                        style="text-align: center;"

                    )),
                    hidden(
                    div(id = "resetSldrDiv",
                        actionButton("resetSldrBtn", "Reset Slider Increments", icon("repeat"),
                                     style="text-align: center; color: #000; background-color: #999"),
                        style="text-align: center;"
                    )),
                    selectInput("coeffIssue", "Issue Type", 
                            choices = list("Territory" = 1, "Maritime" = 2, "River" = 3), 
                            selected = 1, multiple = FALSE),

                    sliderInput("coeffSal", label = "Within-Issue Salience",   
                                    min = 0, max = 12, step = 1, value = 0),
                    sliderInput("coeffPastMID", "Past Military Settlement Attempts (weighted)",   
                                min = 0, max = 5.4, step = 0.1, value = 0),
                    sliderInput("coeffPastPcFail", "Past Failed Peaceful Settlement Attempts (weighted)",
                                min = 0, max = 10.3, step = 0.1, value = 0),
                    radioButtons("coeffDemocDy", label = "Democratic Dyad?",
                                choices = list("Yes" = 1, "No" = 0), 
                                selected = 1),
                    sliderInput("coeffCINC", "Stronger State's % of Dyad's Military Capabiitiess",
                                min = 50, max = 100, step = 0.1, value = 50),
                    p( em("50% = parity, 100% = one state has all the dyad's mil caps"), 
                       style = "font-color: #999; font-size: 75%; margin-top: 0px;")
                )
            ),
            # Defines main frame's contents
            mainPanel(
                # Main frame's contents start.

                # EQUATION: Generic expression
                h2("The generic expression for the calculation"),
                p(paste0(
                 '\\(
                      \\widehat{\\Pr(y = 1)} = \\frac{\\exp \\left( ', linCombo, ' \\right)}
                                                     {1 + \\exp \\left(', linCombo, ' \\right)} 
                  \\)'
                )),

                # EQUATION: with values inserted (hidden until model's estimated)
                conditionalPanel("input.estmButton != 0",
                    span(title="What you're setting in the left sidebar", h2("Inserting your covariate values")),
                    uiOutput("fFormPrExpr"),
                    div(id="interp",
                        br(),
                        uiOutput("interp")
                    )
                ),

                # summary() output (hidden until model's estimated)
                conditionalPanel("input.estmButton != 0",
                    span(title="Where the beta values in the equations come from", h2("Estimated Logit Model")),
                    div(id="wrapper_rslts",
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
                        )
                    ),
                    br()            
                )
            )

        ),
        fluidRow(
            column(4, " "),
            column(8,
                div(id="footer", style="text-align:left;",
                    HTML(paste0(strong("Source"), ": Hensel, Paul R., Sara McLaughlin Mitchell, Thomas E. Sowers II, and Clayton L. Thyne.  
                            2008. ", a(href="https://doi.org/10.1177%2F0022002707310425", '\"Bones of Contention: Comparing Territorial, Maritime, and River Issues.\" '), 
                            em("Journal of Conflict Resolution"), " 52 (1): 117&#8211;143."
                    )))
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