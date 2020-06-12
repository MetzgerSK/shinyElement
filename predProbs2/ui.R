library(shinyjs)

# Linear combination (generic) - to define once
linCombGen <- "\\text{intercept} + \\beta_\\text{2nd class?}*\\left( \\text{2nd class?} \\right)
                                 + \\beta_\\text{3rd class?}*\\left( \\text{3rd class?} \\right)
                                 + \\beta_\\text{age}*\\left( \\text{age} \\right)
                                 + \\beta_\\text{male?}*\\left( \\text{male?} \\right)"

# > UI START ----
ui <- fluidPage(
    # Custom CSS
    tags$style(type = "text/css", "
        /* Lato's italic font isn't loading, so do it manually. */
        @import url('//fonts.googleapis.com/css?family=Lato:400i');
      
        /* Reversing action button's color on hover, overriding Superhero theme's default */
        .btn:hover{
            background-color: #FFFFFF !important;
            color: #337ab7 !important;
        }

        /* Making the min/max labels more legible than Superhero theme's default */
        .irs-min, .irs-max {
            color: #999999;
        }

        /* Making MathJax hover text black so it's readable */
        #MathJax_Zoom{
            color: #000000;
        }

        /* Making table text legible + MathJax styling */
        table{
            font-size: 115%;
            font-family: MathJax_Main;
        }
    "), 
    
    titlePanel(HTML("Logit Predicted Probabilities: The <em>Titanic</em> Sinking")), # title for app
    theme = shinythemes::shinytheme("superhero"),                                    # quick arbitrary theme
    
    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on
    
    sidebarLayout(
        # Defines left sidebar's contents
        sidebarPanel(
            # Left sidebar's contents start
            div(
                actionButton("estmButton", " Estimate Model", icon("calculator"), 
                             style="color:#fff; background-color:#337ab7; border-color:#2e6da4;
                                    box-shadow:3px 3px 35px 0px rgba(153,153,153,0.45);"), 
                style="text-align: center; margin-bottom: 35px;"  # Wrapped in div to center the button.  
                                                                  # Also added bottom margin to get spacing between button and rest (instead of br(),br(),)
            ),
            p(id="instrText", "Must estimate model first before calculating any predicted probabilities.", 
              style="text-align:center; color:#FF5252; font-weight:bold;"), 
            disabled(
                selectInput("coeffClass", "Ticket Type", 
                        choices = list("1st Class" = 1, "2nd Class" = 2, "3rd Class" = 3), 
                        selected = 1, multiple = FALSE),
                radioButtons("coeffGender", label = "Passenger Gender",
                            choices = list("Female" = 0, "Male" = 1), 
                            selected = 1),
                sliderInput("coeffAge", label = "Passenger Age", 
                            min = -1235, max = 71888, step = 1, value = 128)
            )
        ),
        # Defines main frame's contents
        mainPanel(
            # Main frame's contents start.
            
            # EQUATION: Generic expression
            h2("The generic expression for the calculation"),
            p(paste0('\\(
                \\widehat{\\Pr(y = 1)} = 
                    \\frac{\\exp \\left(', linCombGen, '\\right)}
                          {1 + \\exp \\left(', linCombGen, '\\right)} 
              \\)')
            ),

            # EQUATION: with values inserted (hidden until model's estimated)
            conditionalPanel("input.estmButton != 0",
                span(title="What you're setting in the left sidebar", h2("Inserting your covariate values")),
                uiOutput("fFormPrExpr")
            ),
            
            # summary() output (hidden until model's estimated)
            conditionalPanel("input.estmButton != 0",
                span(title="Where the beta values in the equations come from", h2("Estimated Logit Model")),
                uiOutput("modObj")             
            )
        )
    )
)