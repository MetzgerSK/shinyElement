ui <- fluidPage(
  sidebarLayout(
    # Define left sidebar's contents
    sidebarPanel(
        # Left sidebar's contents.
        actionButton("estmButton", "Estimate Model"),
        br(),br(),      # to force some space between button and other widgets 
        selectInput("coeffClass", "Ticket Type", 
                    choices = list("1st Class" = 1, "2nd Class" = 2, "3rd Class" = 3), 
                    selected = 1, multiple = FALSE),
        radioButtons("coeffGender", label = "Passenger Gender",
                     choices = list("Female" = 0, "Male" = 1), 
                     selected = 1),
        sliderInput("coeffAge", label = "Passenger Age", 
                    min = 1, max = 71, step = 1, value = 28)  
    ),
    # Define main frame's contents
    mainPanel(
        # Main frame's contents
        withMathJax(),
        p(
          '\\(\\Pr(y = 1) = \\frac{\\exp \\left( 
             \\text{intercept} + \\beta_\\text{2nd class?}*\\left( \\text{2nd class?} \\right)
              + \\beta_\\text{3rd class?}*\\left( \\text{3rd class?} \\right)
              + \\beta_\\text{age}*\\left( \\text{age} \\right)
              + \\beta_\\text{male?}*\\left( \\text{male?} \\right)
            \\right)}{1 + \\exp \\left(
             \\text{intercept} + \\beta_\\text{2nd class?}*\\left( \\text{2nd class?} \\right)
              + \\beta_\\text{3rd class?}*\\left( \\text{3rd class?} \\right)
              + \\beta_\\text{age}*\\left( \\text{age} \\right)
              + \\beta_\\text{male?}*\\left( \\text{male?} \\right)
          \\right)} \\)'
        ),
        uiOutput("fFormExpr"),
        uiOutput("predPr"),
        verbatimTextOutput("modObj")
    )
  )
) 