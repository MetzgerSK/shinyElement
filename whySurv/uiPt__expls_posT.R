# wrapped in fluidRow only b/c app kicks error otherwise
fluidRow(style="margin:0px;",
    conditionalPanel("input.model_posT == 1",
        h4("What should I see?"),
        HTML("This is a naive model&mdash;as if you opened the dataset and ran an OLS regression using the duration, without transforming the duration at all."),
        HTML("The estimates will be <em>biased</em> because the true DGP is non-linear in parameters.  OLS requires linearity in parameters to obtain unbiased estimates; see the OLS assumption simulation <a href=\"https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/olsApp/\">here</a>."),
        "The standard errors will also be ", em("inefficient"), "due to the non-linearity.",
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Biased:"), "Top two rows won't match for all columns; top row won't fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Inefficient:"), "5th row's values will be larger than the log-normal's 5th row (for a very rough heuristic; the log-normal constitutes the 'No Violations' scenario)."
    ),
    conditionalPanel("input.model_posT == 2",
        h4("What should I see?"),
        "We have transformed the duration by taking the natural log, which gets rid of the non-linearity in parameters.  As a consequence, the OLS estimates will now be", em("unbiased."), "This includes the constant term, given that the error is distributed normal.",
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Efficient:"), "5th row will generally be the smallest (or very close to it) of all three models' fifth rows (for a very rough heuristic).  Additionally, 5th and 6th rows will match/be close for all columns."
    ),
    conditionalPanel("input.model_posT == 3",
        h4("What should I see?"),
        HTML("The log-normal parametric duration model assumes normal errors.  This model and an OLS model with ln(<em>y</em>) should be virtually identical to one another when no right-censored observations exist."),
        "The result is ", em("unbiased"), "and ", em("efficient"), "estimates across the board.",
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Efficient:"), "5th row will generally be the smallest (or very close to it) of all three models' fifth rows (for a very rough heuristic).  Additionally, 5th and 6th rows will match/be close for all columns."
    )
    
) #end fluidRow