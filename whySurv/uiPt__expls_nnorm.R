# wrapped in fluidRow only b/c app kicks error otherwise
fluidRow(style="margin:0px;",
    conditionalPanel("input.model_nnorm == 1",
        h4("What should I see?"),
        "This is a naive model--as if you opened the dataset and ran an OLS regression using the duration, without transforming the duration at all.  The standard errors will be",em("inefficient"),"for the reasons noted above.  However, the estimates will also be ",em("biased"),"because the true DGP is non-linear in parameters.  OLS requires linearity in parameters to obtain unbiased estimates; see the OLS assumption simulation", HTML(paste0(a(href="http://www.shawnakmetzger.com/ols","here."))),
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Biased:"), "Top two rows won't match for all columns; top row won't fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Inefficient:"), "5th row's values will be larger than the Weibull's 5th row (for a very rough heuristic; the Weibull constitutes the 'No Violations' scenario)."
    ),
    conditionalPanel("input.model_nnorm == 2",
        h4("Why do non-normally distributed errors matter?"),
        paste(nonNormText),
        br(),br(),
        h4("What should I see?"),
        "We have transformed the duration by taking the natural log, which gets rid of the non-linearity in parameters.  As a consequence, the OLS estimates will now be", em("unbiased."), "The exception is the constant term, which will still be biased as OLS does not *explicitly* model the shape parameter.  (OLS's closest approximation is the RMSE, the inverse of which is reported here.)  However, OLS still assumes the stochastic error is distributed normally.  The standard errors will still be",em("inefficient."),
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Unbiased:"), "Top two rows should match/be close for all columns except intercept and shape; top row should fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Inefficient:"), "5th row's values will be larger than the Weibull's 5th row (for a very rough heuristic; the Weibull constitutes the 'No Violations' scenario)."
    ),
    conditionalPanel("input.model_nnorm == 3",
        h4("Why do non-normally distributed errors matter?"),
        paste(nonNormText),
        br(),br(),
        h4("What should I see?"),
        "The Weibull parametric duration model assumes non-normal errors.  Specifically, it assumes Type I Extreme Value (minimum) errors--the same as the simulation's true DGP.  As a result, the Weibull estimates will now be", em("unbiased"), "and ", em("efficient."),
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Efficient:"), "5th row will be smallest of all three models' fifth rows (for a very rough heuristic).  Additionally, 5th and 6th rows will match/be close for all columns."
    )
    
) #end fluidRow