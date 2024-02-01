# wrapped in fluidRow only b/c app kicks error otherwise
fluidRow(style="margin:0px;",
    conditionalPanel("input.model_cens == 1",
        h4("What should I see?"),
        "From the 'Non-Normal' tab, we've already established that OLS with the untransformed 
        duration performs poorly because of non-linearity in parameters.  Right-censored data makes OLS' performance even worse because of OLS' inability to model right censoring properly.  As a consequence, the estimates will be", em("biased"), "and  ", em("inefficient."),
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Biased:"), "Top two rows won't match for all columns; top row won't fall in between values in 3rd and 4th rows for all columns.  Additionally, these estimates will also differ from the OLS estimates on the 'Non-Normal' tab (provided that all common slider/field values are identical).",
        br(),
        strong("Inefficient:"), "5th row's values will be larger than the Weibull's 5th row (for a very rough heuristic; the Weibull constitutes the 'No Violations' scenario)."
    ),
    conditionalPanel("input.model_cens == 2",
        h4("What should I see?"),
        "Despite our transformation of", em("t"), "to remove the non-linearity in parameters, these OLS estimates will still be ", em("biased"), "and  ", em("inefficient,")," because OLS's inability to properly model right-censored durations.",
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Biased:"), "Top two rows won't match for all columns; top row won't fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Inefficient:"), "5th row's values will be larger than the Weibull's 5th row (for a very rough heuristic; the Weibull constitutes the 'No Violations' scenario)."
    ),
    conditionalPanel("input.model_cens == 4",
        h4("What should I see?"),
        "Censored linear regression is a special type of linear regression that can handle right censoring.  Therefore, the estimates will now be", em("unbiased"), "except for the constant term, which will still be biased because OLS does not *explicitly* model the shape parameter.  However, censored linear regression assumes the errors are normally distributed, same as OLS (which is potentially problematic, see 'Non-Normal Errors' tab).  As a result, the standard errors will still be",em("inefficient."),
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Unbiased:"), "Top two rows should match/be close for all columns except intercept and shape; top row should fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Inefficient:"), "5th row's values will be larger than the Weibull's 5th row (for a very rough heuristic; the Weibull constitutes the 'No Violations' scenario)."
    ),
    conditionalPanel("input.model_cens == 3",
        h4("What should I see?"),
        "We already know the Weibull duration model assumes non-normal errors (TIEVmin, specifically; see 'Non-Normal Errors' tab).  In addition, though, all duration models can handle right-censored data by modeling it properly (i.e., not treating the censored observations as if they are observed failure times).  The Weibull is no exception.  As a result, the Weibull estimates will be both ", em("unbiased"), "and ", em("efficient."),
        br(),br(),
        h4("Where should I look to see that?"),
        strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns.",
        br(),
        strong("Efficient:"), "5th row will be smallest of all three models' fifth rows (for a very rough heuristic).  Additionally, 5th and 6th rows will match/be close for all columns."
    )
                 
) # end fluidRow