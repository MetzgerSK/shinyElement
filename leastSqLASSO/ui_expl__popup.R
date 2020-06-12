jqui_draggable(
    bsModal("modal_expl", "What's going on here?", "explainButton", size = "large",
        h4("The Situation"),                            
        p( "You have a dataset whose points are displayed in blue. Your dependent variable's values are continuous (graph's vertical axis).
            You have two independent variables.  \\(z\\) is a binary variable and \\( x \\) is a continuous variable, which (arbitrarily) 
            is distributed normally with a mean of 0 and standard deviation of 1. There is one graph for every unique \\(z\\) value, and
            \\(x\\) appears along the graphs' horizontal axis."),  
        
        h4("Your Oveararching Goal"),
        HTML( "<p>You'd like to estimate a regression model using these data.  That means you'd like to use values of \\(x \\) and \\(z\\)
            to predict values of \\( y \\).  Because your dependent variable is continuous, we've chosen to estimate a linear regression. However,
            you've opted to use the least absolute shrinkage and selection operator (LASSO).  (Usually, you would choose this when you have <em>many</em>
            potential covariates and would like to whittle down the list.  You wouldn't use LASSO with only two variables in practice, but we're doing
           it here so you can see how LASSO works.)</p>"),
        
        h4("What needs to happen to reach my overarching goal?"),
        HTML("<p>R uses a variant of ordinary least squares to estimate LASSO models.  It starts by standardizing all the variables and proceeds from there.
            Informally, R is going through all the possible \\(x\\) slope values, 
            forms combinations with all the possible slope values for \\( z \\), and seeing which of the combinations produces a line that best fits
            the data.  LASSO defines the best-fitting line as the one whose residuals, when squared, then summed yield the smallest possible value compared to
            all the other lines&mdash;same as OLS, up to this point.  However, LASSO then applies an additive penalty term to the sum of squares involving \\(\\lambda\\), 
            where the penalty will be bigger for lines with large slope coefficients and/or many estimated coefficients 
            
            $$ \\sum_{i=1}^n \\left( y_i-\\hat{\\alpha} - \\hat{\\beta}x_i \\right)^2 +
            \\lambda \\sum_{k=1}^{K} \\lvert \\beta_k \\rvert   $$
            
            </p>
            
            <p>The value of each data point's squared 
            residual is displayed as a gray-shaded box in the figure.  The LASSO line will be the one where the sum of these boxes' area, post-penalty, is as small as 
            possible, given the data.  (Note: R is not *actually* going through every combination of the two slopes.  It uses calculus to be smart about things.  You, as the user,
            select \\(\\lambda\\)'s value, as explained by the \"Wait, what's this?\" link in the left sidebar.)</p>"),
    
        p("R isn't directing the search for the best \\(\\beta_x\\) and \\(\\beta_z\\) value, though.  ", strong("YOU"), "are."),
        
        p("You have (an artificially limited range of) \\(x\\) slope 
            and \\(z\\) slope combinations to check manually, using the sliders in the left sidebar.  The regression line corresponding to your current 
            slopes is the red line in the graph.  The proposed line's penalized sum of squared residuals (SSR) is being 
            being displayed under \"Current Point Total\".  If you have the prose slider set to \"Formal\", it'll instead read \"Current Penalized SSR
            value\". In \"Formal\" prose mode, you'll also see a small eyeball button where you can view the actual formula for the calculation producing that 
            value, if you'd like."),
        
        h4("Your Immediate Task"),
        HTML("<p>Because you'd like to find the slopes that produce the smallest penalized SSR for your data, your task is to <strong>find 
            the combination that gives you the SMALLEST \"point\"/penalized SSR value possible</strong>.</p>"),
          
        p( "The lowest penalized SSR value you've found so far is recorded under \"Current all-time best guess\"/\"Smallest penalized SSR value (so far)\".
            The orange \"Restore\" button will set the sliders to the two slope values associated with your best guess."),
    
        p( "After you hunt for a bit, clicking \"Show Answer\" will display the best \\(\\beta_x\\)-\\(\\beta_z\\) combination that R found for your \\(\\lambda\\) value.")
        
    )
)