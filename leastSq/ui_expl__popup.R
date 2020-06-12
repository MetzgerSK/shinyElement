jqui_draggable(
    bsModal("modal_expl", "What's going on here?", "explainButton", size = "large",
        h4("The Situation"),                            
        p( "You have a dataset whose points are displayed in blue. Your dependent variable's values are continuous (graph's vertical axis).
            You have one independent variable (horizontal axis), \\( x \\), which (arbitrarily) is distributed normally with a mean of 0 and 
            standard deviation of 1."),  
        
        h4("Your Oveararching Goal"),
        p( "You'd like to estimate a regression model using these data.  That means you'd like to use values of \\(x \\)
            to predict values of \\( y \\).  Because your dependent variable is continuous, we've chosen to estimate a linear regression."),
        
        h4("What needs to happen to reach my overarching goal?"),
        p( "By default, R uses ordinary least squares to estimate linear models.  Informally, R is going through all the possible intercept values, 
            forms combinations with all the possible slope values for \\( x \\), and seeing which of the combinations produces a line that best fits
            the data.  OLS defines the best-fitting line as the one whose residuals, when squared, then summed, yield the smallest possible value compared to
            all the other lines \\( \\left( \\sum_{i=1}^n \\left( \\hat{u}_i^2 \\right) = \\sum_{i=1}^n \\left( y_i-\\hat{\\alpha} - \\hat{\\beta}x_i \\right)^2 \\right) \\).  
            (Note: R is not *actually* going through every slope-intercept combination.  It uses calculus to be smart about things.)  The value of each data point's squared 
            residual is displayed a gray-shaded box in the figure.  The OLS line will be the one where the sum of these boxes' area is as small as possible, given the data."),
    
        p(  "R isn't directing the search for the best slope and intercept value, though.  ", strong("YOU"), "are."),
        
        p("You have (an artificially limited range of) slope 
            and intercept combinations to check manually, using the sliders in the left sidebar.  The regression line corresponding to your current 
            slope and intercept is the red line in the graph.  The proposed line's sum of squared residuals (SSR) is being 
            being displayed under \"Current Point Total\".  If you have the prose slider set to \"Formal\", it'll instead read \"Current SSR
            value\". In \"Formal\" prose mode, you'll also see a small eyeball button where you can view the actual formula for the calculation producing that 
            value, if you'd like."),
        
        h4("Your Immediate Task"),
        HTML("<p>Because you'd like to find the slope and intercept that produces the smallest SSR for your data, your task is to <strong>find 
            the combination that gives you the SMALLEST \"point\"/SSR value possible</strong>.</p>"),
          
        p( "The lowest SSR value you've found so far is recorded under \"Current all-time best guess\"/\"Smallest SSR value (so far)\".
            The orange \"Restore\" button will set the sliders to the slope and intercept values associated with your best guess."),
    
        p( "After you hunt for a bit, clicking \"Show Answer\" will display the best slope-intercept combination that R found.")
        
    )
)