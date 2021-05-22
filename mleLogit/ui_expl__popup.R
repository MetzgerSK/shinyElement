jqui_draggable(
    bsModal("modal_expl", "What's going on here?", "explainButton", size = "large",
        h4("The Situation"),                            
        p( "You have a dataset whose points are displayed in blue. Your dependent variable's value is either 0 or 1 (graph's vertical axis).
            You have one independent variable (horizontal axis), \\( x \\), which (arbitrarily) is distributed normally with a mean of 0 and 
            standard deviation of 1."),  
        
        h4("Your Oveararching Goal"),
        p( "You'd like to estimate a regression model using these data.  That means you'd like to use values of \\(x \\)
            to predict values of \\( y \\).  Because your dependent variable is dichotomous, we've chosen to estimate a logit model for the  
            regression. (We could have also used probit.)"),
        
        h4("What needs to happen to reach my overarching goal?"),
        p(  "R uses maximum likelihood estimation (MLE) to find the best-fitting line for logit models (like many, if not all, of the major 
            statistical packages do for logit).  Informally, R is going through all the possible intercept values, forms combinations with all the 
            possible slope values for \\( x \\), and seeing which of the combinations is \"most likely\" responsible for generating the patterns
            you see in your dataset. (Note: R is not *actually* going through every slope-intercept combination.  It uses various optimization rules 
            to hone in on the likely candidates quickly.)"),
        
        p(  "R isn't directing the search for the best slope and intercept value, though.  ", strong("YOU"), "are."),
        
        p( "You have (an artificially limited range of) slope 
            and intercept combinations to check manually, using the sliders in the left sidebar.  The regression line corresponding to your current 
            slope and intercept is the red solid line in the graph.  The \"likeliness\" of seeing your data, given this slope and intercept, is what's 
            being displayed under \"Current Point Total\".  If you have the prose slider set to \"Formal\", it'll instead read \"Current log-likelihood 
            value\". In \"Formal\" prose mode, you'll also see a small eyeball button where you can view the actual formula for the calculation producing that 
            value, if you'd like."),
        
        h4("Your Immediate Task"),
        HTML("<p>Because you'd like to find the \"most likely\" slope and intercept that produced your data, your task is to <strong>find 
            the combination that gives you the LARGEST \"point\"/likelihood value possible</strong>.  Be mindful of the likelihood's sign as you hunt: 
            likelihood values are almost always negative (we log them to protect against really, really big numbers that'll eat up all the computer's 
            memory and cause the program to crash).</p>"),
          
        p( "The largest likelihood value you've found so far is recorded under \"Current all-time best guess\"/\"Maximum log-likelihood value (so far)\".
            The orange \"Restore\" button will set the sliders to the slope and intercept values associated with your best guess."),
    
        p( "After you hunt for a bit, clicking \"Show Answer\" will display the best slope-intercept combination that R found.")
    
    )
)