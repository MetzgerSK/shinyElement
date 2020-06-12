jqui_draggable(
    bsModal("modal_expl", "What's going on here?", "explainButton", size = "large",
            
        h4("The Situation"),
        p( "You have a dataset whose points are displayed in blue. Your dependent
        variable's values are continuous (graph's vertical axis). You have one
        independent variable (horizontal axis), \\( x \\), which (arbitrarily) is
        distributed normally with a mean of 0 and standard deviation of 1."),
    
        
        h4("Your Oveararching Goal"),
        p( "You'd like to estimate a regression model using these data.  That
        means you'd like to use values of \\(x \\) to predict values of \\( y \\).
        Because your dependent variable is continuous, we've chosen to estimate a
        linear regression."),
    
        
        h4("What needs to happen to reach my overarching goal?"),
        HTML("<p>By default, R uses ordinary least squares to estimate linear
        models (see <code>leastSq</code> app).  However, if we're willing to
        assume the population error term is normally distributed, we can also use
        maximum likelihood estimation (MLE) to find the best-fitting line for a
        linear model.  Informally, R is going through all the possible intercept
        values, forms combinations with all the possible slope values for \\( x
        \\), and seeing which of the combinations is \"most likely\" responsible
        for generating the patterns you see in your dataset. (Note: R is not
        *actually* going through every slope-intercept combination.  It uses
        various optimization rules to hone in on the likely candidates
        quickly.)</p>"),
    
        HTML("<p>R is also taking the variance of the error term into account
        (\\(\\sigma^{2}\\)) when it makes its guesses. The slope and intercept tell
         us something about \\(y\\)'s mean.  However, they tell us nothing about
        \\(y\\)'s dispersion (\\(\\sigma\\)), which we know is also an important
        quantity if we're trying to characterize the values a variable, like
        \\(y\\), takes (i.e., its distribution).<sup><a id=\'fn1_ret'></a>
        <a href=\'#fn1\'>[1]</a></sup>"),
          
        p("Provided all the Gauss-Markov assumptions are met, we know \\(y\\)'s
        dispersion is equal to \\(u\\)'s dispersion.  We also know that, if
        \\(u\\) is normally distributed, then \\(y\\) will also be normally
        distributed.  When R checks various combinations of
        slope-intercept-\\(\\sigma\\) values, then, it's essentially asking \"A
        normal distribution with what mean and what standard deviation is most
        likely to have produced the \\(y\\) values I have in my dataset?\""),
        
        p("R isn't directing the search for the best slope, intercept, and
        \\(\\sigma\\) value, though.  ", strong("YOU"), "are."),
       
        HTML("<p>You have (an artificially limited range of) slope, intercept, and
        \\(\\sigma\\) combinations to check manually, using the sliders in the left
        sidebar.
            <ul> 
                <li>Left graph: The regression line corresponding to your current
                slope and intercept is the red line.</li> 
                <li>Right graph: Displays uHat's distribution.  The graph gives
                you a sense of how reasonable your guess is for \\(\\sigma\\),
                given the slope and intercept values you've chosen.  Reasonable
                choices will have the red dashed line (normal distribution)
                generally approximating the solid blue line ('observed' data,
                given your current slope-intercept guess).</li>
            </ul>
            The \"likeliness\" of seeing your data, given this slope, intercept,
            and \\(\\sigma\\) guess, is displayed under \"Current Point Total\".
            If you have the prose slider set to \"Formal\", it'll instead read
            \"Current log-likelihood value\". In \"Formal\" prose mode, you'll
            also see a small eyeball button where you can view the actual formula
            for the calculation producing that value, if you'd like.</p>"),
        
       
        h4("Your Immediate Task"),
        HTML("<p>Because you'd like to find the \"most likely\" slope, intercept,
        and \\(\\sigma\\) term that produced your data, your task is to
        <strong>find the combination that gives you the LARGEST
        \"point\"/likelihood value possible</strong>.  Be mindful of the
        likelihood's sign as you hunt: likelihood values are almost always
        negative (we log them to protect against really, really big numbers
        that'll eat up all the computer's memory and cause the program to
        crash).</p>"),
          
        p( "The largest likelihood value you've found so far is recorded under
        \"Current all-time best guess\"/\"Maximum log-likelihood value (so far)\".
        The orange \"Restore\" button will set the sliders to the slope,
        intercept, and \\(\\sigma\\) values associated with your best guess."),
    
        p( "After you hunt for a bit, clicking \"Show Answer\" will display the
        best slope-intercept-\\(\\sigma\\) combination that R found."),
    
       
       
        div(class="bs-callout bs-callout-info",
            HTML("<h4><i class='fas fa-lightbulb'></i> Suggestion</h4>"),
            HTML(
                "<p><ol>
                <li>Find the smallest log-likelihood value possible from changing the slope and intercept sliders 
                alone.</li>  
                <li>Shift to finding the best \\(\\sigma\\) value, given your slope and intercept from the previous step.</li>
                </ol>
                This informal procedure will get you pretty close to the truth, most of the time.
                (R doesn't need to break things into steps like this because it's a computer.<a id=\'fn2_ret'></a><sup><a href=\'#fn2\'>[2]</a></sup>
                The chunks are a heuristic to help us, the humans, out.) </p>") 
        ),
       
        hr(),
        HTML("<p style=font-size:80%;><a id=\'fn1\'></a>1: If the slope and intercept did influence \\(\\sigma\\)'s value, we'd have heteroskedasticity. <a href=\'#fn1_ret\'>&larrhk;</a></p>"),
        HTML("<p style=font-size:80%;><a id=\'fn2\'></a>2: Some more complex optimization algorithms do break optimization problems into 
            distinct steps.  The expectation&ndash;maximization algorithm is one example. <a href=\'#fn2_ret\'>&larrhk;</a></p>")
    )
)
