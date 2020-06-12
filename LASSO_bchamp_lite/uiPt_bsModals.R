fluidRow(style="margin:0px",  # Needs to be wrapped in this for both modals to work.
    
bsModal("modal_bench", "Why's this relevant?", "benchLink", size = "large",
    HTML("<div id = \"div_benchHL\">
            <p>Beauchamp makes a point of discussing the various benchmarks we can use 
            to judge a model's predictive effort (2017, 491&ndash;493).</p>   
         </div>"),    
    
    HTML("
        <h4 style='margin-top:20px;'>&#9654; In-Sample Polls Predicting Out-of-Sample Polls</h4>
    
        <p> Usually, we assess performance using the <em>same dependent variable</em> as the model itself.  On the set of cases
        with full data, we split the sample, estimate the model on one subsample, and then assess the model's performance
        using the other subsample.</p>

        <p>In this example, we would split the sample of states with polling data, estimate a model 
        in which we regress (e.g., using OLS, LASSO, ridge regression...) the Twitter data on the state's poll on one group of states
        (\"in sample\") and then use the model's estimates to generate poll predictions for the other, excluded subsample of states
        (\"out of sample\").  We then look to see how the poll predictions compare with the actual polling data within
        the out-of-sample group.  Beauchamp does use this comparison as one of his benchmarks in the full paper.</p>


        <h4 style='margin-top:20px;'>&#9654; In-Sample Polls Predicting Election Results</h4>

        <p>However, with polling data, we have another viable benchmark: looking to see how well the polls predict <em>the actual
        election results</em>.  (This is the entire point of polling, after all.)  This particular benchmark is what this app
        focuses on, for illustrative purposes.</p>")
),

# Pop-up window with explanation about what's going on
bsModal("modal_expl", "Is this what Beauchamp actually does?", "explainButton", size = "large",
    h4("Short Answer"), 
    p("No, he doesn't use classic LASSO. He uses a more sophisticated model that performs better (which is one of his major contributions)."),
      
    h4("Longer Answer", style="margin-top:20px;"),   
    HTML("<p>The LASSO model that appears after clicking 'Show Replication' is one of the comparison models Beauchamp runs, as a benchmark for his 
    preferred model's performance.  His preferred model starts from the same penalization term as LASSO (the sum of the coefficients' absolute value [L1 penalty]),
    but then has additional features.  For one, his preferred model includes state fixed effects and time fixed effects, to take advantage of his time-series, cross-sectional data structure. 
    He also uses a larger ensemble of individual OLS models to help determine his model's equivalent of \\( \\lambda \\).  For more details, see Beauchamp 2017, 495&ndash;496.</p>"),
    
    h4("Attribution", style="margin-top:20px;"), 
    HTML("<p>All of the app's computation code comes directly from <code>elasticnet_predictallstates.R</code> in Beauchamp's replication files.  The only modifications are Shiny related.</p>")
)


)