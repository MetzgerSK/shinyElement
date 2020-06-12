bsModal("modal_lambda", "What's \\(\\lambda \\)?", "benchLink", size = "medium",
    HTML("
        <div id = \"div_benchHL\">
            <p>\\(\\lambda \\) is LASSO's penalization parameter. It is an
            example of a hyperparameter.</p>   
         </div>
         <h4>What's that mean?</h4>
         <p> \\(\\lambda \\) dictates just how severely your model will be
         punished during the estimation process for having either (a) many
         estimated coefficients, (b) large coefficient sizes, or (c) both.</p>
         
         <p>For continuous \\(y\\)s, LASSO uses a modified form of least
         squares to find the best-fitting line.  The best-fit line will minimize 
         the sum of squared errors...after applying an additive penalty term 
         to \\(\\sum\\hat{u}^2 \\) involving the proposed coefficient values and
         \\(\\lambda\\). <code>leastSqLASSO</code> refers to this as
         <em>penalized least squares</em>, for convenience.  You may also hear
         this sum of squares + penalty quantity referred to as the value of 
         LASSO's <em>loss function</em>.</p>
         
         <h4>Why am I choosing a value <em>now</em>, though?</h4> <p>Unlike
         the intercept or slope coefficients, \\(\\lambda \\)'s value is not
         part of the estimation process.  Its value is set beforehand. The
         computer takes the \\(\\lambda \\) value as given, and finds the
         intercept and slopes with the smallest penalized sum of squares for
         this \\(\\lambda \\). Parameters whose values are set before
         estimation are known as <em> hyperparameters</em>.  They appear most
         frequently in discussions of Bayesian models.</p>
         
         <h4>Why not try out multiple \\(\\lambda\\) values?</h4>
         <p>If we wanted to automatically run over a large list of \\(\\lambda\\)
         values when we estimated the model (in an
         appropriate fashion), we could perform <em>cross-validation</em> 
         using <code>glmnet::cv.glmnet()</code> for LASSO.</p>")
)
