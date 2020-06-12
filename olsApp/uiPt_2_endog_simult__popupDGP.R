# To cut down on repetitiveness later
linCombo <- function(x){
    res <- "\\beta_{RRR_0} + \\beta_{RRR_1}z_1 + \\beta_{RRR_2}z_2 + \\cdots + \\beta_{RRR_k}z_k + \\sigma_RRR \\epsilon_RRR"
    return(gsub("RRR", paste(x), res))
}

gamma <-  "\\left( \\alpha_1 + \\alpha_2 - \\alpha_1\\alpha_2 \\right) ^{-1}"

bsModal("modal_simultDGP", "Why does this DGP look so complicated?", "explain_simultDGP", size = "large",

    withMathJax(),
    h4("The Problem: Generating \\(y\\)'s Value"),
    HTML("<p>If \\( y \\)'s (and \\( x \\)'s) value depends on the <em>other</em> variable's value,
    we have a chicken-egg problem: it is impossible to know \\(y\\)'s value without knowing \\(x\\)'s
    value, but we cannot know \\(x\\)'s value without knowing \\(y\\)'s value.  How could we possibly
    generate simulated data, given these characteristics?</p>"),
    
    br(),
    h4("The Solution: Isolate the Endogenous and Exogenous Variables"),
    HTML("<p>The general idea behind generating such data is simple: transform the two DGPs on the main tab so that all
    the <em>endo</em>genous variables are on one side of the equals sign and all the <em>exo</em>genous variables
    are on the other.\\(^{[1]} \\)  Performing these transformations is less simple, because it isn't immediately evident
    how we could solve for the two equations' 'endogenous' variables in a way that would be helpful.  (These two equations
    are known as <strong>structural equations</strong>, in this context) </p>"),
    HTML("<p>The key is conceiving of the two structural equations as constituting a single <em>system</em> of equations.  We can then express the
    equations using matrices, and can then solve for our 'unknowns' of interest (here, the value of \\( y \\) (and \\( x \\))).
    Expressing the system of equations in such a way is known as the <strong>reduced-form</strong> equation or expression (Wooldridge 2013, 524-525).</p>"),
    
    br(),
    h4("Solving the System"),
    p("Begin by considering the structural equations--the two DGPs that express how the value of each variable comes about.  Written
    generically, we have:"),
        HTML(paste0("<p style=margin-left:20px;>
          \\( \\begin{align} y &= \\alpha_1 x +", linCombo(1), "\\\\   
                             x &= \\alpha_2 y +", linCombo(2), "
              \\end{align} 
          \\)
        </p>")), 
    p("where the \\(z\\)s denote \\(k \\) other, exogenous regressors.\\(^{[2]} \\)"),
    
    p(style="margin-top: 20px;", "Moving toward putting these equations in matrix form, a first-cut one-for-one translation would look something like:"),
        HTML("<p style=margin-left:20px;>
         \\( \\begin{bmatrix} y \\\\ x \\end{bmatrix} = 
                \\begin{bmatrix} \\alpha_1 \\\\ \\alpha_2 \\end{bmatrix} \\begin{bmatrix} x \\\\ y \\end{bmatrix} +
                \\begin{bmatrix} \\beta_{1_0} \\\\ \\beta_{2_0} \\end{bmatrix} + 
                \\begin{bmatrix} \\beta_{1_1} & \\beta_{1_2} & \\cdots & \\beta_{1_k} \\\\ \\beta_{2_1} & \\beta_{2_2} & \\cdots & \\beta_{2_k} \\end{bmatrix} 
                    \\begin{bmatrix} z_1 & z_2 & \\cdots & z_k \\end{bmatrix} +
                \\begin{bmatrix} \\sigma_1 \\\\ \\sigma_2 \\end{bmatrix} \\begin{bmatrix} \\epsilon_1 \\\\ \\epsilon_2 \\end{bmatrix}
         \\)</p>"),

    HTML("<p>This is closer to where we have to be, but there are three problems with the expression.  
            <ol>
                <li>The order in which \\(x \\) and \\(y \\) appear are different on the left 
                    and right sides.  We need them to appear in the same order on both sides in order to 
                    eventually solve for this matrix.</li> 
                <li>All our multiplication terms involve non-conformable matrices.</li>
                <li>Even we spot-fixed the conformability issues by transposing as needed, we wouldn't end up with
                our original structural equations if we multiplied out these matrices--a problem, to say the least.</li>
            </ol>"),  
    
    p(style="margin-top: 20px;", "To solve all three issues at once, we can exploit the fact that anything times zero is zero, even for 
      matrix multiplication.  We can use this fact to express \\(x \\) and \\(y \\)'s order within the right-hand side matrix in the same way as their order 
      in the left-hand side matrix.  We can use the same trick for the error terms. If we strategically add zeros to 
      the current \\( \\alpha \\) and \\( \\sigma \\) matrices (plus transpose the \\(z \\) matrix to make it conformable), we obtain:"),
    
        HTML("<p style=margin-left:20px;>
         \\( \\begin{bmatrix} y \\\\ x \\end{bmatrix} = 
                \\begin{bmatrix} 0 & \\alpha_1 \\\\ \\alpha_2 & 0 \\end{bmatrix} \\begin{bmatrix} y \\\\ x \\end{bmatrix} +
                \\begin{bmatrix} \\beta_{1_0} \\\\ \\beta_{2_0} \\end{bmatrix} + 
                \\begin{bmatrix} \\beta_{1_1} & \\beta_{1_2} & \\cdots & \\beta_{1_k} \\\\ \\beta_{2_1} & \\beta_{2_2} & \\cdots & \\beta_{2_k} \\end{bmatrix} 
                    \\begin{bmatrix} z_1 \\\\ z_2 \\\\ \\vdots \\\\ z_k \\end{bmatrix} +
                \\begin{bmatrix} \\sigma_1 & 0 \\\\ 0 & \\sigma_2 \\end{bmatrix} \\begin{bmatrix} \\epsilon_1 \\\\ \\epsilon_2 \\end{bmatrix}
         \\)</p>"),
    
    p("If we multiply out these matrices, we successfully get back to our two structural equations."),
    
    p(style="margin-top: 20px;","We can further simplify this expression by condensing the two \\(\\beta \\) matrices into one, 
      as well as the constant and the \\(z\\)s into one matrix, to obtain:"),
      
         HTML("<p style=margin-left:20px;>
         \\( \\begin{bmatrix} y \\\\ x \\end{bmatrix} = 
                \\begin{bmatrix} 0 & \\alpha_1 \\\\ \\alpha_2 & 0 \\end{bmatrix} \\begin{bmatrix} y \\\\ x \\end{bmatrix} +
                \\begin{bmatrix} \\beta_{1_0} & \\beta_{1_1} & \\beta_{1_2} & \\cdots & \\beta_{1_k} \\\\ \\beta_{2_0} & \\beta_{2_1} & \\beta_{2_2} & \\cdots & \\beta_{2_k} \\end{bmatrix} 
                    \\begin{bmatrix} 1 \\\\ z_1 \\\\ z_2 \\\\ \\vdots \\\\ z_k \\end{bmatrix} +
                \\begin{bmatrix} \\sigma_1 & 0 \\\\ 0 & \\sigma_2 \\end{bmatrix} \\begin{bmatrix} \\epsilon_1 \\\\ \\epsilon_2 \\end{bmatrix}
         \\)</p>"), 
    
    p("(Again, notice how multiplying out these matrices will produce our original structural equations.)"),
    
   
    p(style="margin-top: 20px;","At this point, we can solve for \\(x \\) and \\(y \\).  (Remember, we want to do this so that we can generate values of these two variables.
    We can only generate the variables' values if we know the expressions producing said values.)
    If we substitute in symbols to our previous expression, it becomes easier to see that we can now solve out."),
    p("Let \\( \\text{Q} = \\begin{bmatrix} y \\\\ x \\end{bmatrix} \\) (to avoid clashes with our usual notational conventions).  If we use bold 
    letters to denote matrices and regular letters to denote vectors:"),
        HTML("<p style=margin-left:20px;>
             \\( \\text{Q} = \\mathbf{A}\\text{Q} + \\mathbf{B}\\text{Z} + \\mathbf{S} \\text{U} \\)</p>"), 
    
    p("The task before us is now clear: solve for \\( \\text{Q} \\)."), 
    
        HTML("<p style=margin-left:20px;>
             \\( \\begin{align}
                \\text{Q} &= \\mathbf{A}\\text{Q} + \\mathbf{B}\\text{Z} + \\mathbf{S} \\text{U}  \\\\
                \\text{Q} -  \\mathbf{A}\\text{Q} &= \\mathbf{B}\\text{Z} + \\mathbf{S} \\text{U}  \\\\
                \\text{Q} \\left( \\mathbf{I} - \\mathbf{A} \\right) &= \\mathbf{B}\\text{Z} + \\mathbf{S} \\text{U}  \\\\
                \\text{Q} &= \\frac{\\mathbf{B}\\text{Z} +  \\mathbf{S} \\text{U}}{\\left( \\mathbf{I} - \\mathbf{A} \\right)}
             \\end{align} \\)</p>"),

    p(style="margin-top: 20px;", "If we want to notationally simplify further, purely to drive home that we now know what \\(x \\) and \\(y \\) are equal to,
       let \\( \\mathbf{\\Gamma} = \\left( \\mathbf{I} - \\mathbf{A} \\right)^{-1} \\).  If so:"),

        HTML("<p style=margin-left:20px;>
            \\(
                    \\text{Q} = \\mathbf{\\Gamma} \\left(\\mathbf{B}\\text{Z} + \\mathbf{S} \\text{U} \\right)
            \\)</p>"),
    p("This is the expression we can use to generate simulated data in which \\(x\\) and \\(y\\) simultaneously affect one another."),

    p(style="margin-top: 20px;","Finally, if we want to come full circle and return to algebra, we can express the reduced-form matrix expression as
      two separate reduced-form algebraic equations.  \\(y \\) and \\(x \\) are equal to:"),

    HTML(paste0("<p style=margin-left:20px;>
         \\( \\begin{align} y &= \\left[ ", gamma, "\\right]
                                    \\left[ \\left(", linCombo(1), "\\right) \\\\
                                    + \\alpha_1 \\left(", linCombo(2), "\\right)
                                    \\vphantom{", gamma,"}
                                 \\right] \\\\
                            x &= \\left[ ", gamma, "\\right]
                                    \\left[ \\left(", linCombo(2), "\\right) \\\\
                                    + \\alpha_2 \\left(", linCombo(1), "\\right)
                                    \\vphantom{", gamma,"}
                                 \\right] \\\\
             \\end{align}
         \\)
        </p>")),
    
    hr(),
    HTML("<p style=font-size:80%;>1: Remember that an <strong>endogenous variable</strong> is one whose values depend on the
      value of other variable(s) in our model.  \\(y\\), how we usually denote a dependent variable, is the most common example.  By contrast, an 
      <strong>exogenous variable</strong> is one whose values do not depend on those of other variable(s) in our model.  All standard regression
      models assume \\(x\\), our 
      usual notation for an independent variable, is exogenous.  By this, we mean \\(x \\)'s value is either (a) randomly determined (e.g., random
      assignment to treatment groups in an experiment) or (b) determined by factors external to the process being theorized about, meaning that it is
         uncorrelated with \\( y \\)'s error term.</p>"),
    
    HTML("<p style=font-size:80%;>2: If we eventually want to estimate \\(x \\)'s effect on \\(y\\), there will need to be an instrument in \\(x \\)'s 
          equation, a variable
          whose only effect on \\(y\\) is through \\(x\\).  We can then proceed using two-stage least squares (2SLS) or limited information maximum likelihood (LIML).
          If we also wanted to get estimates for the parameters in \\(x\\)'s equation, we would also need an instrument in \\(y\\)'s equation.  We'd then
          estimate the two equations jointly using three-stage least squares (3SLS) or full information maximum likelihood (FIML).
          
          </p>")
    
)
