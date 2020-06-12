# Libraries
## (server)
library(shiny)
library(DT)
library(qqplotr)
library(plotly)
library(RColorBrewer)
library(magrittr)
library(forecast)
library(MASS)
library(shinyWidgets)
library(VGAM)
library(lavaan)

## (unique to ui, not already in server's list)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)

#****************************************************************************
# Residual Plot UI: residual type selector
residChoices       <- list("Regular"="reg", "Standardized"="stand", "Studentized"="stud")

# list of column headers for DT (common to all sessions)
DT.header.reg <- c("aHat", "b1Hat", "b2Hat", "se.aHat", "se.b1Hat", "se.b2Hat")
DT.header.end <- c("aHat", "b1Hat", "se.aHat", "se.b1Hat")

# list of all tabs' input suffixes
tabList <- c("", "end", "ar", "endSim", "endMeasErrX", "endMeasErrY",
             "ac", "hetero", "nlin", "heavy", "skewed", "mmodal")

# Convenience call (note: b is useless--just input "1" when calling in practice.
`%ss%` <- function(a, b=NULL) eval(parse(text=a))
 
# function to manually insert tooltips for # subjects slider, since bsTooltip's not injecting the appropriate script tags into the final HTML.
nObsBsTooltip <- function(stub){
    ifelse(stub=="", {xtra <- ""}, {xtra <- "_"})
           
    expr <- paste0('       
                tags$script("
                $(document).ready(function() {setTimeout(function() {shinyBS.addTooltip(\'nObs', xtra, stub, '\', \'tooltip\', {\'placement\': \'bottom\', \'trigger\': \'hover\', \'title\': \'Double click to change min/max.\'})}, 500)});
                ")'
            )
    
    eval(parse(text=expr))
}
#****************************************************************************
# Explanations: Common text
## E(u|x) vs E(u) note
techSideNoteEu0 <- HTML("<em>Technical side note:</em> \\( E \\left( u \\right) =0 \\) only guarantees \\( \\text{Corr} \\left( x,u \\right) 
                                                             = 0 \\) when covariates are nonstochastic (Gujarati 2003).  The simulation uses stochastic covariates.")

## No violation tooltip text
noViolTooltip <- 'Copy the parameter values from the "No Violation" scenario to this scenario.'

## Caption for y vs. yHat graph
yVsyHatCapt <- "* when \\( z = 0\\)"

## Choices in dropdown for diagnostic plot tab
plotErrDistChoices <- c("Histogram + Kernel" = "hist", 
                        "Q-Q Plot"="qq", "P-P Plot"="pp", 
                        "Resids vs. Predictor" = "hetero",
                        "Autocorrelation Function" = "acf", "Partial AC Function" = "pacf")

## Explaination for "WSIS" tab, all non-normal error distro pages
nonNormErrExpl <- fluidPage(
    h4("What happens to OLS estimates when the error's not normally distributed?"),
            HTML("<p>A normally distributed error is not a Gauss-Markov assumption, but we make it for inference purposes.  It also guarantees
                that OLS is the best unbiased estimator (BUE), instead of only being the best <em>linear</em> unbiased estimator, which is 
                what the GMT assumptions guarantee.
               (<a href='http://dx.doi.org/10.1017/psrm.2018.34' target='_blank'>Baissa and Rainey 2020</a>).  Formally, we assume that
                 the error is normally distributed, once conditioned on the covariates.</p>"),
           
            HTML("<p>If the error is not normally distributed, provided the error still has a mean of 0, the OLS estimates will still
                be <em>unbiased</em> but their <em>precision</em> will be affected.   (OLS is still BLUE, so the estimates aren't 
                technically 'inefficient.'  However, they will no longer be BUE.)  The estimates' sampling distibutions will also be <em>non-normal</em>.
                As <em>n</em> increases, the Central Limit Theorem will kick in,
                producing estimates and standard errors that behave more like the 'No Violations' tab, as well as estimate sampling distributions that
                are normally distributed.  How big <em>n</em> has to be
                depends on the way in which the error's non-normally distributed.  Errors that appreciably deviate from normal (e.g., multi-modal
                errors, instead of merely heavy-tailed errors) will require larger samples before CLT kicks in. </p>"),
           
            h4(class="WSIS", "Where should I look to see that?"),
            h5("On the 'Main' tab..."),
            p(strong("Unbiased:"), "Top two rows should match/be close for all columns; top row should fall in between values in 3rd and 4th rows for all columns."),       
            p(strong("Precision Loss:"), "Fifth row's values won't match No Violations' 5th row (provided all common slider/field values are identical)."),
            h5("On the 'Estimates: Distribution Plots' tab..."),
            p(strong("Non-normal sampling distributions"), HTML("Not all the sampling distributions will be normally distributed for small <em>n</em>.  The smoothed 
            density (solid navy line) will differ from the normal distribution (dashed red line).  Use the sampling distributions on the No Violations tab
            for the same <em>n</em> and common sliders to help see the differnces."))
)