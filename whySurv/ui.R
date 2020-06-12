library(shiny)
library(DT)
library(Rcpp)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

# STATIC TEXT ----
nonNormText <- "OLS assumes normally distributed errors for hypothesis testing
                purposes.  If we don't have them, and we incorrectly assume we do, the
                estimates will be **inefficient**.  An estimator that can handle the
                non-normal errors will always be more efficient, asymptotically (Greene 2012,
                73-75)."
censText <- "Censoring is one of the most frequently mentioned reasons for
             OLS' inappropriateness.  A (right-)censored duration is one where a subject
             does not fail before our observation period ends.  As a result, we do not
             observe their actual failure time.  Instead, we only know they survived *up
             to* the end of our observation period.  Right censoring creates a problem for
             OLS because the estimator cannot handle it.  Instead, OLS treats all subjects
             as failing at the time we record, which is clearly not true." 
simInstrs <- "Set values at left, click 'Simulate!' button to run, and wait
              3-15 seconds for results to appear (see loading bar at upper or lower right)."

# > UI START ----
ui <- fluidPage(
    
    # Loading up the CSS
    includeCSS("style.css"),

    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on
    
    navbarPage("Why Duration Models?",                 
               
        # NON-NORMAL ERRORS ====
        tabPanel("Non-Normal Errors", 
            source("uiPt_nnorm.R", local=TRUE)$value
        ),
        
        # CENSORING ====
        tabPanel("Right Censoring",
            source("uiPt_cens.R", local=TRUE)$value
        ),

        # Footer info ====
        footer = column(12, align="center", br(), 
                       HTML(paste(strong("Author: "), a(href="http://www.shawnakmetzger.com", "Shawna K. Metzger,"),  
                                  a(href="mailto:shawna@shawnakmetzger.com", "shawna@shawnakmetzger.com"))), br(),
                       HTML("<em>Using Shiny to Teach Econometric Models</em>, Cambridge University Press"))
    )
    
)
