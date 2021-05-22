#*********************************************
# ALL GLOBAL STATIC TEXT
#**************************

# Common text: explanations
noViolTooltip <- 'Copy the parameter values from the "No Violation" scenario to this scenario.'
simulateBriefInstrs <- "Set values at left, click 'Simulate!' button to run, and wait 15-90 seconds for results to appear."
noise_endSim <- "Constrained to 1 to help simplify the data generation."


# checkbox text expl for loading saved res
loadSavedTxt <- "Load saved results if seeds don't match, but all else does (including # of sim draws)."
loadSavedTooltip <- "This scenario may have saved results, but for a different seed.  New simulations take a minute or two to run.  Check this box to change your seed to match a set of already saved results, if they exist.  Leave unchecked to keep your seed and run new simulations, if needed."


# Table key row builder
tabKeyRow <- function(lin, abbr, text){
    
    # Start building the string
    str <- "<p class='tabKey'>"
    
    # Get lin vs. non-lin estm
    if(lin=="lin"){
        labTy <- "success"
        ttip <- "Belongs to class of linear estimators"
        desc <- "Linear"
    } else{
        labTy <- "warning"
        ttip <- "Belongs to class of non-linear estimators"    
        desc <- "Non-Linear"
    }
    
    # Insert badge code
    str <- paste0(str, "<span class='label label-", labTy, 
                        "' data-tooltip='", ttip, "' data-tooltip-position='left'>",
                        desc, "</span> ")
    
    # Insert model abbreviation
    str <- paste0(str, "<strong>", abbr, "</strong>: ")
    
    # Insert longer description, end para
    str <- paste0(str, text, "</p>")
    
    # Return
    return(str)
}


# Table keys
tableKeyBase <- {
    str <- paste0("<h4>Table Key</h4>",
                  tabKeyRow("lin", "OLS",    
                            "Ordinary least squares (OLS), regular standard error (SE) estimates"),
                  tabKeyRow("lin", "Rob.SE", 
                            "OLS, heteroskedasticity robust SE estimates 
                              <em>(synonyms: White SEs, Huber SEs, Eicker SEs, or some hyphenated combination)</em>"),
                  tabKeyRow("lin", "HAC.SE", 
                            "OLS, heteroskedasticity- and autocorrelation-consistent SE estimates 
                              <em>(synonyms: Newey&ndash;West SEs)</em>"),
                            "BASEBASEBASE",
                  tabKeyRow("nlin", "M.estim", 
                            "robust regression (<em>M</em> estimation using Huber's objective function)"),
                  tabKeyRow("nlin", "LTS", 
                            "least trimmed squares")
    )
    HTML(str)
}

    ## Main variant (double \\s to deal with gsub)
    tableKey <- gsub("BASEBASEBASE",
                    paste0( tabKeyRow("lin" , "WLS" , 
                                      "Weighted least squares, with weights equal to \\\\( \\\\lvert x \\\\rvert^{-1} \\\\)"),
                            tabKeyRow("nlin", "FGLS", 
                                      "Feasible generalized least squares 
                                        <em>(synonym: estimated/empirical generalized least squares [EGLS])</em>")
                          ),
                    tableKeyBase
                )

    ## AC variant
    tableKeyAC <-
                gsub("BASEBASEBASE",
                    paste0( tabKeyRow("lin" , "GLS" , 
                                      "Generalized least squares, using AR(1) and the true AC coefficient 
                                        <em>(impossible to do with unknown AC coefficient, in practice)</em>"),
                            tabKeyRow("nlin", "FGLS", 
                                      "Feasible generalized least squares using AR(1) 
                                        <em>(synonym: estimated generalized least squares [EGLS])</em>")
                          ),
                    tableKeyBase
                )