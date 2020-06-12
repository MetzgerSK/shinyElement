#*********************************************
# ALL GLOBAL FUNCTIONS (GENERIC)
#**************************

# Convenience call (note: b is useless--just input "1" or NULL when calling in practice.)
`%ss%` <- function(a, b=NULL) eval(parse(text=a))


# Function to manually insert tooltips for # subjects slider, 
# since bsTooltip's not injecting the appropriate script tags into the final HTML.
nObsBsTooltip <- function(stub){
    ifelse(stub=="", {xtra <- ""}, {xtra <- "_"})
           
    expr <- paste0('       
                tags$script("
                    $(document).ready(function() {
                        setTimeout(function() {
                            shinyBS.addTooltip(\'nObs', xtra, stub, '\', \'tooltip\', 
                                {\'placement\': \'bottom\', \'trigger\': \'hover\', \'title\': 
                                 \'Double click to change min/max.\'
                            })
                        }
                        , 500)
                    });
                ")'
            )
    
    eval(parse(text=expr))
}


# Function to manually insert tooltips for load checkbox, for same reason as above.
canLoadBsTooltip <- function(stub){
    ifelse(stub=="", {xtra <- ""}, {xtra <- "_"})
           
    expr <- paste0('       
                tags$script("
                    $(document).ready(function() {
                        setTimeout(function() {
                            shinyBS.addTooltip(\'canLoad', xtra, stub, '\', \'tooltip\',      
                                {\'placement\': \'bottom\', \'trigger\': \'hover\', \'title\': 
                                 \'', loadSavedTooltip, '\'
                            })
                        }
                        , 500)
                    });
                ")'
            )
    
    eval(parse(text=expr))
}


# Function to build bs-callout WITH close button
bsCallout <- function(title, content, class="info"){
    str <- paste0('
                    div(class="bs-callout bs-callout-', class, '",
                        div(class="bs-close", icon("times-circle")),
                        HTML("<h4>', title, '</h4>"),
                        HTML("<strong>', content, ' </strong>")
                    )
                  ')
    
    eval(parse(text=str))
}


# Function to convert results into row vector
reducer <- function(b, se){
    lm_b <- c(b)
    names(lm_b) <- paste0("b", 0:(length(lm_b)-1),sep="")
    lm_se <- c(se)
    names(lm_se) <- paste0("se", 0:(length(lm_se)-1),sep="")   
    
    return(c(lm_b,lm_se))
}


# Name checker (given selector's numeric ID, retrieve corresponding name)
nmChk <- function(i){
    idxLoc <- which(modList==i)
    nm <- names(modList)[idxLoc]
    
    return(nm)
}
