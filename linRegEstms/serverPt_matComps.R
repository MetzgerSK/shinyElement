# GENERIC FUNCTION: SINGLE MATRIX EXTRACT
matReturn <- function(modID, matrix=NULL, coeffs=NULL){
    # Get as numeric
    modID <- as.numeric(modID)
    
    # Behavior depends on type
    ## Empirical
    if(is.null(coeffs)){
        # output the relv matrix
        mat <- matrix[[modID]]
     
    ## So-called theoretical       
    } else {
        # pull the coefficients, calc vcov
        mat <- coeffs[[modID]] %>% 
                data.frame %>% 
                dplyr::select(starts_with("b")) %>% 
                plyr::rename(c(b0="(Intercept)", b1="x1", b2="z"), 
                             warn_missing=FALSE) %>%
                cov     # variance-covariance calc
    }
    
    # return invisibly
    invisible(mat)
}

#*********************************************
# Create all the output functions 
for(i in 1:length(tabList)){
    # Pull stub
    stub <- tabList[i]
    
    # Get model labels
    ifelse(stub=="ac", {modL <- "modListAC"}, {modL <- "modList"})
    
    # If empty, means it's reg
    ifelse(stub=="", {stub <- "_reg"}, {stub <- paste0("_", stub)})
        
    # Build the expression
    expr <- paste0("
    
        # Based on current button selection, the requested VCE type for the mats
        mat_req", stub, " <- reactive({
            ## Force the update once the go button gets pushed
            if(stub==\'reg\')   input$goButton          # to deal with your poor naming convention choices
            else                input$goButton", stub,"
            
            ## The regular time to update
            if(input$vceSel", stub, "==1){
                mi <- matReturn(input$iMat", stub, ", data", stub, "()[[4]])
                mj <- matReturn(input$jMat", stub, ", data", stub, "()[[4]])
            } else {
                mi <- matReturn(input$iMat", stub, ", coeffs = data", stub, "()[[1]])
                mj <- matReturn(input$jMat", stub, ", coeffs = data", stub, "()[[1]])
            }
            
            # traces (for convenience)
            trI <- sum(diag(mi))
            trJ <- sum(diag(mj))
            
            return(list(mi, mj, trI, trJ))
        })
        
        output$matI", stub, " <- renderPrint({
            mat_req", stub, "()[[1]]
        })
        
        output$matJ", stub, " <- renderPrint({
            mat_req", stub, "()[[2]]
        })

        output$matDiff", stub, " <- renderPrint({
            cat('Trace of A:', mat_req", stub, "()[[3]], '\n')
            cat('Trace of B:', mat_req", stub, "()[[4]]      )
        })

        output$matImpl", stub, " <- renderPrint({
            trA <- mat_req", stub, "()[[3]] 
            trB <- mat_req", stub, "()[[4]] 

            if(trA<trB)         cat('A more efficient than B')
            else if(trA>trB)    cat('B more efficient than A')
            else                cat('A and B have same relative efficiency.\n',  
                                   '(This mostly occurs when viewing the \"theoretical\" VCEs,\n', 
                                    'for OLS vs. OLS + robust SEs or HAC SEs.)')
        })
        
        # Update the selector choices
        observeEvent(input$iMat", stub, ", {
            ## relevant input
            inp <- as.numeric(input$iMat", stub, ")
            
            ## Get currently selected option from other list (if it's the to-be
            ## removed object, default to first item in list)
            selJ <- ifelse(inp!=as.numeric(input$jMat", stub, "), 
                            as.numeric(input$jMat", stub, "),
                            ifelse(inp==1, 2, 1))   # if inp's OLS, go to 2nd item in list
        
            ## Remove this option from other list
            cListJ <- ", modL, "[-c(which(", modL, "==inp),which(", modL, "==6))]
            updateSelectInput(session, 'jMat", stub, "', choices=cListJ, selected=selJ)

        }, ignoreInit=TRUE)
    ")
    
    # Execute it
    eval(parse(text=expr))
    
}
