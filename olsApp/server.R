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
 
# (contents of global.R get loaded automatically by shiny behind the scenes)

# begin server function 
server <- function(input, output, session){
    
    #***************************************************************
    # >> DGP EQS (MathJax) ----
    #*********************
    source("serverPt_eqs.R", local=TRUE)
    
    
    #***************************************************************
    # >> MC SIM FUNCTION ----
    #*********************	
    ## Used Francis Smart's blogpost on the R simulations to get skeleton
    # http://www.econometricsbysimulation.com/2012/12/easy-monte-carlo-sampler-command.html
    
    MC_easy <- function(dgp, estimator, spec, obs, reps, seed) {
        withProgress(message = 'Running Simulations', value=0, min=1, max=reps, {
            # Set seed
            set.seed(seed)
            
            # Create empty holder objects
            MC_results <- NULL
            modObjs <- NULL
            trueX <- NULL
            
            # Loop
            for (i in 1:reps) {
                # Advance counter
                incProgress(1, detail = paste0(i, " of ", reps)) 
                
                # Pull data
                temp_data <- get(dgp)(obs)
                
                # Estimate model
                returned <- get(estimator)(temp_data, spec)
                
                # Store results
                MC_results[[i]] <- returned[[1]]
                modObjs[[i]] <- returned[[2]]
                trueX[[i]] <- ifelse("xTrue" %in% colnames(temp_data), 
                                 data.frame(temp_data$xTrue),
                                 data.frame(temp_data$x1))

            }
            
            # Return raw sim output + model objects + true X values (b/c of measErr tab)
            return(list(do.call(rbind,MC_results), modObjs, data.frame(trueX)))

        })
    }
    
    
    #***************************************************************
    # >> DGP FUNCTIONS ----
    #*********************************
    source("serverPt_dgps.R", local=TRUE)
    
    
    #***************************************************************
    # >> MODEL ESTIMATION ----
    #*********************	
    OLS_est <- function(temp_data, spec) {
        lm <- lm(spec, data=temp_data)
        lm_coef <- coef(summary(lm))
        
        # reduce lm_coef to a single vector with values for each b and se
        lm_b <- c(lm_coef[,1])
            names(lm_b) <- paste0("b",0:(length(lm_b)-1))
        lm_se <- c(lm_coef[,2])
            names(lm_se) <- paste0("se",0:(length(lm_se)-1))

        return(list(c(lm_b,lm_se), lm))
    }
    
    
    #******************************************
    ## >> DATA OUTPUTS ----
    #*********************
    ## (( See headers, defined in global.R: DT.header.reg, DT.header.end ))
    
    source("serverPt_datOut.R", local=TRUE)
    
    
    #******************************************
    ## >> GRAPHS ----
    #*********************
    source("serverPt_graphFuncts.R", local=TRUE)
    source("serverPt_graphOutputs.R", local=TRUE)
    
    
    #***************************************************************
    # >> RESULT PRINTER ----
    #*********************
    # Col builder (rows = param statistics -> building cols by stacking rows)
    colBuilder <- function(true, ptEst, se){
        row <- rbind("**True Value**"           = true,
                       "Estimated Value (Mean)" = mean(ptEst),
                       "Lower 95% CI"           = quantile(ptEst, .025),
                       "Upper 95% CI"           = quantile(ptEst, .975),
                       "Estimated SE (Mean)"    = mean(se),
                       "StDev of Estimate"      = sd(ptEst))
        
        return(row)
    }

    # Table builder (cols = params, rows = param statistics)
    printMCs <- function(aHat, b1Hat, b2Hat, ptEstA, ptEstB1, ptEstB2, seA, seB1, seB2) {
                    
        text <- cbind(colBuilder(aHat,  ptEstA,  seA ),  # For the intercept
                      colBuilder(b1Hat, ptEstB1, seB1),  # For x
                      colBuilder(b2Hat, ptEstB2, seB2)   # For z
                     )
            
        colnames(text)<- c("Intercept (aHat)",
                           "x's Coeff (b1Hat)",
                           "z's Coeff (b2Hat)")
        
        return(text)
    }
    
    # Faster to just write separate function for OV case
    printMCs_end = function(aHat, b1Hat, ptEstA, ptEstB1, seA, seB1) {
        
        text <- cbind(colBuilder(aHat,  ptEstA,  seA ),  # For the intercept
                      colBuilder(b1Hat, ptEstB1, seB1)   # For x
                     )
            
        colnames(text)<- c("Intercept (aHat)",
                           "x's Coeff (b1Hat)")
        
        return(text)

    }
    
    
    #************************************************
    ## >> PARAM SLIDER VALUE CHNGS ----
    #*********************
    ## Randomly pick values for the parameters ("No Viol" tab only)
    observeEvent(input$randPars, {
        updateSliderInput(session, "aHat" , value=sample(-20:20, 1)/4)
        updateSliderInput(session, "b1Hat", value=sample(-20:20, 1)/4)
        updateSliderInput(session, "b2Hat", value=sample(-20:20, 1)/4)
        updateSliderInput(session, "noise", value=sample(1:10, 1))
        
    }) 
    
    ## Reset to the defaults
    observeEvent(input$reset, {
        reset("aHat")
        reset("b1Hat")
        reset("b2Hat")
        reset("noise")
    })      
    

    #************************************************
    ## >> # OBS SLIDER RANGE CHANGES ----
    #*********************
    source("serverPt_nObsDblClk.R", local=TRUE)
    
    
    #************************************************
    ## >> COPY BUTTONS ----
    #*********************
    ## Copying over values from "No Violations" tab
    source("serverPt_copyVals.R", local=TRUE)
    
    
    #************************************************
    ## >> CLOSE CALLOUT DIVS ----
    #*********************
    runjs('$(".bs-close").click(function() {
        $(this).parent().fadeOut("slow");
    });')  
    
    
    #******************************************
    ## ((Housekeeping)) ----
    #*********************
    ## kill connection to server once app stops running   
    session$onSessionEnded(stopApp)
    
}
