# (contents of global.R get loaded automatically by shiny behind the scenes)

# How many models there are, in total
maxMod <- 7

# The server function
server <- function(input, output, session) {
    
    #***************************************************************
    # >> DGP EQS (MathJax) ----
    #*********************
    source("serverPt_eqs.R", local=TRUE)
    
    #************************************************
    ## >> VCOV AVERAGES ----
    #*********************
    source("serverPt_vcovAvgs.R", local=TRUE)
    
    #******************************************
    # >> GRAPHS ----
    #*********************
    ## (need to be this early b/c one of the functions in graphFuncts gets called 
    ## early on in another reactive function.)
    source("serverPt_graphFuncts.R", local=TRUE)
    source("serverPt_graphOutputs.R", local=TRUE) 
    
    #************************************************
    # >> MONITOR FOR CANLOAD CHKBOX + FILE CHECK HELPER ----
    #*********************
    source("serverPt_fileCheck.R", local=TRUE)
    
    #***************************************************************
    # >> MC SIM FUNCTION ----
    #*********************	
    ## Used Francis Smart's blogpost on the simulations in R to get skeleton
    # http://www.econometricsbysimulation.com/2012/12/easy-monte-carlo-sampler-command.html

    MC_easy <- function(dgp, estimator, spec, obs, reps, seed, acType, lts, canLoad) {
        withProgress(message = 'Running Simulations', value=0, min=1, max=reps, {
            
            # Form up file name + list
            dgpR <- strsplit(dgp, "_")[[1]][1]
            out <- fNameBuilder(dgpR)
               fName <- out[[1]]
               fList <- out[[2]]
            
            # If there's a match...
            if(length(fList)>0){
                ans <- fileCheck(fName, fList, dgpR, reps, seed, canLoad)
                
                # Load into relv objects
                rerun <- ans[[1]][[1]]
                fileReps <- ans[[1]][[2]]
                processed <- ans[[2]][[1]]
                vcovMatProc <- ans[[2]][[2]]
                
            # Otherwise, no sims in mem
            } else rerun <- TRUE  
            
            # Run sims if either not in mem OR there are too few of the current sims saved
            if(rerun==TRUE) {
                # Set seed
                set.seed(seed)
                
                # Create empty holder objects
                MC_results <- NULL
                modObjs <- NULL
                vcovMats <- NULL
                trueX <- NULL
                
                # Loop
                for (i in 1:reps) {
                    # Advance counter
                    incProgress(1, detail = paste(i, " of ", reps)) 
                    
                    # Pull data
                    temp_data <- get(dgp)(obs)
                    
                    # Estimate model
                    returned <- get(estimator)(temp_data, spec, acType, lts)
                    
                    # Store results
                    MC_results[[i]] <- returned[[1]]
                    modObjs[[i]]    <- returned[[2]]
                    vcovMats[[i]]   <- returned[[3]]
                    trueX[[i]] <- ifelse("xTrue" %in% colnames(temp_data), 
                                     data.frame(temp_data$xTrue),
                                     data.frame(temp_data$x1))
                }
               
                # To get everything in shape
                processed <- NULL
                modObjProc <- NULL
                vcovMatProc <- NULL
                
                for(i in 1:maxMod){
                    # For MC results    
                    processed[[i]] <- lapply(MC_results, "[[", i) %>% as.data.frame() %>% t()
                    rownames(processed[[i]]) <- NULL # Nuke the row names; cols are what's relevant.
                    
                    # For model object
                    modObjProc[[i]] <- lapply(modObjs, "[[", i)
                    
                    # For VCOV matrices
                    vcovMatProc[[i]] <- lapply(vcovMats, "[[", i)
                }
                
                # Save the main four objects
                incProgress(0, message = "Saving", detail = "Compressing file (may take a moment)") 
                save(processed, vcovMatProc,  
                     file=paste0("__simRslts/", fName, "_r", seed, "_s", reps, ".RData"),
                     compress=TRUE, compression_level=9, version=2)
                
                # If there were files, but those files didn't have enough reps,
                # erase them and keep the new
                if(length(fList)>0)  file.remove(paste0("__simRslts/", fName, "_s", fileReps, ".RData"))
            }
            
            # Further process the vcovs, to save computation time later.
            vcovAvgs <- vcovAvg(vcovMatProc)
                
            # return the results of the estimation.
            return(list(processed, NULL, vcovMatProc, vcovAvgs)) 
            
        })
    }
    
    
    #***************************************************************
    # >> DGP FUNCTIONS ----
    #*********************************
    source("serverPt_dgps.R", local=TRUE)
    
    
    #***************************************************************
    # >> MODEL ESTIMATION ----
    #*********************
    # ~~ ACTUAL ESTIMATION FUNCTIONS ~~
    source("serverPt_models.R", local=TRUE)
    
    # ~~ OMNIBUS CALL TO RUN EVERYTHING ~~
    estOmni <- function(temp_data, spec, acType, lts){
        # Because 4 of the 7 of these start from the lm() fit,
        # so estimate it here and pass the obj along to save time
        lm.mod <- lm(spec, data=temp_data)
        
        # Run each estimator
        set1 <- OLS_est(temp_data, spec, lm.mod)
        set2 <- OLS_rob_est(temp_data, spec, lm.mod)
        set3 <- OLS_HAC_est(temp_data, spec, lm.mod)
        
        # If this isn't AC, then run hetero-based WLS, FGLS
        if(regexpr("p\\s*=\\s*0,\\s*q\\s*=\\s*0", acType)>0){
            set7 <- WLS_est(temp_data, spec)
            set4 <- FGLS_est(temp_data, spec, lm.mod)
         
        # Otherwise, run the AC-based WLS, FGLS     
        } else {
            set7 <- GLS_est(temp_data, acCoef=input$arVal)  # NOTE: no spec argument -> is hardcoded inside function
            set4 <- FGLS_AC_est(temp_data, spec, acType)
        }
        
        set5 <- robM_est(temp_data, spec)
        set6 <- LTS_est(temp_data, spec, lts)

        # Shift things around so that all the estms are in
        # one object, and all the models in another.
        list(list(set1[[1]], set2[[1]], set3[[1]], set4[[1]], set5[[1]], set6[[1]], set7[[1]]),
             list(set1[[2]], set2[[2]], set3[[2]], set4[[2]], set5[[2]], set6[[2]], set7[[2]]),
             list(set1[[3]], set2[[3]], set3[[3]], set4[[3]], set5[[3]], set6[[3]], set7[[3]])
        )  
    }
    

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
                       "SE: Lower 95% CI"       = quantile(se, .025),
                       "SE: Upper 95% CI"       = quantile(se, .975),
                       "StDev of Estimate"      = sd(ptEst))
        
        return(row)
    }
        
    printMCs <- function(mods, param, true, header) {
        # Input: object with all 7 sets of simulation results, value 
        #        of relevant estSel widget, true val of that parameter

        # Extract the relevant bHat and SEs, given what the user's selected 
        relv <- lapply(mods, function(x){ 
                                dplyr::select(data.frame(x), 
                                              all_of(param), 
                                              all_of(gsub("b", "se", param))) 
                             } 
                      )
        
        # Loop over models
        for(m in 1:maxMod){
            # Paste in model results
            assign(paste0("m", m),
                colBuilder(true, 
                           relv[[m]][,paste0(param)],
                           relv[[m]][,paste0(gsub("b", "se", param))])
            )
        }

        text <- cbind(m1, m2, m3, m7, m4, m5, m6)  ## HARDCODED.
        
        colnames(text) <- header
        return(text)
    }

    
    #************************************************
    ## >> SLIDER VALUE CHNGS ----
    #*********************
    ## Randomly pick values for the parameters
    observeEvent(input$randPars, {
        updateSliderInput(session, "aHat" , value=sample(-20:20, 1)/4)
        updateSliderInput(session, "b1Hat", value=sample(-20:20, 1)/4)
        updateSliderInput(session, "b2Hat", value=sample(-20:20, 1)/4)
        updateSliderInput(session, "noise", value=sample(1:10, 1))
        
    }) 
    
    ## Reset to the defaults
    observeEvent(input$reset, {
        updateSliderInput(session, "aHat" , value=3.5)
        updateSliderInput(session, "b1Hat", value=-0.75)
        updateSliderInput(session, "b2Hat", value=0.25)
        updateSliderInput(session, "noise", value=1)
        
    })      
    
    #************************************************
    ## >> # OBS SLIDER RANGE CHANGES ----
    #*********************
    source("serverPt_nObsDblClk.R", local=TRUE)
    
    
    #************************************************
    ## >> COPY BUTTONS ----
    #*********************
    ## Copying over values from No violations tab
    source("serverPt_copyVals.R", local=TRUE)
    
    
    #******************************************
    ## >> DATA OUTPUTS ----
    #*********************
    # (DT headers now defined in global.R)
    
    source("serverPt_datOut.R", local=TRUE)
    
    
    #******************************************
    ## >> EFFICIENCY MAT OUTPUTS ----
    #*********************
    source("serverPt_matComps.R", local=TRUE)   

    
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
