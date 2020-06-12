## PROC RES TABLE: DIGITS TO ROUND TO
digitsResTab <- 3

## GENERIC DT FUNCTION ----
dtPrint <- function(data, header){
    DT::datatable(data, rownames= FALSE, colnames = header, 
                  options = list(
                                dom = 't p' 
                            )
    ) %>% formatRound(c(1:ncol(data)), 5)    
}

## GENERIC: PULL TRUE PARAM VALUE ----
paramTrue <- function(est, stub){
    
    # Pull the correct input name
    if(est=="b0"){
        inp <- paste0("input$aHat" , stub ) 
        
    } else if(est=="b1"){
        inp <- paste0("input$b1Hat", stub ) 
        
    } else{
        inp <- paste0("input$b2Hat", stub )
    }
    
    # Evaluate the input's value and return it
    val <- eval(parse(text=inp))
    val
}

## GENERIC DWNLD UNHIDES + FYI HIDE ----
dwnUnhide <- function(stub){
    
    # Because of the noViol button having no suffix:
    ifelse(stub=="reg", {btnSt <- ""}, {btnSt <- paste0("_", stub)})
    
    expr <- paste0('
        # Activate fake dataset download button (among other things)===
        observeEvent(input$goButton', btnSt, ',{
            shinyjs::show("datDwn_', stub, '")
            shinyjs::show("rawDwn_', stub, '")
            shinyjs::show("tabKey_', stub, '")
            shinyjs::show("estSel_', stub,'")
            shinyjs::show("distGphDisp_', stub,'")
            shinyjs::show("rawResTblDisp_', stub,'")
            shinyjs::hide(selector="h4.simFyiHdr_', stub, '")
            shinyjs::show("effInfo_', stub,'")
        })  
    ')
    
    return(expr)               
}

## GENERIC DWNLD FUNCTS (DATASET FROM DGP + RESULTS) ----
dwnFuncts <- function(stub){
    
    # Because of the noViol button having no suffix + 
    # other odd naming conventions:
    if(stub=="reg") {
        btnSt <- ""
        fSt <- "noViol"
        dgpPr <- "true"
    } else {
        btnSt <- paste0("_", stub)
        fSt <- stub
        dgpPr <- stub
    }
    
    expr <- paste0('
        # Data download ===
        output$datDwn_', stub, ' <- downloadHandler(   
            filename = function(){
                paste0("fakeData_', fSt, '.csv")  
            },
            content = function(file){
                write.csv(', dgpPr, '_OLS_data(input$nObs', btnSt, '), file, row.names = FALSE)
            }
        )
        
        # Raw results download ===
        output$rawDwn_', stub, ' <- downloadHandler(   
            filename = function(){
                paste0("rawSims",as.numeric(input$modRaw_', stub, '),"_', fSt, '.csv")  
            },
            content = function(file){
                write.csv(data_', stub, '()[[1]][as.numeric(input$modRaw_', stub, ')], file, row.names = FALSE)
            }
        )
    ')
    
    return(expr)               
}


# (looping over all the tabs for the download button unhides + the functions
# defining the download CSVs, to expedite -> deviates from rest of file's
# layout)
for(i in 1:length(tabList)){   ### tabList defined in global.R
    
    # For the first element, buttons use "reg" instead of "" for noViol
    arg <- tabList[i]
    if(i==1)    arg <- "reg"

    # Evaluate for the button unhides
    eval(parse(text=
                   dwnUnhide(arg)
    ))
    
    # Evaluate for the dwnld file creates
    eval(parse(text=
                   dwnFuncts(arg)
    ))
}

#*******************************************************************************
## >> TRUTH (no violations)----
    # Data ===
    data_reg <- eventReactive(input$goButton, {
        rv$running <- TRUE
        seed <- input$seed  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs  # number of observations
        sims <- input$sims  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"   
        
        #  MC it
        mc <- MC_easy(dgp="true_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha,
                      canLoad = input$canLoad)
        
        rv$running <- FALSE
        mc 
    })
    
    # Basic results table ===
    output$data_reg <- renderTable({
        input$goButton
        parTrue <- paramTrue(input$estSel_reg, "")
        
        # Print out the stuff.
        isolate(printMCs(data_reg()[[1]], input$estSel_reg, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    # Sim draw output ===
    output$table_reg <- DT::renderDataTable({
        dat <- data_reg()[[1]][as.numeric(input$modRaw_reg)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })

    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()

    
## >> ENDOG/OV BIAS----
    # Data ===
    data_end <- eventReactive(input$goButton_end, {
        seed <- input$seed_end  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_end  # number of observations
        sims <- input$sims_end  # of simulations
        
         # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
    
        #  MC it
        mc <- MC_easy(dgp="endog_OLS_data", estimator="estOmni", spec="y~x1", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_end,
                      canLoad = input$canLoad_end)
        
        mc
    })
 
    # Basic results table ===
    output$data_end <- renderTable({
        input$goButton_end
        parTrue <- paramTrue(input$estSel_end, "_end")
        
        # Print out the stuff.
        isolate(printMCs(data_end()[[1]], input$estSel_end, parTrue, header))

    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)    

    # All sim draw output ===
    output$table_end <- DT::renderDataTable({
        dat <- data_end()[[1]][as.numeric(input$modRaw_end)] %>% data.frame(.)
        dtPrint(dat, DT.header.end)
    })

    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()
    
    
## >> SIMULT----
    # Data ===
    data_endSim <- eventReactive(input$goButton_endSim, {
        seed <- input$seed_endSim  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_endSim  # number of observations
        sims <- input$sims_endSim  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="endSim_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_endSim,
                      canLoad = input$canLoad_endSim)
        
        mc
    })
        
    # Basic results table ===
    output$data_endSim <- renderTable({
        input$goButton_endSim
        parTrue <- paramTrue(input$estSel_endSim, "_endSim")
        
        # Print out the stuff.
        isolate(printMCs(data_endSim()[[1]], input$estSel_endSim, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab) 
    
    # All sim draw output ===
    output$table_endSim <- DT::renderDataTable({
        dat <- data_endSim()[[1]][as.numeric(input$modRaw_endSim)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })
    
    # Activate fake dataset download button ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()


## >> MEAS ERR: X ----
    # Data ===
    data_endMeasErrX <- eventReactive(input$goButton_endMeasErrX, {
        seed <- input$seed_endMeasErrX  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_endMeasErrX  # number of observations
        sims <- input$sims_endMeasErrX  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="endMeasErrX_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_endMeasErrX,
                      canLoad = input$canLoad_endMeasErrX)
        
        mc 
    })
    
    # Basic results table ===
    output$data_endMeasErrX <- renderTable({
        input$goButton_endMeasErrX
        parTrue <- paramTrue(input$estSel_endMeasErrX, "_endMeasErrX")
        
        # Print out the stuff.
        isolate(printMCs(data_endMeasErrX()[[1]], input$estSel_endMeasErrX, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)   
    
    # All sim draw output ===
    output$table_endMeasErrX <- DT::renderDataTable({
        dat <- data_endMeasErrX()[[1]][as.numeric(input$modRaw_endMeasErrX)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })

    # Estimate selector activate ===
        ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()

    
## >> MEAS ERR: Y----
    # Data ===
    data_endMeasErrY <- eventReactive(input$goButton_endMeasErrY, {
        seed <- input$seed_endMeasErrY  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_endMeasErrY  # number of observations
        sims <- input$sims_endMeasErrY  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="endMeasErrY_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_endMeasErrY,
                      canLoad = input$canLoad_endMeasErrY)
        
        mc
    })
    
    # Basic results table ===
    output$data_endMeasErrY <- renderTable({
        input$goButton_endMeasErrY
        parTrue <- paramTrue(input$estSel_endMeasErrY, "_endMeasErrY")
        
        # Print out the stuff.
        isolate(printMCs(data_endMeasErrY()[[1]], input$estSel_endMeasErrY, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    # All sim draw output ===
    output$table_endMeasErrY <- DT::renderDataTable({
        dat <- data_endMeasErrY()[[1]][as.numeric(input$modRaw_endMeasErrY)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg) 
    })
    
    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()


## >> E(u)!=0 ----
    # Data ===
    data_ar <- eventReactive(input$goButton_ar, {
        seed <- input$seed_ar  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_ar  # number of observations
        sims <- input$sims_ar  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="ar_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_ar,
                      canLoad = input$canLoad_ar)
        
        mc 
    })
    
    # Basic results table ===
    output$data_ar <- renderTable({
        input$goButton_ar
        parTrue <- paramTrue(input$estSel_ar, "_ar")
        
        # Print out the stuff.
        isolate(printMCs(data_ar()[[1]], input$estSel_ar, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    # All sim draw output ===
    output$table_ar <- DT::renderDataTable({
        dat <- data_ar()[[1]][as.numeric(input$modRaw_ar)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)        
    })

    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()
    
    
## >> HETERO====
    # Data ===
    data_hetero <- eventReactive(input$goButton_hetero, {
        seed <- input$seed_hetero  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_hetero  # number of observations
        sims <- input$sims_hetero  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="hetero_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_hetero,
                      canLoad = input$canLoad_hetero)
        
        mc
    })

    # Basic results table ===
    output$data_hetero <- renderTable({
        input$goButton_hetero
        parTrue <- paramTrue(input$estSel_hetero, "_hetero")
        
        # Print out the stuff.
        isolate(printMCs(data_hetero()[[1]], input$estSel_hetero, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)

    # All sim draw output ===
    output$table_hetero <- DT::renderDataTable({
        dat <- data_hetero()[[1]][as.numeric(input$modRaw_hetero)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })

    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()
    

## >> AUTOCORRELATION----
    # Data ===
    data_ac <- eventReactive(input$goButton_ac, {
        seed <- input$seed_ac  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_ac  # number of observations
        sims <- input$sims_ac  # of simulations
        
        # AC type: depends
        if(input$acChoice=="ar")  acType <- "corARMA(p=1, q=0)"
        else                      acType <- "corARMA(p=0, q=1)"
        
        #  MC it
        mc <- MC_easy(dgp="ac_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_ac,
                      canLoad = input$canLoad_ac)
        
        mc 
    })

    # Basic results table ===
    output$data_ac <- renderTable({
        input$goButton_ac
        parTrue <- paramTrue(input$estSel_ac, "_ac")
        
        # Print out the stuff.
        isolate(printMCs(data_ac()[[1]], input$estSel_ac, parTrue, headerAC))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)

    # All sim draw output ===
    output$table_ac <- DT::renderDataTable({
        dat <- data_ac()[[1]][as.numeric(input$modRaw_ac)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })

    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()
    
    
## >> NONLINEAR----
    # Data ===
    data_nlin <- eventReactive(input$goButton_nlin, {
        seed <- input$seed_nlin  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_nlin  # number of observations
        sims <- input$sims_nlin  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="nlin_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_nlin,
                      canLoad = input$canLoad_nlin)
        
        mc
    })
    
    # Basic results table ===
    output$data_nlin <- renderTable({
        input$goButton_nlin
        parTrue <- paramTrue(input$estSel_nlin, "_nlin")
        
        # Print out the stuff.
        isolate(printMCs(data_nlin()[[1]], input$estSel_nlin, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    
    # All sim draw output ===
    output$table_nlin <- DT::renderDataTable({
        dat <- data_nlin()[[1]][as.numeric(input$modRaw_nlin)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })

    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()

    
## >> HEAVY ----
    # Data ===
    data_heavy <- eventReactive(input$goButton_heavy, {
        seed <- input$seed_heavy  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_heavy  # number of observations
        sims <- input$sims_heavy  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="heavy_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_heavy,
                      canLoad = input$canLoad_heavy)
        
        mc
    })
    
    # Basic results table ===
    output$data_heavy <- renderTable({
        input$goButton_heavy
        parTrue <- paramTrue(input$estSel_heavy, "_heavy")
        
        # Print out the stuff.
        isolate(printMCs(data_heavy()[[1]], input$estSel_heavy, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    # All sim draw output ===
    output$table_heavy <- DT::renderDataTable({
        dat <- data_heavy()[[1]][as.numeric(input$modRaw_heavy)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })
    
    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()

    
## >> SKEWED ----
    # Data ===
    data_skewed <- eventReactive(input$goButton_skewed, {
        seed <- input$seed_skewed  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_skewed  # number of observations
        sims <- input$sims_skewed  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
      
        #  MC it
        mc <- MC_easy(dgp="skewed_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_skewed,
                      canLoad = input$canLoad_skewed)
        
        mc
    })
    
    # Basic results table ===
    output$data_skewed <- renderTable({
        input$goButton_skewed
        parTrue <- paramTrue(input$estSel_skewed, "_skewed")
        
        # Print out the stuff.
        isolate(printMCs(data_skewed()[[1]], input$estSel_skewed, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    # All sim draw output ===
    output$table_skewed <- DT::renderDataTable({
        dat <- data_skewed()[[1]][as.numeric(input$modRaw_skewed)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })
    
    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts()
    
    
## >> MULTI-MODAL ----
    # Data ===
    data_mmodal <- eventReactive(input$goButton_mmodal, {
        seed <- input$seed_mmodal  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_mmodal  # number of observations
        sims <- input$sims_mmodal  # of simulations
        
        # AC type: 0,0
        acType <- "corARMA(p=0, q=0)"
        
        #  MC it
        mc <- MC_easy(dgp="mmodal_OLS_data", estimator="estOmni", spec="y~x1+z", 
                      obs=nObs, seed=seed, reps=sims, acType=acType, lts=input$ltsAlpha_mmodal,
                      canLoad = input$canLoad_mmodal)
        
        mc
    })
    
    # Basic results table ===
    output$data_mmodal <- renderTable({
        input$goButton_mmodal
        parTrue <- paramTrue(input$estSel_mmodal, "_mmodal")
        
        # Print out the stuff.
        isolate(printMCs(data_mmodal()[[1]], input$estSel_mmodal, parTrue, header))
            
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE, digits=digitsResTab)
    
    # All sim draw output ===
    output$table_mmodal <- DT::renderDataTable({
        dat <- data_mmodal()[[1]][as.numeric(input$modRaw_mmodal)] %>% data.frame(.)
        dtPrint(dat, DT.header.reg)
    })
    
    # Estimate selector activate ===
    ## Defined above in dwnUnhide()
    
    # Data download ===
    ## Defined above in dwnFuncts()
    
    # Raw results download ===
    ## Defined above in dwnFuncts() 