## GENERIC DT FUNCTION ----
dtPrint <- function(data, header){
    DT::datatable(data, rownames= FALSE, colnames = header, 
                  options = list(
                                dom = 't p' 
                            )
    ) %>% formatRound(c(1:ncol(data)), 5)    
}

## GENERIC DWNLD UNHIDES + FYI HIDE ----
dwnUnhide <- function(stub){
    
    # Because of the noViol button having no suffix:
    ifelse(stub=="reg", {btnSt <- ""}, {btnSt <- paste0("_", stub)})
    
    expr <- paste0('
        # Activate fake dataset download button ===
        observeEvent(input$goButton', btnSt, ',{
            shinyjs::show("datDwn_', stub, '")
            shinyjs::show("rawDwn_', stub, '")
            shinyjs::hide(selector="h4.simFyiHdr_', stub, '")
        })  
    ')
    
    return(expr)               
}

# (looping over all the tabs for the download button unhides, to expedite ->
# deviates from rest of file's layout)

for(i in 1:length(tabList)){   ### tabList defined in global.R
    
    # For the first element, buttons use "reg" instead of "" for noViol
    arg <- tabList[i]
    if(i==1)    arg <- "reg"

    # Evaluate
    eval(parse(text=
                   dwnUnhide(arg)
    ))
}

#*******************************************************************************

## >> TRUTH (no violations)----
    # Data ===
    data_reg <- eventReactive(input$goButton, {
        seed <- input$seed  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs  # number of observations
        sims <- input$sims  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="true_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_reg <- renderTable({
        MC_est <- data_reg()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat, b1Hat=input$b1Hat, b2Hat=input$b2Hat, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
        
        }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # Sim draw output ===
    output$table_reg <- DT::renderDataTable({
        dtPrint(data_reg()[[1]], DT.header.reg)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_reg <- downloadHandler(  
        filename = function(){
            paste0("fakeData_noViol.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(true_OLS_data(input$nObs), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_reg <- downloadHandler(  
        filename = function(){
            paste0("rawSims_noViol.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_reg()[[1]], file, row.names = FALSE)
        }
    )


## >> ENDOG/OV BIAS----
    # Data ===
    data_end <- eventReactive(input$goButton_end, {
        seed <- input$seed_end  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_end  # number of observations
        sims <- input$sims_end  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="endog_OLS_data", estimator="OLS_est", spec="y~x1", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })

    # Basic results table ===
    output$data_end <- renderTable({
        MC_est <- data_end()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs_end(aHat=input$aHat_end, b1Hat=input$b1Hat_end, 
                             ptEstA=MC_est$b0, ptEstB1=MC_est$b1, 
                             seA=MC_est$se0, seB1=MC_est$se1))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)    

    # All sim draw output ===
    output$table_end <- DT::renderDataTable({
        dtPrint(data_end()[[1]], DT.header.end)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_end <- downloadHandler(  
        filename = function(){
            paste0("fakeData_OV.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(endog_OLS_data(input$nObs_end), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_end <- downloadHandler(  
        filename = function(){
            paste0("rawSims_OV.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_end()[[1]], file, row.names = FALSE)
        }
    )
 

## >> SIMULT----
    # Data ===
    data_endSim <- eventReactive(input$goButton_endSim, {
        seed <- input$seed_endSim  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_endSim  # number of observations
        sims <- input$sims_endSim  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="endSim_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
    })
    
    # Basic results table ===
    output$data_endSim <- renderTable({
        MC_est <- data_endSim()[[1]] %>% data.frame()
    
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_endSim, b1Hat=input$b1Hat_endSim, b2Hat=input$b2Hat_endSim, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)    
    
    # All sim draw output ===
    output$table_endSim <- DT::renderDataTable({
        dtPrint(data_endSim()[[1]], DT.header.reg)
    })
    
    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_endSim <- downloadHandler(
        filename = function(){
            paste0("fakeData_endSim.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(endSim_OLS_data(input$nObs_endSim), file, row.names = FALSE)
        }
    )

    # Raw results download ===
    output$rawDwn_endSim <- downloadHandler(
        filename = function(){
            paste0("rawSims_endSim.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_endSim()[[1]], file, row.names = FALSE)
        }
    )
    

## >> MEAS ERR: X ----
    # Data ===
    data_endMeasErrX <- eventReactive(input$goButton_endMeasErrX, {
        seed <- input$seed_endMeasErrX  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_endMeasErrX  # number of observations
        sims <- input$sims_endMeasErrX  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="endMeasErrX_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]], mc[[3]])
    })
    
    # Basic results table ===
    output$data_endMeasErrX <- renderTable({
        MC_est <- data_endMeasErrX()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_endMeasErrX, b1Hat=input$b1Hat_endMeasErrX, b2Hat=input$b2Hat_endMeasErrX, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)    
    
    # All sim draw output ===
    output$table_endMeasErrX <- DT::renderDataTable({
        dtPrint(data_endMeasErrX()[[1]], DT.header.reg)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_endMeasErrX <- downloadHandler(   
        filename = function(){
            #paste0("fileFilefile.txt")
            paste0("fakeData_endMeasErrX.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(endMeasErrX_OLS_data(input$nObs_endMeasErrX), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_endMeasErrX <- downloadHandler(  
        filename = function(){
            paste0("rawSims_endMeasErrX.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_endMeasErrX()[[1]], file, row.names = FALSE)
        }
    )
    

## >> MEAS ERR: Y----
    # Data ===
    data_endMeasErrY <- eventReactive(input$goButton_endMeasErrY, {
        seed <- input$seed_endMeasErrY  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_endMeasErrY  # number of observations
        sims <- input$sims_endMeasErrY  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="endMeasErrY_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_endMeasErrY <- renderTable({
        MC_est <- data_endMeasErrY()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_endMeasErrY, b1Hat=input$b1Hat_endMeasErrY, b2Hat=input$b2Hat_endMeasErrY, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)    
    
    # All sim draw output ===
    output$table_endMeasErrY <- DT::renderDataTable({
        dtPrint(data_endMeasErrY()[[1]], DT.header.reg)
    })
    
    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_endMeasErrY <- downloadHandler(  
        filename = function(){
            paste0("fakeData_endMeasErrY.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(endMeasErrY_OLS_data(input$nObs_endMeasErrY), file, row.names = FALSE)
        }
    )

    # Raw results download ===
    output$rawDwn_endMeasErrY <- downloadHandler(  
        filename = function(){
            paste0("rawSims_endMeasErrY.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_endMeasErrY()[[1]], file, row.names = FALSE)
        }
    )


## >> E(u)!=0 ----
    # Data ===
    data_ar <- eventReactive(input$goButton_ar, {
        seed <- input$seed_ar  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_ar  # number of observations
        sims <- input$sims_ar  # of simulations
          
        # MC it
        mc <- MC_easy(dgp="ar_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_ar <- renderTable({
        MC_est <- data_ar()[[1]] %>% data.frame()       
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_ar, b1Hat=input$b1Hat_ar, b2Hat=input$b2Hat_ar, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # All sim draw output ===
    output$table_ar <- DT::renderDataTable({
        dtPrint(data_ar()[[1]], DT.header.reg)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_ar <- downloadHandler(
        filename = function(){
            paste0("fakeData_EU0.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(ar_OLS_data(input$nObs_ar), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_ar <- downloadHandler(
        filename = function(){
            paste0("rawSims_EU0.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_ar()[[1]], file, row.names = FALSE)
        }
    )
    

## >> HETERO====
    # Data ===
    data_hetero <- eventReactive(input$goButton_hetero, {
        seed <- input$seed_hetero  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_hetero  # number of observations
        sims <- input$sims_hetero  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="hetero_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })

    # Basic results table ===
    output$data_hetero <- renderTable({
        MC_est <- data_hetero()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_hetero, b1Hat=input$b1Hat_hetero, b2Hat=input$b2Hat_hetero, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    },  rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)

    # All sim draw output ===
    output$table_hetero <- DT::renderDataTable({
        dtPrint(data_hetero()[[1]], DT.header.reg)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_hetero <- downloadHandler(
        filename = function(){
            paste0("fakeData_hetero.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(hetero_OLS_data(input$nObs_hetero), file, row.names = FALSE)
        }
    )

    # Raw results download ===
    output$rawDwn_hetero <- downloadHandler(
        filename = function(){
            paste0("rawSims_hetero.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_hetero()[[1]], file, row.names = FALSE)
        }
    )
   

## >> AUTOCORRELATION----
    # Data ===
    data_ac <- eventReactive(input$goButton_ac, {
        seed <- input$seed_ac  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_ac  # number of observations
        sims <- input$sims_ac  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="ac_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })

    # Basic results table ===
    output$data_ac <- renderTable({
        MC_est <- data_ac()[[1]] %>% data.frame()       
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_ac, b1Hat=input$b1Hat_ac, b2Hat=input$b2Hat_ac, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)

    # All sim draw output ===
    output$table_ac <- DT::renderDataTable({
        dtPrint(data_ac()[[1]], DT.header.reg)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_ac <- downloadHandler(
        filename = function(){
            paste0("fakeData_ac.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(ac_OLS_data(input$nObs_ac), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_ac <- downloadHandler(
        filename = function(){
            paste0("rawSims_ac.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_ac()[[1]], file, row.names = FALSE)
        }
    )
  

## >> NONLINEAR----
    # Data ===
    data_nlin <- eventReactive(input$goButton_nlin, {
        seed <- input$seed_nlin  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_nlin  # number of observations
        sims <- input$sims_nlin  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="nlin_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_nlin <- renderTable({
        MC_est <- data_nlin()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_nlin, b1Hat=input$b1Hat_nlin, b2Hat=input$b2Hat_nlin, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    }, rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # All sim draw output ===
    output$table_nlin <- DT::renderDataTable({
        dtPrint(data_nlin()[[1]], DT.header.reg)
    })

    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_nlin <- downloadHandler(  
        filename = function(){
            paste0("fakeData_nlin.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(nlin_OLS_data(input$nObs_nlin), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_nlin <- downloadHandler(  
        filename = function(){
            paste0("rawSims_nlin.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_nlin()[[1]], file, row.names = FALSE)
        }
    )
    

## >> HEAVY ----
    # Data ===
    data_heavy <- eventReactive(input$goButton_heavy, {
        seed <- input$seed_heavy  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_heavy  # number of observations
        sims <- input$sims_heavy  # of simulations
      
        # MC it
        mc <- MC_easy(dgp="heavy_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_heavy <- renderTable({
        MC_est <- data_heavy()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_heavy, b1Hat=input$b1Hat_heavy, b2Hat=input$b2Hat_heavy, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    },  rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # All sim draw output ===
    output$table_heavy <- DT::renderDataTable({
        dtPrint(data_heavy()[[1]], DT.header.reg)
    })
    
    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_heavy <- downloadHandler( 
        filename = function(){
            paste0("fakeData_heavyTailedErr.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(heavy_OLS_data(input$nObs_heavy), file, row.names = FALSE)
        }
    )

    # Raw results download ===
    output$rawDwn_heavy <- downloadHandler( 
        filename = function(){
            paste0("rawSims_heavyTailedErr.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_heavy()[[1]], file, row.names = FALSE)
        }
    )
    
    
## >> SKEWED ----
    # Data ===
    data_skewed <- eventReactive(input$goButton_skewed, {
        seed <- input$seed_skewed  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_skewed  # number of observations
        sims <- input$sims_skewed  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="skewed_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_skewed <- renderTable({
        MC_est <- data_skewed()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_skewed, b1Hat=input$b1Hat_skewed, b2Hat=input$b2Hat_skewed, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    },  rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # All sim draw output ===
    output$table_skewed <- DT::renderDataTable({
        dtPrint(data_skewed()[[1]], DT.header.reg)
    })
    
    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_skewed <- downloadHandler(  
        filename = function(){
            paste0("fakeData_skewedErr.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(skewed_OLS_data(input$nObs_skewed), file, row.names = FALSE)
        }
    )
    
    # Raw results download ===
    output$rawDwn_skewed <- downloadHandler( 
        filename = function(){
            paste0("rawSims_skewedErr.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_skewed()[[1]], file, row.names = FALSE)
        }
    )
    
    
## >> MULTI-MODAL ----
    # Data ===
    data_mmodal <- eventReactive(input$goButton_mmodal, {
        seed <- input$seed_mmodal  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_mmodal  # number of observations
        sims <- input$sims_mmodal  # of simulations
        
        # MC it
        mc <- MC_easy(dgp="mmodal_OLS_data", estimator="OLS_est", spec="y~x1+z", obs=nObs, seed=seed, reps=sims)
        
        list(data.frame(mc[[1]]), mc[[2]])
    })
    
    # Basic results table ===
    output$data_mmodal <- renderTable({
        MC_est <- data_mmodal()[[1]] %>% data.frame()
        
        # Print out the stuff.
        isolate(printMCs(aHat=input$aHat_mmodal, b1Hat=input$b1Hat_mmodal, b2Hat=input$b2Hat_mmodal, 
                         ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, 
                         seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2))
    },  rownames = TRUE, bordered=TRUE, striped=TRUE, hover=TRUE)
    
    # All sim draw output ===
    output$table_mmodal <- DT::renderDataTable({
        dtPrint(data_mmodal()[[1]], DT.header.reg)
    })
    
    # Download button activate ===
    ## defined at file's top
    
    # Data download ===
    output$datDwn_mmodal <- downloadHandler(
        filename = function(){
            paste0("fakeData_mmodalErr.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(mmodal_OLS_data(input$nObs_mmodal), file, row.names = FALSE)
        }
    )    
    
    # Raw results download ===
    output$rawDwn_mmodal <- downloadHandler(
        filename = function(){
            paste0("rawSims_mmodalErr.csv")  # what the saved file's default name should be
        },
        content = function(file){
            write.csv(data_mmodal()[[1]], file, row.names = FALSE)
        }
    ) 