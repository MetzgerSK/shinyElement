library(shiny)
library(DT)
library(survival)
library(msm)
library(magrittr)
library(censReg)
library(dplyr)

server <- function(input, output, session){
    
    #*****************************************
    ## EQUATION OUTPUTS -------------
    #***********************    
    source("serverPt_eqs.R", local=TRUE)
    
    
    #*********************************************************************
    # MC SIM FUNCTION -------------------
    #***********************	
    ## Used Francis Smart's blogpost on R simulations to get skeleton
    # http://www.econometricsbysimulation.com/2012/12/easy-monte-carlo-sampler-command.html
    
    MC_easy = function(dgp, estimator, obs, reps, seed, cens=0) {
        withProgress(message = 'Running Simulations', value=1, min=1, max=reps, {
            # Set seed
            set.seed(seed)
            
            # Create object to hold all sim results
            MC_results <- NULL
            
            # Run the simulations
            for (i in 1:reps) {
                # Pull data
                temp_data <- get(dgp)(obs)
    
                # Run model, store results
                MC_results[[i]] <- get(estimator)(temp_data)
                
                # Update counter
                incProgress(1, detail = paste(i, " of ", reps)) 
            }
            
            # Return the results
            return(list(do.call(rbind,MC_results)))
        })
    }
            
    
    #**********************************************
    # DGP FUNCTIONS -------------------
    #***********************
    source("serverPt_dgps.R", local=TRUE)

        
    #**********************************************
    # MODEL ESTM FUNCTS (OMNI + INDV) -------------------
    #***********************
    source("serverPt_estm.R", local=TRUE)   
        
    
    #**********************************************
    ## EVENT REACTIVES (!! START) -----------------------
    #***********************
    ## > Non-Normal ==================
    data_reg_nnorm <- eventReactive(input$goButton_nnorm, {
        seed <- input$seed_nnorm  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_nnorm  # number of observations
        sims <- input$sims_nnorm  # of simulations
        
        #  MC it
        list(data.frame(MC_easy(dgp="nnorm_OLS_data", estimator="sim_est", obs=nObs, seed=seed, reps=sims)))
    })
    
    ## > Censoring ==================
    data_reg_cens <- eventReactive(input$goButton_cens, {
        seed <- input$seed_cens  # to set seed for each MC experiment, for replicability.
        nObs <- input$nObs_cens  # number of observations
        sims <- input$sims_cens  # of simulations
        cens <- input$perc_cens
        
        #  MC it
        return(list(data.frame(
                        MC_easy(dgp="cens_OLS_data", estimator="sim_est_cens", obs=nObs, seed=seed, reps=sims, cens=cens)
                    ))
               )
    })
    
    # On first run, unhide the results div, hide the instruction div    
    observeEvent(input$goButton_nnorm,{
        shinyjs::show("all_nnorm")
        shinyjs::hide("instr_nnorm")
    })
    
    observeEvent(input$goButton_cens,{
        shinyjs::show("all_cens")
        shinyjs::hide("instr_cens")
    })
    
    #**********************************************
    ## TABLES (all) -----------------------
    ## (Has both printMC() and the render()s)
    #*********************** 
    source("serverPt_datOut.R", local=TRUE)
    
    
    #**********************************************
    ## BUTTONS: RANDOM, RESET -------------------
    #***********************
    source("serverPt_btns.R", local=TRUE)
    
    
    #**********************************************
    ## GRAPHS -------------------
    #***********************
    source("serverPt_gphs.R", local=TRUE)

    
    #******************************************
    ## ((Housekeeping)) ----
    #*********************
    ## kill connection to server once app stops running 
    ## (remove comment before deploying on web)
    
    session$onSessionEnded(stopApp)
}
