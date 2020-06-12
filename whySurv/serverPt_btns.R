## >> GENERIC FUNCTION: RANDOM ----
randVals <- function(stub){

    str <- paste0('
            observeEvent(input$randPars_', stub,' , {
                updateSliderInput(session, "aHat_' , stub, '", value=sample(-20:20, 1)/4)
                updateSliderInput(session, "b1Hat_', stub, '", value=sample(-20:20, 1)/4)
                updateSliderInput(session, "b2Hat_', stub, '", value=sample(-20:20, 1)/4)
                updateSliderInput(session, "noise_', stub, '", value=sample(0:15, 1)/4 + 0.25)

            })'
           )
    
    eval(parse(text=str))
}
    
## >> GENERIC FUNCTION: RESET ----
resetVals <- function(stub){

    str <- paste0('
            observeEvent(input$reset_', stub,' , {
                reset("aHat_' , stub, '")
                reset("b1Hat_', stub, '")
                reset("b2Hat_', stub, '")
                reset("noise_', stub, '")
            })'
           )
    
    eval(parse(text=str))
}


## > NON-NORMAL ---------
randVals("nnorm")
resetVals("nnorm")


## > CENSORING ----------
randVals("cens")
resetVals("cens")
