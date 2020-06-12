## >> GENERIC FUNCTION ----
copyVals <- function(stub){

    str <- paste0('
            observeEvent(input$copyButton_', stub,' , {
                updateNumericInput(session, "seed_', stub, '", value=input$seed)
                updateSliderInput(session, "sims_', stub, '", value=input$sims)

                updateSliderInput(session, "nObs_', stub, '", value=input$nObs)
                updateSliderInput(session, "aHat_', stub, '", value=input$aHat)
                updateSliderInput(session, "b1Hat_', stub, '", value=input$b1Hat)
                updateSliderInput(session, "b2Hat_', stub, '", value=input$b2Hat)
                updateSliderInput(session, "noise_', stub, '", value=input$noise)
            }) 
        ')
    
    eval(parse(text=str))
}


## ENDOG/OV BIAS ----
copyVals("end")

## SIMULT ----
copyVals("endSim")

## E(u)!=0 ----
copyVals("ar")

## MEAS ERR: X ----
copyVals("endMeasErrX")

## MEAS ERR: Y ----
copyVals("endMeasErrY")

## HETERO ----
copyVals("hetero")

## AC ----
copyVals("ac")

## NON-LIN ----
copyVals("nlin")

## NORMALITY: HEAVY-TAILED ----
copyVals("heavy")

## NORMALITY: SKEWED ----
copyVals("skewed")

## NORMALITY: MULTI-MODAL----
copyVals("mmodal")