## >> GENERIC FUNCTION ----
copyVals <- function(stub){

    str <- paste0('
            observeEvent(input$copyButton_', stub,' , {
                updateNumericInput(session, "seed_', stub, '", value=input$seed)
                updateSliderInput(session, "sims_', stub, '", value=input$sims)

                updateSliderInput(session, "aHat_', stub, '", value=input$aHat)
                updateSliderInput(session, "b1Hat_', stub, '", value=input$b1Hat)
                updateSliderInput(session, "b2Hat_', stub, '", value=input$b2Hat)
                updateSliderInput(session, "noise_', stub, '", value=input$noise)

                updateSliderInput(session, "ltsAlpha_', stub, '", value=input$ltsAlpha)
                
                # Depending on value for n, swap sliders
                if((input$nObs<lgMin & rv_', stub, '$state==\'large\') |
                   (input$nObs>smMax & rv_', stub, '$state==\'small\') ){
                    runjs("$(\'#nObs_', stub, '\').dblclick()")
                   }
                # Covers the case where there was no slider state change   
                updateSliderInput(session, "nObs_', stub, '", value=input$nObs)  
                # Covers the case where there WAS a slider state change
                onNextInput(updateSliderInput(session, "nObs_', stub, '", value=input$nObs))
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