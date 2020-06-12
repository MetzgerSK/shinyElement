## Define as a reactive, just in case
rv <- reactiveValues(state="large") 

## Begin defining the behavior for the double click fire
onevent("nObs", event="dblclick", expr={    
    clk <- ifelse(rv$state=="small", 0, 1)  # rv will have pre-switch state
    curr <- input$nObs                      # current slider value

    # Just to define these in one place, so I don\'t go nuts if I make changes
    ## LABELS FLIPPED (b/c small is now default)
    lgMin <- 3
    lgMax <- 150
    lgDel <- 3
                
    smMin <- 25
    smMax <- 500
    smDel <- 25


    # General idea: when user switches, keep the slider value in the same
    # general ballpark of where it currently is, which means figuring out
    # the current slider\'s position and determining the new value, given
    # the new slider range

    if(mod(clk,2)==0){      # current is now going to be large -> pre-switch = small
        # Update reactive
        rv$state <- "large"

        # Update color
        runjs("$(\'#nObs \').parent(\'div\').removeClass(\'smaller\')")
        
        # Set other vals
        min <- smMin # small
        max <- smMax
        del <- smDel

        othMin <- lgMin # large
        othMax <- lgMax
        othDel <- lgDel

    } else {  # current is now going to be small -> pre-switch = large
        # Update reactive
        rv$state<- "small"

        # Update color
        runjs("$(\'#nObs \').parent(\'div\').addClass(\'smaller\')")

        # Set other vals
        min <- lgMin # large
        max <- lgMax
        del <- lgDel

        othMin <- smMin #small
        othMax <- smMax
        othDel <- smDel
    }
    
    # as percentage of \'current\'
    perc <- (curr - min)/(max-min) 
              
    # Adjust
    val <- perc*(othMax-othMin) + othMin
    val <- plyr::round_any(val, othDel) 

    # Change slider
    updateSliderInput(session, "nObs", min = othMin, max = othMax, step = othDel, value = val)
    
})     # <- the end of the onevent()
