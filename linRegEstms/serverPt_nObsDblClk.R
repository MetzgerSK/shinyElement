# GENERIC FUNCTION
dblClk <- function(stub, nSld){
    
    # To deal with main tab not having underscore in its naming conventions
    ifelse(stub=="", {stub}, {stub <- paste0("_", stub)})
    
    # The actual code
    expr <- paste0('
        ## Define as a reactive, just in case
        rv', stub, ' <- reactiveValues(state="large") 

        ## Begin defining the behavior for the double click fire
        onevent("nObs', stub,'", event="dblclick", expr={    
            clk <- ifelse(rv', stub,'$state=="small", 0, 1)  # rv will have pre-switch state
            curr <- input$nObs', stub, '                     # current slider value
        
            # General idea: when user switches, keep the slider value in the same
            # general ballpark of where it currently is, which means figuring out
            # the current slider\'s position and determining the new value, given
            # the new slider range
        
            if(mod(clk,2)==0){      # current is now going to be large -> pre-switch = small
                # Update reactive
                rv', stub, '$state <- "large"

                # Update color
                runjs("$(\'#nObs', stub, ' \').parent(\'div\').removeClass(\'smaller\')")
                
                # Set other vals
                min <- smMin # small
                max <- smMax
                del <- smDel

                othMin <- lgMin # large
                othMax <- lgMax
                othDel <- lgDel

            } else {  # current is now going to be small -> pre-switch = large
                # Update reactive
                rv', stub, '$state<- "small"

                # Update color
                runjs("$(\'#nObs', stub, ' \').parent(\'div\').addClass(\'smaller\')")
 
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
            updateSliderInput(session, "nObs', stub, '", min = othMin, max = othMax, step = othDel, value = val)
            
        })'     # <- the end of the onevent()
    )  
      
    return(expr)
}

# Tweak LTS alpha margin
ltsMarg <- function(stub){
    # To deal with main tab not having underscore in its naming conventions
    ifelse(stub=="", {stub}, {stub <- paste0("_", stub)})
    
    # Actual code
    expr <- paste0('
                runjs("$(\'#ltsAlpha', stub, ' \').parent(\'div\').addClass(\'ltsSlider\')")'
            )
    
    return(expr)
}
#*************************************************
# DEFINE FOR EACH TAB ----
#************************
# tabList defined in global.R

# Loop
for(i in 1:length(tabList)){
    # Dbl click
    eval(parse(text=
                   dblClk(tabList[i])
        ))
    
    #LTS alpha margin
    eval(parse(text=
                   ltsMarg(tabList[i])
        ))
}