## ** GENERIC GRAPH FUNCTIONS ** ----

# sampling distros
sampDistro <- function(dat.test, param, mod){
    
    font <- "Lato"
    
    ggplot(dat.test, aes(x=estm)) + 
        geom_histogram(aes(y=..density..), fill = "gray", color = "black") +
        geom_density(alpha=.2, size = 1.05, color="#104E8B") + 
        geom_rug(alpha=0.5) + 
        stat_function(fun = dnorm, args = list(mean = mean(dat.test$estm), sd = sd(dat.test$estm)),
                      color = "#FF4040", size = 1, linetype="dashed") + 
        xlim(mean(dat.test$estm)-3.5*sd(dat.test$estm), mean(dat.test$estm)+3.5*sd(dat.test$estm)) +
        labs(title=paste0(mod),
             x=paste0(param, " Values")) + 
        theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
              )

}

# Sampling distro stacker
## Creating a 3 x 2 plot matrix
sampDistMatrix <- function(dat, param, ac = "")
{
    # Get parameter; pull relevant columns
    relv <- lapply(dat, function(x){ 
                        dplyr::select(data.frame(x), all_of(param), all_of(gsub("b", "se", param))) %>%
                        rename(., estm=param, se=gsub("b", "se", param))
                     } 
              )
    
    # Go graph by graph
    for(i in 1:maxMod){
        nm <- nmChk(i) 
        
        # If it's robust or HAC, skip (since will be identical to vanilla OLS)
        if(regexpr("OLS", nm)>-1){
            if(nchar(nm)>3)   next
            else              nm <- "OLS (all)"  # shift label for graph
        }
        
        # If it's AC, replace "WLS" with "GLS"
        if(ac!="" & nm=="WLS")   nm <- "GLS"
        
        # Generate
        assign(paste0("g", i),
               sampDistro(relv[[i]], param, nm)
        )
    }
    
    # Return the patchwork object
    (g1 | g7) / (g4 | g5) / (g6 | plot_spacer())+ 
        plot_annotation(paste0(param, "'s Sampling Distribution"),
                        theme=theme(plot.title = element_text(hjust = 0.5, size = "17", face = "bold", family = "Lato"))) 
}

# Cache builder - list of all unique values for a violation
cacheList <- function(stub, fName=FALSE){
    # Just do no viol by hand, because the naming conventions are awful
    if(stub=="reg" | stub=="true"){
        running <- list("input$nObs", "input$aHat", "input$b1Hat", "input$b2Hat", "input$noise",
                        "input$ltsAlpha", "input$paramG_true", "input$sims", "input$seed")
        
        # Nuke the last two entries, if fName's true (was easier to do this way than the reverse)
        if(fName==TRUE){
            l <- length(running)
            running <- running[-c((l-2):l)] 
        }
        
    # Iterate a bunch first
    } else {
        # To deal with this slight naming mismatch
        if(stub=="endog")  stub <- "end"
        
        # Starting list of inputs to whittle down
        running <- names(input)
        
        # Keep the names with the stub
        running <- running[regexpr(paste0(stub, "$"), running)!=-1]
        
        # Toss all the copy buttons, go buttons, and selectors
        tossList <- c("copyButton", "goButton", "estSel", "modRaw", "vceSel", "canLoad", 
                      "iMat", "jMat", "drop", "reset", "randPars", "datDwn", "rawDwn")
        for(i in 1:length(tossList)){
            running <- running[regexpr(tossList[i], running)==-1]
        }
        
        # Add back in the special cases
        if(stub=="end"){
            running <- c(running, "corrXZ")
        } else if(stub=="ac") {
            running <- c(running, "acChoice", "arVal")
        } else if(stub=="ar") {
            running <- c(running, "meanU")
        } else if(stub=="heavy") {
            running <- c(running, "tDFVal")
        } else if(stub=="skewed") {
            running <- c(running, "snShape", "distChoice")
        } else if(stub=="mmodal")  {
            running <- c(running, "numModes")
        }
        
        # Toss true param AND num sims if building the file (b/c will call nSims
        # separately later) + triple check everything's alphabetized (to ensure
        # it's the same order, always)
        if(fName==TRUE){
            # toss others (including seed--will append outside of this)
            tossList <- c("paramG", "sims", "seed")
            for(i in 1:length(tossList)){
                running <- running[regexpr(tossList[i], running)==-1]
            }
            running <- unlist(running) %>% sort(.) %>% relist(., running) %>%
                        relist(., running) %>% strsplit(., split=" ")
        }    
        
        # Insert input$ in front of all the list's elements
        running <- sapply(running, function(x) { paste0("input$", x)}, USE.NAMES=FALSE)
    }
    
    # Return   
    return(list(running))
}
