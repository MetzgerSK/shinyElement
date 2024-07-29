#*********************************************
# ALL GLOBAL LISTS/VECTORS
#**************************
# Min/max/step values for the nObs sliders (b/c these will now be referenced
# across multiple files -> define in one place, so I don't go nuts if I make changes)
    smMin <- 9
    smMax <- 30
    smDel <- 1
                
    lgMin <- 20
    lgMax <- 500
    lgDel <- 10
        
    
# Raw Sim Output: model results selector
    ## Main variant
    modList <- list( "OLS" = 1, 
                     "OLS w/robust SEs" = 2,
                     "OLS w/robust + AC-corrected SEs" = 3,
                     "WLS" = 7,
                     "FGLS" = 4,
                     "M-estimation (Huber)" = 5,
                     "Least Trimmed Squares" = 6)

    ## AC variant
    modListAC <- modList
    names(modListAC)[which(modList==7)] <- "GLS"
    
    
# List of all tabs' input suffixes
tabList <- c("", "end", "ar", "endSim", "endMeasErrX", "endMeasErrY",
             "ac", "hetero", "nlin", "heavy", "skewed", "mmodal")


# Processed result table headers
    ## Main variant
    header <- c("OLS", "Rob.SE", "HAC.SE", "WLS", "FGLS", "M.estm", "LTS")  
    
    ## AC variant
    headerAC <- header
    headerAC[which(header=="WLS")] <- "GLS"
    

# List of column headers for DT tables
DT.header.reg <- c("aHat", "b1Hat", "b2Hat", "se.aHat", "se.b1Hat", "se.b2Hat")
DT.header.end <- DT.header.reg[c(-3,-6)]
 
   
# Main + Raw Sim Output: selecting which estimates to view
estmChoices       <- list("Intercept"="b0", "<em>x</em>'s Coeff"="b1", "<em>z</em>'s Coeff"="b2")
estmChoices_endog <- estmChoices[-3]