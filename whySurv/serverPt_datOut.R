# Common header
DT.header.reg <- c("aHat", "b1Hat", "b2Hat", "se.aHat", "se.b1Hat", "se.b2Hat", "shape")
DT.header.weib <- c("aHat", "b1Hat", "b2Hat", "se.aHat", "se.b1Hat", "se.b2Hat", "shape", "se.shape")
   

## > NON-NORMAL ----------------------------
# Processed Results (table) ====
output$res_reg_nnorm <- renderTable(rownames=TRUE, striped=TRUE, na="NA",  {  
    MC_est <- data_reg_nnorm()[[1]] %>% data.frame()  

    if(input$model_nnorm == 1){
        MC_est <- MC_est[,1:7]
        MC_est$dm_shapeSE <- NA
        
    } else if(input$model_nnorm == 2){
        MC_est <- MC_est[,8:14]
        MC_est$dm_shapeSE <- NA
        
    } else if(input$model_nnorm == 3){
        MC_est <- MC_est[,15:22]
    } 
    
    # Print out the stuff.
    isolate(printMCs(aHat=input$aHat_nnorm, b1Hat=input$b1Hat_nnorm, b2Hat=input$b2Hat_nnorm, shape=input$noise_nnorm,
                     ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, ptEstSh=MC_est$shape,
                     seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2, seSh=MC_est$dm_shapeSE))
})  

# Raw results (DT) ====
output$table_rawOutpt_nnorm <- DT::renderDataTable({
    MC_est <- data_reg_nnorm()[[1]] %>% data.frame()
    
    if(input$model_nnorm == 1){
        MC_est <- MC_est[,1:7]
        colHdr <- "DT.header.reg"
        
    } else if(input$model_nnorm == 2){
        MC_est <- MC_est[,8:14]
        colHdr <- "DT.header.reg"
        
    } else if(input$model_nnorm == 3){
        MC_est <- MC_est[,15:22]
        colHdr <- "DT.header.weib"
        
    } 
    DT::datatable(MC_est, rownames= FALSE, colnames = eval(parse(text=colHdr)),
                  options = list(
                                 dom = 'l t p' 
                                )) %>% 
          formatRound(c(1:ncol(MC_est)), 5)
    
})
        
    
## > CENSORING ----------------------------
# Processed Results (table) ====
output$res_reg_cens <- renderTable(rownames=TRUE, striped=TRUE,  {  
    MC_est <- data_reg_cens()[[1]] %>% data.frame()

    if(input$model_cens == 1){
        MC_est <- MC_est[,1:7]
        MC_est$dm_shapeSE <- NA
        
    } else if(input$model_cens == 2){
        MC_est <- MC_est[,8:14]
        MC_est$dm_shapeSE <- NA
        
    } else if(input$model_cens == 3){
        MC_est <- MC_est[,15:22]
        
    } else if(input$model_cens == 4){
        MC_est <- MC_est[,23:30]
    }
    
    # Print out the stuff.
    isolate(printMCs(aHat=input$aHat_cens, b1Hat=input$b1Hat_cens, b2Hat=input$b2Hat_cens, shape=input$noise_cens,
                     ptEstA=MC_est$b0, ptEstB1=MC_est$b1, ptEstB2=MC_est$b2, ptEstSh=MC_est$shape,
                     seA=MC_est$se0, seB1=MC_est$se1, seB2=MC_est$se2, seSh=MC_est$dm_shapeSE ))
})

# Raw Results (DT) ====
output$table_rawOutpt_cens <- DT::renderDataTable({
    MC_est <- data_reg_cens()[[1]] %>% data.frame() 
    
    if(input$model_cens == 1){
        MC_est <- MC_est[,1:7]
        colHdr <- "DT.header.reg"
        
    } else if(input$model_cens == 2){
        MC_est <- MC_est[,8:14]
        colHdr <- "DT.header.reg"
        
    } else if(input$model_cens == 3){
        MC_est <- MC_est[,15:22]
        colHdr <- "DT.header.weib"
        
    } else if(input$model_cens == 4){
        MC_est <- MC_est[,23:30]
        colHdr <- "DT.header.reg"
    }
    
    DT::datatable(MC_est, rownames= FALSE, colnames = eval(parse(text=colHdr)),
                  options = list(
                                 dom = 'l t p' 
                                )
                  ) %>% 
          formatRound(c(1:ncol(MC_est)), 5)
})
    
 
#**********************************************
# RESULT PRINTER --------------------------
#***********************
# Col builder (rows = param statistics -> building cols by stacking rows)
colBuilder <- function(true, ptEst, se){
    row <- rbind("**True Value**"           = true,
                   "Estimated Value (Mean)" = mean(ptEst),
                   "Lower 95% CI"           = quantile(ptEst, .025),
                   "Upper 95% CI"           = quantile(ptEst, .975),
                   "Estimated SE (Mean)"    = mean(se),
                   "StDev of Estimate"      = sd(ptEst))
    
    return(row)
}

# Table builder (cols = params, rows = param statistics)
printMCs = function(aHat, b1Hat, b2Hat, shape, ptEstA, ptEstB1, ptEstB2, ptEstSh, seA, seB1, seB2, seSh) {
                
    text <- cbind(colBuilder(aHat,  ptEstA,  seA ),  # For the intercept
                  colBuilder(b1Hat, ptEstB1, seB1),  # For x
                  colBuilder(b2Hat, ptEstB2, seB2),  # For z
                  colBuilder(shape, ptEstSh, seSh)   # For the shape
                 )
    
    colnames(text)<- c("Intercept (aHat)",
                       "x's Coeff (b1Hat)",
                       "z's Coeff (b2Hat)", 
                       "Shape (pHat)")
    
    return(text)
} 