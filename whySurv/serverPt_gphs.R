## THE GENERIC GRAPH FUNCTION ----
sampDistro <- function(x, param){
    
    if (param == "aHat"){
        dat.test <- x$b0 %>%
                    data.frame(estm=., se=x$se0)
        
        pTitle <- pAxis <- param
        
    } else if(param == "b1Hat"){
        dat.test <- x$b1 %>%
                    data.frame(estm=., se=x$se1) 
        
        pTitle <- pAxis <- param
        
    } else if(param == "b2Hat"){
        dat.test <- x$b2 %>%
                    data.frame(estm=., se=x$se2)
        
        pTitle <- pAxis <- param
        
    } else if(param =="Shape (pHat)"){
        dat.test <- x$shape %>%
                    data.frame(estm=., se=sd(.))
                    # ^ used SD of shape b/c faster
        pTitle <- "Shape"
        pAxis <- "pHat"
    }
    
    hist(dat.test$estm, col = 'darkgray',border = 'white',
                 breaks = max(ceiling(input$sims/15),5), 
                 xlim = range( (mean(dat.test$estm) -5*mean(dat.test$se)),
                               (mean(dat.test$estm) +5*mean(dat.test$se)) 
                             ), 
                 main = paste0(pTitle,"'s Sampling Distribution"), 
                 xlab = paste0(pAxis, " Values")
        )
      
}    
    

## > NON-NORMAL PLOT ----
output$distPlot_nnorm <- renderPlot({
    x <- data_reg_nnorm()[[1]] %>% data.frame()
    
    if(input$model_nnorm == 1){
        x <- x[,1:7]
    } else if(input$model_nnorm == 2){
        x <- x[,8:14]    
    } else if(input$model_nnorm == 3){
        x <- x[,15:22]
    }
    
    sampDistro(x, input$paramG_nnorm)  
    
})
    
    
## > CENSORED ----
output$distPlot_cens <- renderPlot({
    x <- data_reg_cens()[[1]] %>% data.frame()
    
    if(input$model_cens == 1){
        x <- x[,1:7]
    } else if(input$model_cens == 2){
        x <- x[,8:14]
    } else if(input$model_cens == 3){
        x <- x[,15:22]
    } else if(input$model_cens == 4){
        x <- x[,23:30]
    }
    
    sampDistro(x, input$paramG_cens)  
})