## >> NO VIOLATIONS ====
# Sampling Distros ####
output$distPlot_true <- renderPlot({  
    x <- data_reg()[[1]]
    param <- input$paramG_true

    sampDistMatrix(x, param)
})


## >> ENDOG/OV ====
# Sampling Distros ####
output$distPlot_end <- renderPlot({
    x <- data_end()[[1]]
    param <- input$paramG_end
    
    sampDistMatrix(x, param)
})


## >> SIMULT ====
# Sampling Distros ####
output$distPlot_endSim <- renderPlot({
    x <- data_endSim()[[1]]
    
    param <- input$paramG_endSim
    
    sampDistMatrix(x, param)
    
})


## >> E(u)!=0 ====
# Sampling Distros ####
output$distPlot_ar <- renderPlot({
    x <- data_ar()[[1]]
    param <- input$paramG_ar
    
    sampDistMatrix(x, param)
    
})


## >> MEAS ERR: X ====
# Sampling Distros ####
output$distPlot_endMeasErrX <- renderPlot({
    x <- data_endMeasErrX()[[1]]
    param <- input$paramG_endMeasErrX
    
    sampDistMatrix(x, param)
    
})


## >> MEAS ERR: Y ====
# Sampling Distros ####
output$distPlot_endMeasErrY <- renderPlot({
    x <- data_endMeasErrY()[[1]]
    param <- input$paramG_endMeasErrY
    
    sampDistMatrix(x, param)
    
})


## >> HETERO ====
# Sampling Distros ####
output$distPlot_hetero <- renderPlot({
    x <- data_hetero()[[1]]
    param <- input$paramG_hetero
    
    sampDistMatrix(x, param)
    
})


## >> AC ====
## Sampling Distros ####
output$distPlot_ac <- renderPlot({
    x <- data_ac()[[1]]
    param <- input$paramG_ac
    
    sampDistMatrix(x, param, "ac")
})


## >> NON-LINEAR ====
# Sampling Distros ####
output$distPlot_nlin <- renderPlot({
    x <- data_nlin()[[1]]
    param <- input$paramG_nlin
    
    sampDistMatrix(x, param)
    
})


## >> HEAVY ====
# Sampling Distros ####
output$distPlot_heavy <- renderPlot({
    
    x <- data_heavy()[[1]]
    param <- input$paramG_heavy
    
    sampDistMatrix(x, param)
    
})

# t vs. normal ####
output$errVsNorm_heavy <- renderPlot({
    
    ggplot(data=data.frame(x=seq(-4*input$noise_heavy, 4*input$noise_heavy, 0.05)), aes(x=x)) +
        stat_function(aes(color="Normal"), fun = dnorm, args = list(mean = 0, sd = input$noise_heavy),
                      size = 1,  linetype="dashed") +   
        stat_function(aes(color="t"), fun = dt, args = list(df=input$tDFVal), size = 1, linetype="solid") +
        geom_vline(xintercept=0, color="gray") + 
        scale_color_brewer(palette="Dark2") +
            theme(legend.title = element_blank(),
                  legend.position="right",
                  text = element_text(family = "Lato", size=15)) # for legend font
})


## >> SKEWED ====
# Sampling Distros ####
output$distPlot_skewed <- renderPlot({
    x <- data_skewed()[[1]]
    param <- input$paramG_skewed
    
    sampDistMatrix(x, param)
    
})

# Err vs. normal ####
output$errVsNorm_skewed <- renderPlot({
    
    input$snShape
    input$distChoice
    
    if(input$distChoice=="sknorm"){
        otherFunct <- paste0('stat_function(aes(color="Skew-Normal"), fun = dskewnorm, 
                              args = list(location = 0, 
                                          scale = ', input$noise_skewed, ',
                                          shape = ', input$snShape, '), 
                              size = 1, linetype="solid", alpha=0.65)')
    } else {
        otherFunct <- paste0('stat_function(aes(color="Chi2"), fun = dchisq, 
                              args = list(df=', input$snShape, '), 
                              size = 1, linetype="solid", alpha=0.65)')
    }
    
    ggplot(data=data.frame(x=seq(-4*input$noise_skewed, 4*input$noise_skewed, 0.05)), 
           aes(x=x)) +
        stat_function(aes(color="Normal"), fun = dnorm, args = list(mean = 0, sd = input$noise_skewed),
                      size = 1, linetype="dashed", alpha=0.65) + 
        eval(parse(text=otherFunct)) +
        geom_vline(xintercept=0, color="gray") + 
        scale_color_brewer(palette="Dark2") +
            theme(legend.title = element_blank(),
                  legend.position="right",
                  text = element_text(family = "Lato", size=15)) # for legend font
})


## >> MULTI-MODAL ====
# Sampling Distros ####
output$distPlot_mmodal <- renderPlot({
    x <- data_mmodal()[[1]]
    param <- input$paramG_mmodal
    
    sampDistMatrix(x, param)
    
})
