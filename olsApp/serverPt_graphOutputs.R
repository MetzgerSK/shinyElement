## >> NO VIOLATIONS ====
# Sampling Distros ####
output$distPlot_true <- renderPlot({
    x <- data_reg()[[1]]
    param <- input$paramG_true
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_reg()[[1]]
observeEvent(input$goButton, {
    updateSliderInput(session, "selectSim_plotErrDist_reg", max=nrow(data_reg()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_reg"   , max=nrow(data_reg()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_reg, {
    toggle(id="plotErrDist_heteroPlot_reg", condition=input$plotErrDist_reg=="hetero")
})

# Error Plots ####
## (written this way to dynamically size the returned plotOutput object) ##
distPlotErr_reg <- reactive({
    output$distPlotErr_reg <-renderPlot({
    
    draw <- input$selectSim_plotErrDist_reg
    mod <- data_reg()[[2]][[draw]]
    
    type <- input$plotErrDist_reg
    sel <- input$plotErrDist_heteroPlot_reg
    
    errorPlots(mod, type, sel, rType=input$residSel_reg)
        
    })
    
    type <- input$plotErrDist_reg
    ifelse(sum(type==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_reg", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_regUI

# y vs. yHat Plots ####
output$distPlotYHat_reg <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_reg
    mod <- data_reg()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})


## >> ENDOG/OV ====
# Sampling Distros ####
output$distPlot_end <- renderPlot({
    x <- data_end()[[1]]
    param <- input$paramG_end
    
    sampDistro(x, param)
})

# Update slider, based on # of rows in data_end()[[1]]
observeEvent(input$goButton_end, {
    updateSliderInput(session, "selectSim_plotErrDist_end", max=nrow(data_end()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_end"   , max=nrow(data_end()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_end, {
    toggle(id="plotErrDist_heteroPlot_end", condition=input$plotErrDist_end=="hetero")
})

# Error Plots ####
## (written this way to dynamically size the returned plotOutput object) ##
distPlotErr_end <- reactive({
    output$distPlotErr_end <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_end
        mod <- data_end()[[2]][[draw]]
        
        type <- input$plotErrDist_end
        sel <- input$plotErrDist_heteroPlot_end
        
        errorPlots(mod, type, sel, rType=input$residSel_end)
        
    })
    
    sel <- input$plotErrDist_end
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_end", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_endUI

# y vs. yHat Plots ####
output$distPlotYHat_end <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_end
    mod <- data_end()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params, end=TRUE)
    
})


## >> SIMULT ====
# Sampling Distros ####
output$distPlot_endSim <- renderPlot({
    x <- data_endSim()[[1]] %>% data.frame()
    
    param <- input$paramG_endSim
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_endSim()[[1]]
observeEvent(input$goButton_endSim, {
    updateSliderInput(session, "selectSim_plotErrDist_endSim", max=nrow(data_endSim()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_endSim"   , max=nrow(data_endSim()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_endSim, {
    toggle(id="plotErrDist_heteroPlot_endSim", condition=input$plotErrDist_endSim=="hetero")
})

# Error Plots ####
distPlotErr_endSim <- reactive({
    output$distPlotErr_endSim <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_endSim
        mod <- data_endSim()[[2]][[draw]]
        
        type <- input$plotErrDist_endSim
        sel <- input$plotErrDist_heteroPlot_endSim
        
        errorPlots(mod, type, sel, rType=input$residSel_endSim)
        
    })
    
    sel <- input$plotErrDist_endSim
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_endSim", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_endSimUI
    
# y vs. yHat Plots ####
output$distPlotYHat_endSim <- renderPlotly({
    
    # Backwards solver: given a specific value of x, with all 
    # other parameters fixed, figure out what size shock you'd
    # approximately need
    irfX <- function(xVal, gamma, a2Hat, aHat, aHatX, noiseX){
        
        # Solve out
        err <- noiseX^{-1} * (xVal/gamma - (a2Hat-1)*aHat - aHatX)
        
        # Return
        return(err)
    }
    
    # proceed as usual, now
    draw <- input$selectSim_plotYHat_endSim
    mod <- data_endSim()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    
    # Generate yTrue here (since crazy complex b/c of the reduced form)
    # y's eq
    aHat  <- input$aHat_endSim      # intercept
    b1Hat <- input$b1Hat_endSim	    # for x
    b2Hat <- input$b2Hat_endSim		# for z
   
    # x's eq
    aHatX  <- input$aHatX_endSim    # intercept
    alpha2 <- input$alpha2_endSim   # for y's effect
    b2HatX <- input$b2HatX_endSim   # for z
    noiseX <- input$noiseX_endSim   # spread for u's structural error
    
    # Gamma
    gamma <- (b1Hat + alpha2 - b1Hat*alpha2)^-1

    # Need to figure out how much to shock x, to get comparable range of
    # x values to what we actually observe
    minShock <- irfX(min(mod$model$x), gamma, alpha2, aHat, aHatX, noiseX)
    maxShock <- irfX(max(mod$model$x), gamma, alpha2, aHat, aHatX, noiseX)
    
    # Set eX values
    eX <- irfX(mod$model$x, gamma, alpha2, aHat, aHatX, noiseX) 
    
    # Lin combos - exog + errs only (z = 0 for the scenario, remember)
    linComb1 <- aHat  
    linComb2 <- aHatX + noiseX*eX 

    # Gen y and x via their reduced form eqs
    yTr <- gamma * ( (linComb1) + (b1Hat-1) *(linComb2) )
    xTr <- gamma * ( (linComb2) + (alpha2-1)*(linComb1) )
    
    yVsyHat(mod, params, yTrueX = data.frame(yTr=yTr,xTr=xTr))
    
})


## >> E(u)!=0 ====
# Sampling Distros ####
output$distPlot_ar <- renderPlot({
    x <- data_ar()[[1]]
    param <- input$paramG_ar
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_ar()[[1]]
observeEvent(input$goButton_ar, {
    updateSliderInput(session, "selectSim_plotErrDist_ar", max=nrow(data_ar()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_ar"   , max=nrow(data_ar()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_ar, {
    toggle(id="plotErrDist_heteroPlot_ar", condition=input$plotErrDist_ar=="hetero")
})

# Error Plots ####
distPlotErr_ar <- reactive({
    output$distPlotErr_ar <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_ar
        mod <- data_ar()[[2]][[draw]]
        
        type <- input$plotErrDist_ar
        sel <- input$plotErrDist_heteroPlot_ar
        
        errorPlots(mod, type, sel, rType=input$residSel_ar)
        
    })

    sel <- input$plotErrDist_ar
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_ar", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_arUI
    
# y vs. yHat Plots ####
output$distPlotYHat_ar <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_ar
    mod <- data_ar()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})


## >> MEAS ERR: X ====
# Sampling Distros ####
output$distPlot_endMeasErrX <- renderPlot({
    x <- data_endMeasErrX()[[1]]
    param <- input$paramG_endMeasErrX
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_endMeasErrX()[[1]]
observeEvent(input$goButton_endMeasErrX, {
    updateSliderInput(session, "selectSim_plotErrDist_endMeasErrX", max=nrow(data_endMeasErrX()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_endMeasErrX"   , max=nrow(data_endMeasErrX()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_endMeasErrX, {
    toggle(id="plotErrDist_heteroPlot_endMeasErrX", condition=input$plotErrDist_endMeasErrX=="hetero")
})

# Error Plots ####
distPlotErr_endMeasErrX <- reactive({
    output$distPlotErr_endMeasErrX <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_endMeasErrX
        mod <- data_endMeasErrX()[[2]][[draw]]
        
        type <- input$plotErrDist_endMeasErrX
        sel <- input$plotErrDist_heteroPlot_endMeasErrX
        
        errorPlots(mod, type, sel, rType=input$residSel_endMeasErrX)
    
    })
    
    sel <- input$plotErrDist_endMeasErrX
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_endMeasErrX", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_endMeasErrXUI

# y vs. yHat Plots ####
output$distPlotYHat_endMeasErrX <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_endMeasErrX
    mod <- data_endMeasErrX()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    # Because we'll need to use true X instead of measured X
    # for the plots:
    xTr = data_endMeasErrX()[[3]][[draw]]
    yTr = input$aHat + input$b1Hat*xTr
    yVsyHat(mod, params, yTrueX=data.frame(yTr=yTr, xTr=xTr))
    
})


## >> MEAS ERR: Y ====
# Sampling Distros ####
output$distPlot_endMeasErrY <- renderPlot({
    x <- data_endMeasErrY()[[1]]
    param <- input$paramG_endMeasErrY
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_endMeasErrY()[[1]]
observeEvent(input$goButton_endMeasErrY, {
    updateSliderInput(session, "selectSim_plotErrDist_endMeasErrY", max=nrow(data_endMeasErrY()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_endMeasErrY"   , max=nrow(data_endMeasErrY()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_endMeasErrY, {
    toggle(id="plotErrDist_heteroPlot_endMeasErrY", condition=input$plotErrDist_endMeasErrY=="hetero")
})

# Error Plots ####
distPlotErr_endMeasErrY <- reactive({
    output$distPlotErr_endMeasErrY <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_endMeasErrY
        mod <- data_endMeasErrY()[[2]][[draw]]
        
        type <- input$plotErrDist_endMeasErrY
        sel <- input$plotErrDist_heteroPlot_endMeasErrY
        
        errorPlots(mod, type, sel, rType=input$residSel_endMeasErrY)
     
    })
    
    sel <- input$plotErrDist_endMeasErrY
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_endMeasErrY", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_endMeasErrYUI
    
# y vs. yHat Plots ####
output$distPlotYHat_endMeasErrY <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_endMeasErrY
    mod <- data_endMeasErrY()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})


## >> HETERO ====
# Sampling Distros ####
output$distPlot_hetero <- renderPlot({
    x <- data_hetero()[[1]]
    param <- input$paramG_hetero
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_hetero()[[1]]
observeEvent(input$goButton_hetero, {
    updateSliderInput(session, "selectSim_plotErrDist_hetero", max=nrow(data_hetero()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_hetero"   , max=nrow(data_hetero()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_hetero, {
    toggle(id="plotErrDist_heteroPlot_hetero", condition=input$plotErrDist_hetero=="hetero")
})

# Error Plots ####
distPlotErr_hetero <- reactive({
    output$distPlotErr_hetero <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_hetero
        mod <- data_hetero()[[2]][[draw]]
        
        type <- input$plotErrDist_hetero    
        sel <- input$plotErrDist_heteroPlot_hetero
        
        errorPlots(mod, type, sel, rType=input$residSel_hetero)
        
    })

    sel <- input$plotErrDist_hetero
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_hetero", eval(parse(text=w)))
})

    # renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_heteroUI

# y vs. yHat Plots ####
output$distPlotYHat_hetero <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_hetero
    mod <- data_hetero()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})


## >> AC ====
## Sampling Distros ####
output$distPlot_ac <- renderPlot({
    x <- data_ac()[[1]]
    param <- input$paramG_ac
    
    sampDistro(x, param)
})

# Update slider, based on # of rows in data_ac()[[1]]
observeEvent(input$goButton_ac, {
    updateSliderInput(session, "selectSim_plotErrDist_ac", max=nrow(data_ac()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_ac"   , max=nrow(data_ac()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_ac, {
    toggle(id="plotErrDist_heteroPlot_ac", condition=input$plotErrDist_ac=="hetero")
})

# Error Plots ####
distPlotErr_ac <- reactive({
    output$distPlotErr_ac <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_ac
        mod <- data_ac()[[2]][[draw]]
        
        type <- input$plotErrDist_ac
        sel <- input$plotErrDist_heteroPlot_ac
        
        errorPlots(mod, type, sel, rType=input$residSel_ac)
    })
    
    sel <- input$plotErrDist_ac
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_ac", eval(parse(text=w)))
})
    
	# renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_acUI

# y vs. yHat Plots ####
output$distPlotYHat_ac <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_ac
    mod <- data_ac()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})


## >> NON-LINEAR ====
# Sampling Distros ####
output$distPlot_nlin <- renderPlot({
    x <- data_nlin()[[1]]
    param <- input$paramG_nlin
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_nlin()[[1]]
observeEvent(input$goButton_nlin, {
    updateSliderInput(session, "selectSim_plotErrDist_nlin", max=nrow(data_nlin()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_nlin"   , max=nrow(data_nlin()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_nlin, {
    toggle(id="plotErrDist_heteroPlot_nlin", condition=input$plotErrDist_nlin=="hetero")
})

# Error Plots ####
distPlotErr_nlin <- reactive({
    output$distPlotErr_nlin <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_nlin
        mod <- data_nlin()[[2]][[draw]]
        
        type <- input$plotErrDist_nlin
        sel <- input$plotErrDist_heteroPlot_nlin
        
        errorPlots(mod, type, sel, rType=input$residSel_nlin)
        
    })
    sel <- input$plotErrDist_nlin
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_nlin", eval(parse(text=w)))
})
    
	# renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_nlinUI

# y vs. yHat Plots ####
output$distPlotYHat_nlin <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_nlin
    mod <- data_nlin()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    xTr = mod$model$x
    yTr = exp(input$aHat + input$b1Hat*xTr)
    
    yVsyHat(mod, params, yTrueX = data.frame(yTr=yTr, xTr=xTr))
    
})

## >> HEAVY ====
# Sampling Distros ####
output$distPlot_heavy <- renderPlot({
    
    x <- data_heavy()[[1]]
    param <- input$paramG_heavy
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_heavy()[[1]]
observeEvent(input$goButton_heavy, {
    updateSliderInput(session, "selectSim_plotErrDist_heavy", max=nrow(data_heavy()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_heavy"   , max=nrow(data_heavy()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_heavy, {
    toggle(id="plotErrDist_heteroPlot_heavy", condition=input$plotErrDist_heavy=="hetero")
})

# Error Plots ####
distPlotErr_heavy <- reactive({
    output$distPlotErr_heavy <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_heavy
        mod <- data_heavy()[[2]][[draw]]
    
        type <- input$plotErrDist_heavy
        sel <- input$plotErrDist_heteroPlot_heavy
        
        errorPlots(mod, type, sel, rType=input$residSel_heavy)
        
    })
    sel <- input$plotErrDist_heavy
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_heavy", eval(parse(text=w)))
})
    
	# renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_heavyUI

# y vs. yHat Plots ####
output$distPlotYHat_heavy <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_heavy
    mod <- data_heavy()[[2]][[draw]]

    params <- list(aHat=input$aHat_heavy, b1Hat=input$b1Hat_heavy)
    
    yVsyHat(mod, params)
    
})

# Err vs. normal ####
output$errVsNorm_heavy <- renderPlot({
    
    ggplot(data=data.frame(x=seq(-4*input$noise_skewed, 4*input$noise_skewed, 0.05)), aes(x=x)) +
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
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_skewed()[[1]]
observeEvent(input$goButton_skewed, {
    updateSliderInput(session, "selectSim_plotErrDist_skewed", max=nrow(data_skewed()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_skewed"   , max=nrow(data_skewed()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_skewed, {
    toggle(id="plotErrDist_heteroPlot_skewed", condition=input$plotErrDist_skewed=="hetero")
})

# Error Plots ####
distPlotErr_skewed <- reactive({
    output$distPlotErr_skewed <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_skewed
        mod <- data_skewed()[[2]][[draw]]
        
        type <- input$plotErrDist_skewed    
        sel <- input$plotErrDist_heteroPlot_skewed
        
        errorPlots(mod, type, sel, rType=input$residSel_skewed)
        
    })

    sel <- input$plotErrDist_skewed
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_skewed", eval(parse(text=w)))
})
    
	# renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_skewedUI

# y vs. yHat Plots ####
output$distPlotYHat_skewed <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_skewed
    mod <- data_skewed()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})

# Err vs. normal ####
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
    
    sampDistro(x, param)
    
})

# Update slider, based on # of rows in data_mmodal()[[1]]
observeEvent(input$goButton_mmodal, {
    updateSliderInput(session, "selectSim_plotErrDist_mmodal", max=nrow(data_mmodal()[[1]]))
    updateSliderInput(session, "selectSim_plotYHat_mmodal"   , max=nrow(data_mmodal()[[1]]))
}) 

# Update whether to hide or show the dropdown for the hetero plot
observeEvent(input$plotErrDist_mmodal, {
    toggle(id="plotErrDist_heteroPlot_mmodal", condition=input$plotErrDist_mmodal=="hetero")
})

# Error Plots ####
distPlotErr_mmodal <- reactive({
    output$distPlotErr_mmodal <- renderPlot({
        
        draw <- input$selectSim_plotErrDist_mmodal
        mod <- data_mmodal()[[2]][[draw]]
        
        type <- input$plotErrDist_mmodal    
        sel <- input$plotErrDist_heteroPlot_mmodal
        
        errorPlots(mod, type, sel, rType=input$residSel_mmodal)
        
    })
    sel <- input$plotErrDist_mmodal
    ifelse(sum(sel==c("qq","pp"))==1, {w <- "width='400px'"}, {w <- "width='100%'"})
        # NOTE: width as 100% is the default.
        
    plotOutput("distPlotErr_mmodal", eval(parse(text=w)))
})
    
	# renderUI to dynamically size things: now generic function in "serverPt_graphFuncts.R"
    # Resultant name: output$distPlotErr_skewedUI

# y vs. yHat Plots ####
output$distPlotYHat_mmodal <- renderPlotly({
    
    draw <- input$selectSim_plotYHat_mmodal
    mod <- data_mmodal()[[2]][[draw]]
    
    params <- list(aHat=input$aHat, b1Hat=input$b1Hat)
    
    yVsyHat(mod, params)
    
})
