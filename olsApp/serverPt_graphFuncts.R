## ** GENERIC GRAPH FUNCTIONS ** ----

# sampling distros
sampDistro <- function(x, param){
    
    if (param == "aHat"){
        dat.test <- x$b0 %>%
                    data.frame(estm=., se=x$se0)
    } else if(param == "b1Hat"){
        dat.test <- x$b1 %>%
                    data.frame(estm=., se=x$se1) 
    } else if(param == "b2Hat"){
        dat.test <- x$b2 %>%
                    data.frame(estm=., se=x$se2) 
    }
    
    # current theme's default font
    font <- "Lato" 
    
    ggplot(dat.test, aes(x=estm)) + 
        geom_histogram(aes(y=..density..), fill = "gray", color = "black") +
        geom_density(alpha=.2, size = 1.05, color="#104E8B") + 
        geom_rug(alpha=0.5) + 
        stat_function(fun = dnorm, 
                      args = list(mean = mean(dat.test$estm), 
                                  sd = sd(dat.test$estm)),
                      color = "#FF4040", 
                      size = 1, 
                      linetype="dashed") + 
        xlim(mean(dat.test$estm)-3.5*sd(dat.test$estm), 
             mean(dat.test$estm)+3.5*sd(dat.test$estm) ) +
        labs(title = paste0(param, " Sampling Distribution"),
             x     = paste0(param, " Values")) + 
        theme(plot.title = element_text(hjust = 0.5, 
                                        face = "bold", 
                                        family = paste0(font)
                           )
              )
}

# uHat plots
errorPlots <- function(mod, type, sel="x", rType){
    
    # Get residual type, set y-axis label
    if(rType=="reg"){
        resids <- resid(mod)
        resTitle <- "uHat" 
        dist <- "Normal"
        
        # Hist-specific
        compDistro <- 'dnorm(., mean(.), sd(.))'
        histColor <- "#FF4040"
        scaleLab <- "Normal"
        
    } else if(rType=="stand") {
        resids <- stdres(mod)
        resTitle <- "Standardized uHat"
        dist <- "Normal"
        
        # Hist-specific
        compDistro <- 'dnorm(., mean(.), sd(.))'
        histColor <- "#FF4040"
        scaleLab <- "Normal"
        
    } else {    # if not regular or standard, must be studentized
        resids <- studres(mod)  
        resTitle <- "Studentized uHat"
        dist <- "t"
        
        # Hist-specific
        compDistro <- 'dt(., length(.)-mod$rank-1)'
        histColor <- paste0("t w/", length(studres(mod))-mod$rank-1, " d.f.")
                            # NOTE: rank = k + 1.  So, only need to subtract 1 to get n-k-2.
        scaleLab <- histColor  # same, in the case of Studentized
        
    }
    
    # Start plotting
    if (type == "hist"){
        dat.test <- resids %>%
                        data.frame(uHat=., distTrue = eval(parse(text=compDistro)))

        ggplot(dat.test, aes(x=uHat)) + 
            geom_histogram(aes(y=..density..), fill = "gray", color = "black") +
            geom_density(alpha=.2, size = 1.05, color="#104E8B") + 
            geom_rug(alpha=0.5) + 
            geom_line(aes(x = uHat, y = distTrue, color = histColor), 
                                                  size = 1, linetype="dashed") +
            scale_color_brewer(name="",
                               palette="Dark2",
                               labels=paste0(scaleLab)) +
            labs(x=resTitle) 
        
    } else if (type == "qq"){
        # Semi-brute forcing, because R's just not having the way in which distStmt's getting passed (even with quote())
        gg <- ggplot(data = data.frame(uHat=resids), mapping = aes(sample=uHat)) +
                labs(title = paste0("Q-Q Plot: ", resTitle, " vs. ", scaleLab), 
                     x     = paste0(dist, " Distrib. Quantiles"), y = "Residual Quantiles") +
                theme(plot.title=element_text(hjust = 0.5, face = "bold"), aspect.ratio=1)
 
        if(rType=="stud"){
            dfT <- length(studres(mod))-mod$rank-1
            gg <- gg +  stat_qq_point(distribution="t", dparams=c(df=dfT)) + 
                        stat_qq_line( distribution="t", dparams=c(df=dfT)) + 
                        stat_qq_band( distribution="t", dparams=c(df=dfT), alpha=0.5)  
        } else {
            gg <- gg +  stat_qq_point() + 
                        stat_qq_line() + 
                        stat_qq_band(alpha=0.5)  
        }
        rng <- ggplot_build(gg)$layout$panel_scales_x[[1]]$range$range + c(-0.25,0.25) 
        
        gg + coord_cartesian(xlim = rng, ylim=rng) 
        
     } else if (type == "pp") {
        # Semi-brute forcing, because R's just not having the way in which distStmt's getting passed (even with quote())
        gg <- ggplot(data = data.frame(uHat=resids), mapping = aes(sample=uHat)) +
                labs(title=paste0("P-P Plot: ", resTitle, " vs. ", scaleLab), 
                     x = paste0(scaleLab, " CDF"), y = paste0(resTitle, " EDF")) +
                theme(plot.title=element_text(hjust = 0.5, face = "bold"), aspect.ratio=1)
 
        if(rType=="stud"){
            gg <- gg +  stat_pp_point(distribution="t", dparams=c(df=length(studres(mod))-mod$rank-1)) + 
                         stat_pp_line(distribution="t", dparams=c(df=length(studres(mod))-mod$rank-1)) + 
                         stat_pp_band(distribution="t", dparams=c(df=length(studres(mod))-mod$rank-1), alpha=0.5)  
        } else {
            gg <- gg +  stat_pp_point() + 
                        stat_pp_line() + 
                        stat_pp_band(alpha=0.5)  
        }
        
        gg 
         
        
    } else if(type == "hetero") {
        dat.temp <- data.frame(uHat=resids, covar = mod$model$x)
        
        if(sel=="z")            dat.temp$covar <- mod$model$z
        else if(sel=="yHat")    dat.temp$covar <- predict(mod)
                
        ggplot(dat.temp) + 
            geom_point(aes(x=covar, y=uHat)) +
            geom_hline(yintercept=0, linetype="solid", color="black", size=1.1) +
            labs(x=paste0(sel),
                 y=paste0(resTitle),
                 title=paste0(resTitle, " vs. ", sel)) +
            theme(plot.title=element_text(hjust = 0.5, face = "bold"))
        
    } else if (type == "acf") {
        forecast::ggAcf(x = data.frame(uHat=resids)) + 
            labs(title=paste0(resTitle, ": Autocorrelation Function")) +
            theme(plot.title=element_text(hjust = 0.5, face = "bold"))
        
    } else if (type == "pacf") {
        forecast::ggPacf(x = data.frame(uHat=resids)) + 
            labs(title=paste0(resTitle, ": Partial Autocorrelation Function")) +
            theme(plot.title=element_text(hjust = 0.5, face = "bold"))
    }
}


# y vs. yHat
yVsyHat <- function(mod, params=NULL, yTrueX=NULL, end=FALSE){
    mod.dat <- mod$model
    
    dat.temp <- data.frame( yHat = predict(mod, newdata=data.frame(x1=mod.dat$x, z=rep(0, nrow(mod.dat)))),
                            yTrue = params$aHat + params$b1Hat*mod.dat$x,
                            yObsv = mod.dat$y,
                            x = mod.dat$x,
                            xMod = mod.dat$x
                        )
    
    # If yTrue's defined, insert that into the data frame instead
    # (mainly intended for the simultaneity DGP, since the reduced-form is lengthy.)
    if(!is.null(yTrueX)){
        dat.temp$yTrue <- yTrueX$yTr
        dat.temp$x     <- yTrueX$xTr
    }
    
    # Caption for graph (so long as it's not the OV spec)
    capt <- ifelse(end==FALSE, "Other Covariate Values: \\( z = 0\\)", "")
    
    # graph
    gg <- ggplot(dat.temp) + 
            geom_point(aes(x=xMod, y=yObsv,  
                            text = paste0(  "xObsv: ", round(dat.temp$xMod, 3), "\n",
                                            ifelse(dat.temp$xMod!=dat.temp$x, 
                                                   paste0("xTrue: ", round(dat.temp$x, 3), "\n"),
                                                   ""),
                                            "yTrue: ", round(dat.temp$yTrue, 3), "\n",
                                            "- - - - - - - - - - - \n",
                                            "yObsv: ", round(dat.temp$yObsv, 3), "\n",
                                            "yHat: ", round(dat.temp$yHat, 3), "\n")
                          )) +
            geom_line(aes(x=x,    y=yTrue, color="True DGP"),     alpha=.8, size=1, linetype="dash") +
            geom_line(aes(x=xMod, y=yHat,  color="Model's yHat"), alpha=.8, size=1) + 
            scale_color_brewer(palette="Dark2", guide = guide_legend(title = NULL)) +
            labs(x = "<em>x</em>",
                 y = "<em>y</em>",
                 caption = capt) +
            theme(legend.title = element_blank(),
                  text = element_text(family = "Lato", size=15)) # for legend font
    
    #browser()    
    ggplotly(gg, tooltip="text") %>%
             style(hoverlabel = list(font = list(family="Lato", size="15"))) # for tooltip font
}

## For the next helper functions
pages <- c( "reg",  
            "end", "endSim", "ar", "endMeasErrX", "endMeasErrY", 
            "hetero", "ac", 
            "nlin",
            "heavy", "skewed", "mmodal")

# Crafting the render objects for the error graph reactive
for(i in 1:length(pages)){
  # ******* To dynamically resize things ***********
    # craft the string
    functStr <-paste0(
        "output$distPlotErr_", pages[i], "UI <- renderUI({
            distPlotErr_", pages[i], "()    
        })"
    )
    
    # execute it
    eval(parse(text=functStr))
}

