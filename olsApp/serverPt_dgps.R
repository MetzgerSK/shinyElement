# No Violatons ====
true_OLS_data = function(nobs) {
    
    aHat <- input$aHat          # intercept
    b1Hat <- input$b1Hat	    # for x
    b2Hat <- input$b2Hat		# for z
    noise <- input$noise		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    e = rnorm(nobs) 
    
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e
    data.frame(y, x1, z)
}


# Omitted Variable Bias ====
endog_OLS_data = function(nobs) { 
    
    aHat <- input$aHat_end          # intercept
    b1Hat <- input$b1Hat_end	    # for x
    b2Hat <- input$b2Hat_end		# for z
    noise <- input$noise_end		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    CorrXZ <- input$corrXZ
    
    zTemp = rnorm(nobs)
    x1 <- rnorm(nobs)
    
    #define Z
    z = CorrXZ * x1 + sqrt((1-CorrXZ^2)) * zTemp	# inducing correlation between x and z
    
    e = rnorm(nobs)
    
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e

    data.frame(y, x1, z)
}


# Simultaneity ====
endSim_OLS_data = function(nobs) { 
    
    # y's eq
    aHat  <- input$aHat_endSim      # intercept
    b1Hat <- input$b1Hat_endSim	    # for x
    b2Hat <- input$b2Hat_endSim		# for z
    b3Hat <- 0                      # for z1 (y's unique instrument) -> turning off [input$yInstr_endSim]
    noise <- input$noise_endSim		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    # x's eq
    aHatX  <- input$aHatX_endSim    # intercept
    alpha2 <- input$alpha2_endSim   # for y's effect
    b2HatX <- input$b2HatX_endSim   # for z
    b3HatX <- input$xInstr_endSim   # for z2 (x's unique instrument)
    noiseX <- input$noiseX_endSim   # spread for u's structural error
    
    # Set lavaan model parameters
    ## NOTE: lavaan notation.  y = endogenous variables, x = exogenous.
    modStr <- paste0('
            # DGPs, sans intercepts
            y1 ~ (', b1Hat,  ')*y2 + (', b2Hat,  ')*x1
            y2 ~ (', alpha2, ')*y1 + (', b2HatX, ')*x1 + (', b3HatX, ')*x2  
    ')
    
    # Generate
    dat <- simulateData(modStr, sample.nobs = nobs)
    
    # Add in intercepts ex post, to sidestep lavaan's on-and-off rebellion
    dat <- dat %>% mutate(., y1 = y1 + aHat ,
                             y2 = y2 + aHatX)
    
    # Return data
    data.frame(y=dat$y1, x1=dat$y2, z=dat$x1, z2=dat$x2)
}

# Meas Err: X ====
endMeasErrX_OLS_data = function(nobs) { 
    
    # y's eq
    aHat  <- input$aHat_endMeasErrX      # intercept
    b1Hat <- input$b1Hat_endMeasErrX	 # for x
    b2Hat <- input$b2Hat_endMeasErrX	 # for z
    noise <- input$noise_endMeasErrX	 # how spread out the ui's are; higher values = more spread (i.e., more static)
    
    # x's eq
    noiseX <- input$noiseX_endMeasErrX   # spread for u's structural error
    
    # exog covars
    x = rnorm(nobs)
    z <- rnorm(nobs)
    
    # error draw
    e = rnorm(nobs)
    
    # y
    y = aHat + b1Hat*x + b2Hat*z + noise*e

    # Mis-measured x
    x1 <- x + noiseX*rnorm(nobs)
    
    # Return data (NOTE: mismeas X, not true X)
    data.frame(y, x1, z, xTrue=x)
}

# Meas Err: Y ====
endMeasErrY_OLS_data = function(nobs) { 
    
    # y's eq
    aHat  <- input$aHat_endMeasErrY     # intercept
    b1Hat <- input$b1Hat_endMeasErrY	# for x
    b2Hat <- input$b2Hat_endMeasErrY    # for z
    noise <- input$noise_endMeasErrY	# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    # bonus noise in y
    bonus <- input$bonusNoise_endMeasErrY   # spread for u's structural error
    
    # exog covars
    x1 <- rnorm(nobs)
    z <- rnorm(nobs)
    
    # error draw
    e = rnorm(nobs)
    v = rnorm(nobs)
    
    # y
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e - bonus*v
    
    # Return data
    data.frame(y, x1, z)
}

# E(u)!=0 ====
ar_OLS_data = function(nobs) { 
    aHat <- input$aHat_ar       # intercept
    b1Hat <- input$b1Hat_ar	    # for x
    b2Hat <- input$b2Hat_ar		# for z
    noise <- input$noise_ar		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    meanU <- input$meanU
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    e = noise*rnorm(nobs) + meanU
    
    y = aHat + b1Hat*x1 + b2Hat*z + e
    data.frame(y, x1, z)
}


# Heteroskedasticity  ====
##(as function of x; can code in nuance later, if desired)
hetero_OLS_data = function(nobs) { 
    aHat <- input$aHat_hetero       # intercept
    b1Hat <- input$b1Hat_hetero	    # for x
    b2Hat <- input$b2Hat_hetero		# for z
    noise <- input$noise_hetero		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    e = rnorm(nobs) * x1
    
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e
    data.frame(y, x1, z)
}


# Autocorrelation ====
ac_OLS_data = function(nobs) { 
    aHat <- input$aHat_ac       # intercept
    b1Hat <- input$b1Hat_ac	    # for x
    b2Hat <- input$b2Hat_ac		# for z
    noise <- input$noise_ac		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    ac <- input$arVal
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    if(input$acChoice=="ar")    e <- arima.sim(n=nobs, model=list(ar=c(ac))) 
    else                        e <- arima.sim(n=nobs, model=list(ma=c(ac))) 
    
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e
    data.frame(y, x1, z)
}

    # Update slider values/text, depending on which distribution's selected
    observeEvent(input$acChoice, {
        
        val <- input$arVal
        
        if(input$acChoice=="ar"){
            # change slider text (do it twice to force the MJ to render)
            updateSliderInput(session, "arVal", 
               "Autocorrelation Value \\(\\left[AR(1) \\right] \\)" , value=-99
            )
            updateSliderInput(session, "arVal", value=val)
            
        } else {
            # change slider text (do it twice to force the MJ to render)
            updateSliderInput(session, "arVal", 
               "Autocorrelation Value \\(\\left[MA(1) \\right] \\)" , value=-99
            ) 
            updateSliderInput(session, "arVal", value=val)
        }
    })


# Nonlinear in Params ====
nlin_OLS_data = function(nobs) {
    aHat <- input$aHat_nlin         # intercept
    b1Hat <- input$b1Hat_nlin	    # for x
    b2Hat <- input$b2Hat_nlin		# for z
    noise <- input$noise_nlin		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    e = rnorm(nobs)
    
    y = exp(aHat + b1Hat*x1 + b2Hat*z + noise*e)
    data.frame(y, x1, z)
}


# Non-Normal: Heavy-Tailed ====
heavy_OLS_data = function(nobs) { 
    aHat <- input$aHat_heavy       # intercept
    b1Hat <- input$b1Hat_heavy	   # for x
    b2Hat <- input$b2Hat_heavy	   # for z
    noise <- input$noise_heavy	   # how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    e <- rt(nobs, input$tDFVal) 
    
    # Ensure error still has mean 0
    e <- e - mean(e)
    
    # Gen y, store
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e
    data.frame(y, x1, z)
}

# Non-Normal: Skewed ====
skewed_OLS_data = function(nobs) { 
    aHat <- input$aHat_heavy       # intercept
    b1Hat <- input$b1Hat_heavy	   # for x
    b2Hat <- input$b2Hat_heavy	   # for z
    noise <- input$noise_heavy	   # how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    # Pull error
    if(input$distChoice=="chi2")  e <- rchisq(n=nobs, df=input$snShape)
    else                          e <- rskewnorm(n=nobs, scale=noise, shape=input$snShape); noise = 1
    
    # Demean the error, s.t. new mean is 0
    e <- e - mean(e)
    
    # gen y, store
    y = aHat + b1Hat*x1 + b2Hat*z + noise*e
    data.frame(y, x1, z)
}

    # Update slider values/text, depending on which distribution's selected
    observeEvent(input$distChoice, {
        val <- input$snShape
        
        ## Skew-normal
        if(input$distChoice=="sknorm"){
            # change slider text  (do it twice to force the MJ to render)
            updateSliderInput(session, "snShape", 
                label = "\\(\\mathcal{N}_{\\text{skew}} \\): Skew-normal's shape parameter", value=-99
            )
            updateSliderInput(session, "snShape", 
                min = -7, max = 7, step = .25, value = val
            )
            # swap explainer
            shinyjs::hide("chi2_skewed")
            shinyjs::show("snSh_skewed")
           
        ## Chi2     
        } else {
            # change slider text (do it twice to force the MJ to render)
            updateSliderInput(session, "snShape", 
                label = "\\( \\chi ^2\\)'s degrees of freedom", value=-99
            ) 
            updateSliderInput(session, "snShape", 
                min = 1, max = 12, step = 1, value = ifelse(val>0, val, .25)
            ) 
            # swap explainer
            shinyjs::show("chi2_skewed")
            shinyjs::hide("snSh_skewed")
        }
    })
    
    
# Non-Normal: Multi-Modal ====
mmodal_OLS_data = function(nobs) { 
    aHat <- input$aHat_mmodal       # intercept
    b1Hat <- input$b1Hat_mmodal	    # for x
    b2Hat <- input$b2Hat_mmodal		# for z
    noise <- input$noise_mmodal		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    # Preserve RNG state before pulling values for hump means + variances
    seed <- .GlobalEnv$.Random.seed
    set.seed(input$seed_mmodal)
    means <- runif(input$numModes, min=-10, max=10) 
   
    noisePull <- NULL
    for(i in 1:input$numModes){
        noisePull[[i]] <- ifelse(runif(1)>0.5, sample(seq(1, 5, .1),1), sample(seq(.1, .9, .1),1)) 
    }
    
    # Restore seed; continue
    .GlobalEnv$.Random.seed <- seed
    
    # Start building the ifelse for the final part (will generate error term for each mixture component)
    ifStmt <- ""
    for(i in 1:input$numModes){
        assign(paste0("e",i), noise*noisePull[[i]]*rnorm(nobs) + means[i])  # runif to give us some different widths for the distro
        
        if(i<input$numModes){
            if(i+1==input$numModes){
                piece <- paste0("ifelse(mix==", i,", e", i, ", e", i+1, strrep(")", input$numModes-1) )
            } else {
                piece <- paste0("ifelse(mix==", i,", e", i, ",")
            }    
            
            ifStmt <- paste0(ifStmt, piece)
        }
    }
       
    # pull mixture probs 
    mix <- sample.int(input$numModes, size=nobs, replace=TRUE, prob=rep(1/input$numModes, input$numModes))
     
    # form up the error term
    e <- eval(parse(text=ifStmt)) 
    noise <- 1 # because have accounted for dispersion above

    # Demean error s.t. new mean is 0
    e <- e - mean(e)
    
    # gen y, store
    y <- aHat + b1Hat*x1 + b2Hat*z + noise*e
    data.frame(y, x1, z)
}    