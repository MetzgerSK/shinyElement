# Non-normal error - Weibull (always true for this set of sims)
nnorm_OLS_data = function(nobs) {
    aHat  <- input$aHat_nnorm       # intercept
    b1Hat <- input$b1Hat_nnorm	    # for x
    b2Hat <- input$b2Hat_nnorm		# for z
    noise <- input$noise_nnorm		# how spread out the ui's are; higher values = more spread (i.e., more static)
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    u <- log(-log(1-runif(nobs)))  # TIEV min
    
    y <- exp(aHat + b1Hat*x1 + b2Hat*z + noise^-1*u)

    fail <- rep(1, nobs)
    
    data.frame(y, x1, z, fail)
}

# > CENSORING ==================
cens_OLS_data = function(nobs) {
    aHat  <- input$aHat_cens        # intercept
    b1Hat <- input$b1Hat_cens	    # for x
    b2Hat <- input$b2Hat_cens		# for z
    noise <- input$noise_cens		# how spread out the ui's are; higher values = more spread (i.e., more static)
    cens  <- input$perc_cens        # % RC obsvs
    
    z <- rnorm(nobs)
    x1 <- rnorm(nobs)
    
    u <- log(-log(1-runif(nobs)))  # TIEV min
    
    y <- exp(aHat + b1Hat*x1 + b2Hat*z + noise^-1*u)

    fail <- rep(0, nobs)
    
    # If there's censoring, impose it (quantiles currently working, so just do those)
    q <- quantile(as.data.frame(y), c((100-cens)/100), na.rm=TRUE)
    
    dat <- data.frame(y, x1, z, fail)
        dat$fail[dat$y<=q] <- 1 # yes, the event happened if y's less than the censoring point
        dat$y[dat$y>=q] <- q    # otherwise, no, it did not. (stays 0 in obsv).  Recode y's value to the censoring point.
    
    return(dat)
}