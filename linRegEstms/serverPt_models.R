# OLS
OLS_est <- function(temp_data, spec, mod) {
    # Grab model obj
    lm <- mod
    lm_coef <- coef(summary(lm))

    # Reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, vcov(lm)) 
}

# OLS w/robust SEs
OLS_rob_est <- function(temp_data, spec, mod) {
    # Grab model obj
    lm <- mod
    lm_coef <- coef(summary(lm))

    # Reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], sqrt(diag(vcovHC(lm, type="HC1"))))

    # Return
    list(vec, lm, vcovHC(lm, type="HC1")) 
}

# OLS w/HAC SEs
OLS_HAC_est <- function(temp_data, spec, mod) {
    # Grab model obj
    lm <- mod
    lm_coef <- coef(summary(lm))

    # Reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], sqrt(diag(vcovHAC(lm))))

    # Return
    list(vec, lm, vcovHAC(lm)) 
}

# WLS (pretending that we magically know the form of heterosk.)
WLS_est <- function(temp_data, spec, mod) {
    # Estimate 
    lm <- lm(spec, weights=1/abs(x1), data=temp_data)
    lm_coef <- coef(summary(lm))

    # Reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, vcov(lm)) 
}

# GLS (pretending that we magically know the form of AC + the order + AC coef)
GLS_est <- function(temp_data, spec, mod, acType, acCoef) {   
    # Getting diagonal
    gamma <- diag(1, nrow=nrow(temp_data), ncol=nrow(temp_data)) 

    # Replacing (1,1) appropriately
    gamma[1,1] <- sqrt(1 - acCoef^2)

    # Getting the mini-diagonal of rhos
    rhos <- diag(-acCoef, nrow=nrow(temp_data)-1, ncol=nrow(temp_data)-1) 
    rhos <- rbind(rep(0,ncol(rhos)), rhos)
    rhos <- cbind(rhos, rep(0,nrow(rhos)))

    # Getting final gamma matrix
    gamma <- gamma + rhos

    # Do the transformation
    yStar <- gamma %*% temp_data$y
    xStar <- temp_data %>%
        dplyr::select(., x1, z) %>%
        mutate(., ones = 1) 
    xStar <- gamma %*% as.matrix(xStar)

    # build new data frame
    temp_data2 <- data.frame(y=yStar, xStar)

    # Estimate 
    lm <- lm(y ~ ones + x1 + z - 1, data=temp_data2)
    lm_coef <- coef(summary(lm))

    # Reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, vcov(lm)) 
}

# FGLS
FGLS_est <- function(temp_data, spec, mod) {
    # Grab orig model obj
    lm.orig <- mod

    # Run auxiliary regression with log(uHat) as DV
    spec.sub <- gsub("y", "log(resid(lm.orig)^2)", spec)
    aux <- lm(as.formula(spec.sub), data=temp_data)

    # Estimate final, with weights equal to 1/exp(yHat.aux)
    lm <- lm(as.formula(spec), weight=1/exp(fitted(aux)), data=temp_data)
    lm_coef <- coef(summary(lm))

    # reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, vcov(lm)) 
}

# FGLS - AC
FGLS_AC_est <- function(temp_data, spec, acType) {
    # Do the GLM
    lm <- gls(as.formula(spec), data=temp_data, 
              correlation=eval(parse(text=acType)), method="ML")
    lm_coef <- coef(summary(lm))

    # reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, vcov(lm)) 
}

# Robust regression
robM_est <- function(temp_data, spec) {
    # Estm
    lm <- rlm(spec %ss% NULL, data=temp_data)  # Huber is default.
    lm_coef <- coef(summary(lm))

    # reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, vcov(lm)) 
}

# LTS
LTS_est <- function(temp_data, spec, lts) {
    # Estm
    lm <- ltsReg(spec %ss% NULL, alpha=lts/100, data=temp_data) 
    lm_coef <- coef(summary(lm))

    # reduce lm_coef to a single vector with values for each b and se
    vec <- reducer(lm_coef[,1], lm_coef[,2])

    # Return
    list(vec, lm, matrix(NA, ncol=nrow(lm_coef), nrow=nrow(lm_coef))) # no vcov extractor function for ltsReg
}