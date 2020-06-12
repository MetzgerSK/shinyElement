# > WRAPPER: Define omnibus estm functions ===================
sim_est = function(temp_data) {
    set1 <- sim_est_OLS_t(temp_data)
    set2 <- sim_est_OLS_lnT(temp_data)
    set3 <- sim_est_Weib_t(temp_data)
    
    c(set1, set2, set3)
}
   
sim_est_cens = function(temp_data) {
    set1 <- sim_est_OLS_t(temp_data)
    set2 <- sim_est_OLS_lnT(temp_data)
    set3 <- sim_est_Weib_t(temp_data)
    set4 <- sim_est_intreg_lnT(temp_data)
    
    c(set1, set2, set3, set4)
}
                
## > INDV ESTM FUNCTIONS ##################
# OLS w/DV = t
sim_est_OLS_t <- function(temp_data) {
    # estimate model
    sumR <- summary(lm(y~x1+z, data=temp_data))
    lm_coef <- coef(sumR)
    
    # reduce lm_coef to a single vector with values for each b and se
    lm_b = c(lm_coef[,1])
        names(lm_b) = paste0("b",0:(length(lm_b)-1))
    lm_se = c(lm_coef[,2])
        names(lm_se) = paste0("se",0:(length(lm_se)-1))
    
    # RMSE (for the shape estimate)
    lm_sh <- 1/sumR$sigma
        names(lm_sh) = paste0("shape",0:(length(lm_sh)-1))
    
    # return
    return(c(lm_b, lm_se, lm_sh))
}

# OLS w/DV = ln(t)
sim_est_OLS_lnT <- function(temp_data) {
    # estimate model
    sumR <- summary(lm(log(y) ~ x1 + z, data=temp_data))
    lm_coef <- coef(sumR)
    
    # reduce lm_coef to a single vector with values for each b and se
    lm_b <- c(lm_coef[,1])
        names(lm_b) <- paste0("b",0:(length(lm_b)-1))
    lm_se <- c(lm_coef[,2])
        names(lm_se) <- paste0("se",0:(length(lm_se)-1))
    
    # RMSE (for the shape estimate)
    lm_sh <- 1/sumR$sigma
        names(lm_sh) = "shape" 
    
    # return
    return(c(lm_b, lm_se, lm_sh))
}

# Weibull (AFT) w/DV = t
sim_est_Weib_t <- function(temp_data) {
    # estimate model
    model <- survreg(Surv(y, fail) ~ x1 + z, data=temp_data, dist="weibull")  # reports back in AFT
    
    # true # of covariates (cuts off ln(shape))
    cols <- length(c(summary(model)$table[,2]))
    
    # reduce lm_coef to a single vector with values for each b and se
    lm_b <- c(summary(model)$table[1:cols-1,1])
        names(lm_b) <- paste0("b",0:(length(lm_b)-1))
    lm_se <- c(summary(model)$table[1:cols-1,2])
        names(lm_se) <- paste0("se",0:(length(lm_se)-1))
    
    # the shape 
    lm_sh <- 1/summary(model)$scale
        names(lm_sh) <- "shape" 
    lm_shSE <- deltamethod(~ 1/exp(x1), summary(model)$table[4,1], (summary(model)$table[4,2])^2)
        names(lm_shSE) <- "dm_shapeSE" 
        
    return(c(lm_b, lm_se, lm_sh, lm_shSE))  
}

# Censored regr w/DV = ln(t)
sim_est_intreg_lnT <- function(temp_data) {
    # force into interval2 format for survreg
    temp_data <- temp_data %>%
                    mutate(left = log(y),
                           right = case_when(fail==1 ~ log(y),
                                             fail!=1 ~ Inf)
                           )

    # now run everything
    model <- survreg(Surv(left, right, type="interval2") ~ x1 + z, 
                        data=temp_data, dist="gaussian" )  # reports back in AFT  
    mod_sum <- summary(model)
    
    # reduce lm_coef to a single vector with values for each b and se
    lm_b <- coef(mod_sum)
        names(lm_b) <- paste("b",0:(length(lm_b)-1),sep="")
    lm_se <- c(mod_sum$table[1:length(lm_b),2])
        names(lm_se) <- paste("se",0:(length(lm_se)-1),sep="")
    
    # the encore: the shape
    lm_sh <- 1/mod_sum$scale
        names(lm_sh) <- "shape" 
    lm_shSE <- deltamethod(~ 1/exp(x1), 
                            mod_sum$table[length(lm_b)+1,1], 
                           (mod_sum$table[length(lm_b)+1,2])^2) 
        names(lm_shSE) <- "dm_shapeSE" 
    
    # return
    return(c(lm_b, lm_se, lm_sh, lm_shSE))  
}