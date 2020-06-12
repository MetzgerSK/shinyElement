## Note: all the "displayed estimate" equations could be in ui.R just fine if
## we used some conditionalPanel()s, since there's nothing dynamic about them,
## but in the interest of trying to keep all the equations in the same place
## (and also making ui.R as readable as possible), they're going to live here.


# >> GENERIC LINEAR COMBO ----
## (since we refer to it a LOT)
linCombo <- "\\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\beta}_2 z + \\hat{p}^{-1} u"


# > NON-NORMAL ERRORS ----
# Truth ====
output$eq_true_nnorm <- renderUI({
    withMathJax(
        paste(
            '\\(t = \\exp \\left(', input$aHat_nnorm, 
                                    ifelse(input$b1Hat_nnorm>=0, "+", ""), input$b1Hat_nnorm, 'x',
                                    ifelse(input$b2Hat_nnorm>=0, "+", ""), input$b2Hat_nnorm, 'z', 
                                    '+', input$noise_nnorm, '\ ^{-1} u\\right) \\text{,  in which } u \\sim TIEV_{min} \\text{.}\\)'
        )
    )
})

# Displayed Estms ====
output$eq_estm_nnorm <- renderUI({
    olsAssume <- "\\text{ using OLS, which assumes } u \\sim \\mathcal{N} \\text{.}"
    
    if(input$model_nnorm == 1){             # OLS w/DV = t
        str <- paste0('t= ', linCombo, olsAssume)
        
    } else if(input$model_nnorm == 2){      # OLS w/DV = ln(t)
        str <- paste0('\\ln \\left( t \\right) = ', linCombo, olsAssume)
    
    } else if(input$model_nnorm == 3){      # Weibull w/DV = t
        str <- paste0('t= \\exp \\left( ', linCombo, '\\right) 
                            \\text{ using a Weibull duration model, which assumes } u \\sim TIEV_{min} \\text{.}')
    }
    
    withMathJax(
        paste0('\\(', str, '\\)')
    )
        
})


## > RIGHT CENSORING ----
# Truth ====
output$eq_true_cens <- renderUI({
  withMathJax(
    paste(
        '\\(t = \\exp \\left(', input$aHat_cens, 
                                ifelse(input$b1Hat_cens>=0, "+", ""), input$b1Hat_cens, 'x', 
                                ifelse(input$b2Hat_cens>=0, "+", ""), input$b2Hat_cens, 'z', 
                            '+', input$noise_cens, '\ ^{-1} u\\right) \\text{,  in which } u \\sim TIEV_{min} \\\\ \\qquad \\text{ and }', input$perc_cens, '\\text{% of observations are right censored.}\\)')
  )
})

# Displayed Estms ====
output$eq_estm_cens <- renderUI({
    
    # Reused MJ strings
    olsAssume <- paste0("\\text{ using OLS, } \\\\ \\qquad \\text{which assumes } u \\sim \\mathcal{N} ")
    cens0 <- "\\text{ and 0% of observations are right censored.}"
    cens <- paste0('\\text{ and }', input$perc_cens, '\\text{% of observations are right censored.}')
    
    if(input$model_cens == 1){             # OLS w/DV = t
        str <- paste0('t= ', linCombo, olsAssume, cens0)
        
    } else if(input$model_cens == 2){      # OLS w/DV = ln(t)
        str <- paste0('\\ln \\left( t \\right) = ', linCombo, olsAssume, cens0)
    
    } else if(input$model_cens == 3){      # Weibull w/DV = t
        str <- paste0('t= \\exp \\left( ', linCombo, '\\right)', 
                      '\\text{ using a Weibull duration model, }   \\\\ \\qquad \\text{which assumes } u \\sim TIEV_{min}', cens)
    
    } else if(input$model_cens == 4){      # cens reg w/DV = ln(t)
        str <- paste0('\\ln \\left( t \\right) = ', linCombo, 
                      '\\text{ using censored linear regression, } \\\\ \\qquad  \\text{which assumes } u \\sim \\mathcal{N} ', cens)
    }
    
    withMathJax(
        paste0('\\(', str, '\\)')
    ) 
    
})
