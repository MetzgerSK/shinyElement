choiceDescs <- 
    c("linear regression, DV = <em>t</em>",
      "linear regression, DV = ln(<em>t</em>)",
      "weib" = "Weibull, DV = <em>t</em>",
      "LN" = "log-normal, DV = <em>t</em>",
      "cens" = "censored linear regression, DV = ln(<em>t</em>)"
    )

## > POSITIVE T -----
output$selMod_posT <- output$selMod_posT2 <- renderUI({
    str <- {
        if(input$model_posT == 1){             # OLS w/DV = t
            choiceDescs[1]
            #"OLS, DV = <em>t</em>"
            
        } else if(input$model_posT == 2){      # OLS w/DV = ln(t)
            choiceDescs[2]
            #"OLS, DV = ln(<em>t</em>)"
        
        } else if(input$model_posT == 3){      # log-normal w/DV = t
            choiceDescs["LN"]
            #"Weibull, DV = <em>t</em>"
        }
    }
    
    paste0("<strong>Displayed Results:</strong> ", str) %>% HTML
})


## > NON-NORMAL ERRORS -----
output$selMod_nnorm <- output$selMod_nnorm2 <- renderUI({
    str <- {
        if(input$model_nnorm == 1){             # OLS w/DV = t
            choiceDescs[1]
            #"OLS, DV = <em>t</em>"
            
        } else if(input$model_nnorm == 2){      # OLS w/DV = ln(t)
            choiceDescs[2]
            #"OLS, DV = ln(<em>t</em>)"
        
        } else if(input$model_nnorm == 3){      # Weibull w/DV = t
            choiceDescs["weib"]
            #"Weibull, DV = <em>t</em>"
        }
    }
    
    paste0("<strong>Displayed Results:</strong> ", str) %>% HTML
})


## > RIGHT CENSORING ----
output$selMod_cens <- output$selMod_cens2 <- renderUI({
    str <- {
        if(input$model_cens == 1){             # OLS w/DV = t
            choiceDescs[1]
            
        } else if(input$model_cens == 2){      # OLS w/DV = ln(t)
            choiceDescs[2]
        
        } else if(input$model_cens == 3){      # Weibull w/DV = t
            choiceDescs["weib"]
            #"Weibull, DV = <em>t</em>")
    
        } else if(input$model_cens == 4){      # CensReg w/DV = ln(t)
            choiceDescs["cens"]
            #"censored linear regression, DV = <em>t</em>"
    
        }
    }
    
    paste0("<strong>Displayed Results:</strong> ", str) %>% HTML
})