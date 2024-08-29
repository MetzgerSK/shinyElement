library(Stat2Data)
library(shinyjs)
library(stargazer)

# Load so accessible to any of the functions immediately
# (and since static, just load outside of server funct)
data(Titanic)

# > SERVER FUNCTION ----
server <- function(input, output, session){
    
#***********************************************************************
# INTERMEDIATE CALCULATIONS ----
#***********************************************************************  
    # > !!!! SERVER'S ACTIONS START HERE (triggered by button press) !!!! <
    # Run the regression (from this point forward, all the other calculations will auto-update).
    results <- eventReactive(input$estmButton, {
                    glm(Survived ~ PClass + Age + Sex, family=binomial, data=Titanic)
               })
    
#***********************************************************************
# OUTPUT OBJECTS ----
#***********************************************************************  
    # OUTPUT: the model object
    output$modObj <- renderUI({
        req(results())
        
        # get the raw HTML
        tab <- stargazer(results(), 
                    type="html",
                    covariate.labels = c("2nd Class?", "3rd Class?", "Age", "Male?"),

                    dep.var.labels   = "<em>Survived Sinking?</em>",
                    dep.var.caption = "",

                    report = "vcs",               # just for you, Neal!
                    omit.table.layout="n",

                    keep.stat = c("N", "ll")
            )

        # Nuke final table line
        tab <- gsub('<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr></table>',
                    '<tr><td colspan="2" style="border-bottom: 0px solid black"></td></tr></table>',
                    tab)
        
        # Tweak border color
        tab <- gsub("border-bottom: 1px solid black", 
                    "border-bottom: 1px solid rgba(255,255,255,0.25)", 
                    tab)
        
        # return as wrapped HTML
        HTML(tab)

     })
    
    # OUTPUT: the longhand eq w/dynamic components
    output$fFormPrExpr <- renderUI({  #
        req(results())
        
        # To get ticket type dummy variables set correctly
        setTktVars(cl2, cl3)
        
        # Get coeffs
        betas <- coef(results())
        
        # Begin constructing expression w/sign + sig fig helper funct
        linCombCalc <- paste0(signif(betas[1],4), 
                              signSF(betas[2]), "*", cl2,
                              signSF(betas[3]), "*", cl3,
                              signSF(betas[4]), "*", input$coeffAge,
                              signSF(betas[5]), "*", input$coeffGender
                             )
        
        # the general functional form
        ff <- paste0('\\( \\begin{align} \\widehat{\\Pr(y = 1)} &= 
                        \\frac{\\exp \\left(', linCombCalc,'\\right)}
                              {1 + \\exp \\left(', linCombCalc,'\\right)}')

        # The actual pred prob
        pred <- predict(results(), type="response", se.fit=TRUE, 
                     newdata = data.frame(Age=input$coeffAge, 
                                          Sex=ifelse(as.numeric(input$coeffGender)==0, "female", "male"), 
                                          PClass=ifelse(cl2==1, "2nd", ifelse(cl3==1, "3rd", "1st")) 
                               )
                )

        # Build the prose interpretation
        predPr <- paste0('\\\\[1.25ex] 
                    \\widehat{\\Pr(y = 1)} &= ', signif(pred[[1]],3), '\\Leftarrow 
                    \\begin{aligned}
                        &\\vphantom{=} \\text{(predicted) probability that a ', input$coeffAge, '-year-old ',
                                        ifelse(as.numeric(input$coeffGender)==0, "female", "male"), ' passenger} \\\\',
                       '&\\vphantom{=} \\text{riding in ', ifelse(input$coeffClass=="1", "first class", 
                                                              ifelse(input$coeffClass=="2", "second class", "third class")
                                                           ), ' survived the } \\mathit{Titanic} \\text{\'s sinking}
                    \\end{aligned}'
                  )

        # Output the final equation + text + standard error w/MathJax
        withMathJax(
            paste0(
                ff, predPr, ' \\end{align} \\\\
                \\text{ } \\\\
                \\text{ } \\\\
                \\text{SE}(\\widehat{\\Pr(y = 1)}) = ', signif(pred[[2]],3), 
                '\\)'
            )
        )
    })

    
#***********************************************************************
# MISC HELPER CODE ----
#***********************************************************************
    # MISC: get proper coef sign + # of sig figs for MathJax expr
    signSF <- function(beta){
        return(
            paste0(ifelse(beta>=0, "+", ""), signif(beta, 4))
        )
    }
    
    # MISC: the helper function for setting ticket type dummy variables correctly (to show how the code could be more economical)
    setTktVars <- function(obj1=0, obj2=0){
        if(input$coeffClass=="1"){
            assign(deparse(substitute(obj1)), 0, env=parent.frame())
            assign(deparse(substitute(obj2)), 0, env=parent.frame())
            
        } else if(input$coeffClass=="2"){
            assign(deparse(substitute(obj1)), 1, env=parent.frame()) 
            assign(deparse(substitute(obj2)), 0, env=parent.frame())

        } else{
            assign(deparse(substitute(obj1)), 0, env=parent.frame()) 
            assign(deparse(substitute(obj2)), 1, env=parent.frame())
        }
    }
    
    # MISC: gray out the covariate widgets until the "Estimate Model" button's pushed (Javascript)
    observeEvent(input$estmButton, {
        enable("coeffClass")
        enable("coeffAge")
        enable("coeffGender")
        hide("instrText")
    })
    
    # MISC: setting slider using age's calculated max, min, and median values
    updateSliderInput(session, "coeffAge", 
        min   = max(min(Titanic$Age, na.rm=TRUE), 1),  # min()'s wrapped in a max() call b/c there are ages < 1 and we want integers (w/o blindly forcing everything to be an int with ceiling())  
        max   = max(Titanic$Age, na.rm=TRUE), 
        value = median(Titanic$Age, na.rm=TRUE) 
    )

    # kill connection to server once app stops running   
    session$onSessionEnded(stopApp)
}