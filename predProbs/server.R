library(Stat2Data)

server <- function(input, output, session){
    results <- eventReactive(input$estmButton, {
                data(Titanic)
                glm(Survived ~ PClass + Age + Sex, family=binomial, data=Titanic)
            })
            
    output$modObj <- renderPrint({
                        summary(results())
                  })
                    
    output$fFormExpr <- renderUI({
        # To get ticket type dummy variables set correctly
        if(input$coeffClass=="1"){
            cl2 <- 0
            cl3 <- 0
        } else if(input$coeffClass=="2"){
            cl2 <- 1
            cl3 <- 0
        } else{
            cl2 <- 0
            cl3 <- 1
        }

        linCombCalc <- paste0(signif(coef(results())[1],4),
                                    ifelse(coef(results())[2]>=0, "+", ""), 
                                        signif(coef(results())[2], 4), "*", cl2,
                                    ifelse(coef(results())[3]>=0, "+", ""), 
                                        signif(coef(results())[3], 4), "*", cl3,
                                    ifelse(coef(results())[4]>=0, "+", ""), 
                                        signif(coef(results())[4], 4), "*", input$coeffAge,
                                    ifelse(coef(results())[5]>=0, "+", ""), 
                                        signif(coef(results())[5], 4), "*", input$coeffGender
                             )
        withMathJax(
            paste(
                '\\(\\Pr(y = 1) = \\frac{\\exp \\left(', linCombCalc,'\\right)}
                        {1 + \\exp \\left(', linCombCalc,'\\right)} \\)'
            )
        )
    })
    
    output$predPr <- renderUI({
        # To get ticket type dummy variables set correctly
        if(input$coeffClass=="1"){
            cl2 <- 0
            cl3 <- 0
        } else if(input$coeffClass=="2"){
            cl2 <- 1
            cl3 <- 0
        } else{
            cl2 <- 0
            cl3 <- 1
        }

        val <- plogis(
                    coef(results())[1] + coef(results())[2]*cl2 + 
                    coef(results())[3]*cl3 + coef(results())[4]*input$coeffAge + 
                    coef(results())[5]*as.numeric(input$coeffGender)
                )

        withMathJax(
            paste0(
                '\\(\\Pr(y = 1) = ', signif(val,3), 
                    ' \\Leftarrow \\text{ probability a ', input$coeffAge, '-year-old ', 
                    ifelse(as.numeric(input$coeffGender)==0, "female", "male"), ' passenger riding in ', 
                    ifelse(input$coeffClass=="1", "first class", ifelse(input$coeffClass=="2", "second class", "third class")), 
                    ' survived the } \\mathit{Titanic} \\text{\'s sinking}\\)'
            )
        )
    })
}