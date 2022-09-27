# Graph font
font <- "Roboto"


# > SERVER FUNCTION ----
server <- function(input, output, session){

    # So that the values persist
    bestSoFar <- reactiveValues(llh = NULL, # best guess for LLH value so far
                                alG = NULL, # intercept corresponding to best guess so far
                                b1G = NULL) # x slope corresponding to best guess so far
    
    
    # Function for lnL
    genLLH <- function(aHat, bHat){
        # For a given value of aHat, determine lnL value
        xb <- aHat + bHat*all()[[1]]$x1
        obs <- ifelse(all()[[1]]$y==1, log(plogis(xb)), log(plogis(xb, lower.tail=FALSE)))
    
        # Return that sum + full data.fr
        c(sum(obs), data.frame(obs))
    }
    
    # > !!!! STARTS HERE (triggered by button press) !!!! < ----
    all <- eventReactive(input$dataGenButton, {
        # disable the data gen button (force user to reload the entire page for a fresh dataset)
        disable("dataGenButton")
        enable("downloadData")
        disable("nObs")
        disable("seed")
        shinyjs::show("downloadData")
        shinyjs::hide(selector="h4.simFyiHdr")
        
        # Begin
        dat <- dataGen()
        mod <- glm(y ~ x1, family=binomial, data=dat)
        truth <- as.numeric(logLik(mod))    # true LLH
        
        list(data.frame(dat), mod, truth)
    })
    
    # > DATA-RELATED ----
    ## Generate data ====
    dataGen <- function() {
        set.seed(input$seed)
        
        aHatTrue <- sample(-20:20, 1)/4       # intercept
        b1HatTrue <- sample(-20:20, 1)/4	  # for x

        x1 <- rnorm(input$nObs)

        y <- as.numeric(plogis(aHatTrue + b1HatTrue*x1) > runif(input$nObs))

        data.frame(y, x1)
    }
    
    ## LLH for current guess ====
    obsLLH <- reactive({
        genLLH(input$aHat, input$b1Hat)  # now streamlined
    })  
    
    ## All-time Max LLH ====
    ## KEEP TRACK OF ALL-TIME MAX LLH
    allTime <- reactive({
        if(!is.null(bestSoFar$llh) & input$solnButton==0){
            if(max(bestSoFar$llh, obsLLH()[[1]])==obsLLH()[[1]]){
                bestSoFar$llh <- obsLLH()[[1]]
                bestSoFar$alG <- input$aHat
                bestSoFar$b1G <- input$b1Hat
            }
        } else if(is.null(bestSoFar$llh) & input$solnButton==0){
            bestSoFar$llh <- obsLLH()[[1]]
            bestSoFar$alG <- input$aHat
            bestSoFar$b1G <- input$b1Hat
        }
    })

    
#***********************************************************************
# > OUTPUT CODE ----
#***********************************************************************      
        
    # Eqs, CURRENT LOGIT EQ ====
    output$eq_logit <- renderUI({
        linCombCalc <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, "x")
        
        withMathJax(
            paste(
                '\\( \\Pr \\left(y=1~|~x \\right) = 
                    \\frac{    \\exp \\left(', linCombCalc, '\ \\right) } 
                          {1 + \\exp \\left(', linCombCalc, '\ \\right) } \\)'
            )
        )
    })
    
    # Eqs, GENERIC LH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLH <- renderUI({
        
        # do in chunks
        linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x_i')
        num <- paste0('\\exp \\left(', linComb, '\\right)')
        den <- paste0('1 + \\exp \\left(', linComb, '\\right)')
        frac <- paste0(' \\frac{', num, '}{', den,'}')

        withMathJax(
            paste(
                '\\( L\\left( \\alpha, \\beta_1~|~y, x\\right) =',
                '\\prod\\limits_{i=1}^{', input$nObs, '}',
                '\\left\\{ 
                    \\left(', frac, '\\right)^{y_i}  *
                    \\left( \\frac{1}{', den, '} \\right)^\\left( 1 - y_i \\right)',
                '\\right\\}\\)'
            )
        )
    })
    
    # Eqs, GENERIC LLH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLLH <- renderUI({
        
        linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x_i')
        num <- paste0('\\exp \\left(', linComb, '\\right)')
        den <- paste0('1 + \\exp \\left(', linComb, '\\right)')
        frac <- paste0(' \\frac{', num, '}{', den,'}')
    
        withMathJax(
            paste(
                '\\( \\ln L\\left( \\alpha, \\beta_1~|~y, x\\right) = \\sum\\limits_{i=1}^{', input$nObs, '} \\left\\{
                \\left[ y_i* \\ln \\left(', frac, '\\right) \\right] +
                \\left[ \\left( 1 - y_i \\right) * \\ln \\left( \\frac{1}{', den, '} \\right) \\right] \\right\\}\\)'
            )
        )
    })

    # Eqs, SPECIFIC LLH (SIMPL, EACH OBS) ====
    output$eq_fullLLH_all <- renderUI({
        
        equation <- paste0('\\(  \\ln L\\left( \\alpha=', input$aHat, 
                                                        ',~\\beta_1 = ', input$b1Hat, '~|~y, x \\right) = ')
        
        # Loop through every single observation      
        for(i in 1:input$nObs){
            
            # do linear combo, numerator, denominator (in parts, to make syntax errors easier to spot)
            linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, '*', round(all()[[1]][i,2], 3))
            num <- paste0('\\exp \\left(', linComb, '\\right)')
            den <- paste0('1 + \\exp \\left(', linComb, '\\right)')
            frac <- paste0(' \\frac{', num, '}{', den,'}')
            
            # get proper sign depending on whether the DV for this obsv is a 0 or 1 with ifelses
            equation <- paste0(equation, ifelse(i!=1, "+", ""), ' \\left. \\ln \\left( ', 
                               '\\frac{', ifelse(all()[[1]][i,1]==0, "1", num), '}{', den, '}', 
                               '\\right) \\right.') 
        }
            
        # Insert alignment break
        equation <- paste0(equation, ' \\\\~')
        
        # Insert LLH value.
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[3]],5))

        equation <- paste0(equation, ' \\ln L\\left( \\alpha=', input$aHat,
                                                        ',~\\beta_1 = ', input$b1Hat, '~|~y, x \\right) =', llh)

        # Put MathJax closer tag
        equation <- paste0(equation, '  \\)')
        
        # Print it
        withMathJax(
            equation
        )
    })
    
    # Eqs, SPECIFIC LLH (UNSIMPL, EACH OBS) ====
    output$eq_fullLLH_all_unsimp <- renderUI({

        equation <- paste0('\\(  \\ln L\\left( \\alpha=', input$aHat, 
                                                        ',~\\beta_1 = ', input$b1Hat, '~|~y, x \\right) = ')

        # Loop through every single observation
        for(i in 1:input$nObs){

            # do linear combo, numerator, denominator (in parts, to make syntax errors easier to spot)
            linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, '*', round(all()[[1]][i,2], 3))
            num <- paste0('\\exp \\left(', linComb, '\\right)')
            den <- paste0('1 + \\exp \\left(', linComb, '\\right)')
            frac <- paste0(' \\frac{', num, '}{', den,'}')

            # get proper sign depending on whether the DV for this obsv is a 0 or 1 with ifelses
            equation <- paste0(equation, ifelse(i!=1, "+", " \\\\ ~~ "), '\\left\\{', all()[[1]][i,1], '* \\left. \\ln \\left( ', frac, " \\right) \\right. +",
                               '\\left(1-', all()[[1]][i,1], '\\right) * \\left. \\ln \\left( \\frac{1}{', den, '} \\right) \\right. \\right\\} ') 
        }
        
        # Insert alignment break
        equation <- paste0(equation, ' \\\\ ')
        
        # Insert LLH value.
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[3]],5))

        equation <- paste0(equation, ' \\ln L\\left( \\alpha=', input$aHat,
                                                        ',~\\beta_1 = ', input$b1Hat, '~|~y, x \\right) =', llh)

        # Put MathJax closer tag
        equation <- paste0(equation, '  \\)')

        # Print it
        withMathJax(
            equation
        )
    })
    
    # MAIN GRAPH ====
    output$gph<- renderPlot({
        req(input$dataGenButton)
        
        # Generate predicted value, given this expression.
        dat <- all()[[1]] 
        datFake <- data.frame(fakeX = seq(-10,10,0.005)) %>%
                         mutate(yHat = plogis(fakeX*input$b1Hat + input$aHat))

        # graph
        gg <- ggplot() +
                geom_point(aes(y=dat$y, x=dat$x1), color="blue", alpha=0.75) +
                geom_line(aes(y=datFake$yHat, x=datFake$fakeX), col="red") +
            labs(x="<em>x</em>", 
                 y="Pr(<em>y</em> = 1)"
                ) +
            coord_cartesian(ylim=c(0,1), xlim=c(-10,10)) + 
            theme(plot.title = element_text(hjust = 0.5, 
                                            face = "bold", 
                                            family = "font"),
                  axis.title.x = element_markdown(family=font),
                  axis.title.y = element_markdown(family=font)
                 )
        
        # If there's more than 125 observations, add rugs
        if(nrow(dat)>=125){
            gg <- gg + 
                    geom_rug(aes(x=filter(dat, y==1)$x1), sides="t", alpha=0.2) +
                    geom_rug(aes(x=filter(dat, y==0)$x1), sides="b", alpha=0.2)
        }
        
        # Return plot
        gg 
    })
    
    # LLH SUBGPH HEADERS ====
    output$header_lnL_aHat <- renderUI({
        withMathJax(
            paste0("\\( \\ln L \\) values when \\(\\hat{\\beta_1} = ", round(input$b1Hat, 2), "\\)")
        )
    })
    output$header_lnL_b1Hat <- renderUI({
        withMathJax(
            paste0("\\( \\ln L \\) values when \\(\\hat{\\alpha} = ", round(input$aHat, 2), "\\)")
        )
    })
        
    # Start building the graph
    gph_lnL <- function(line_y, line_x, point_y, point_x, xAxisLab, yAxis){
        
        yAxisLab <- ifelse(grepl("aHat", xAxisLab), "b1Hat", "aHat")
                           
        ggplot() +
            geom_line(aes(y=line_y, x=line_x), col="black") +
            geom_point(aes(y=point_y, x=point_x), color="red", size=2.5) +
        labs(x=xAxisLab, 
             y=paste0("ln<em>L</em> | ", yAxisLab, " = ", yAxis)
            ) +
        coord_cartesian(xlim=c(-6,6)) +    
        theme(plot.title = element_text(hjust = 0.5, 
                                        face = "bold", 
                                        family = font),
              axis.title.x = element_markdown(family=font),
              axis.title.y = element_markdown(family=font),
              axis.text.y  = element_blank(),
              axis.ticks.y  = element_blank()
             )
    }
    
    # GRAPH: lnL vs. aHat value  ====
    output$gph_lnL_aHat<- renderPlot({
        req(input$dataGenButton)
        
        # Generate 'dataset' -> values of lnL, given this value of b1Hat
        datFake_aHat <- map_dfr(seq(-6,6,0.05), 
                                ~data.frame(aHat = .x, lnL = genLLH(.x, input$b1Hat)[[1]]))
        
        # Return graph
        gph_lnL(line_y = datFake_aHat$lnL,     
                line_x = datFake_aHat$aHat,
                point_y = genLLH(input$aHat, input$b1Hat)[[1]], 
                point_x = input$aHat,
                xAxisLab = "aHat Values",
                yAxis = round(input$b1Hat, 2))
    })
    
    # GRAPH: lnL vs. b1Hat value  ====
    output$gph_lnL_b1Hat<- renderPlot({
        req(input$dataGenButton)
        
        # Generate 'dataset' -> values of lnL, given this value of aHat
        datFake_b1Hat <- map_dfr(seq(-6,6,0.05), 
                                ~data.frame(b1Hat = .x, lnL = genLLH(input$aHat, .x)[[1]]))
        
        # Return graph
        gph_lnL(line_y = datFake_b1Hat$lnL,     
                line_x = datFake_b1Hat$b1Hat,
                point_y = genLLH(input$aHat, input$b1Hat)[[1]], 
                point_x = input$b1Hat,
                xAxisLab = "b1Hat Values",
                yAxis = round(input$aHat, 2))
    })
    
    # RAW MODEL ====
    # Print the actual logit    
    output$modObj <- renderPrint({ 
        summary(all()[[2]]) 
    })
    
    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
        # number of places to round to
        places <- 2
        
        # Load
        mod <- all()[[2]]
        
        # get the raw HTML
        tab <- stargazer(mod,
                    type="html", 
                    covariate.labels = c("<em>x</em>", "Constant"),
                    title="Model Results",
                    dep.var.labels  = "DV: <em>y</em>",
                    dep.var.caption = "",
                    add.lines = list(c("ln<em>L</em>", 
                                       round(logLik(mod), places))
                                ),
                    keep.stat = c("N"),
                    report="vcs",               # just for you, Neal!
                    omit.table.layout="n",
                    column.sep.width = "2pt",
                    digits=places,
                    digits.extra=4
            )
        
        # Nuke final table line
        tab <- gsub('<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr></table>',
                    '<tr><td colspan="2" style="border-bottom: 0px solid black"></td></tr></table>',
                    tab)
        
        # Change wording to n (brute forcing, rather than arguing with stargazer) 
        tab <- gsub('<tr><td style="text-align:left">Observations</td>',
                    '<tr><td style="text-align:left"><em>n</em></td>',
                    tab)
        
        # Bump width of table
        tab <- gsub('<table style="text-align:center">',
                    '<table style="text-align:center; width:10em;">',
                    tab)
        
        # return as wrapped HTML
        HTML(tab)
    })
    
    # CURRENT GUESS' LLH VALUE ====
    output$llh <- renderUI({
        withMathJax(
            paste0('\\(', round(obsLLH()[[1]], 5), '\\)')
        )
    })
    
    # ALL-TIME MAX LLH ====
    output$bestGuess <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\(', round(bestSoFar$llh, 5), '\\)')
        )
    })
    
    # PRINT ESTIMATES FOR ALL-TIME MAX LLH ====
    output$bestGuess_ests <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\( \\alpha =', bestSoFar$alG, ',~\\beta_1 =', bestSoFar$b1G, ' \\)')
        )
    })
    
    # OUTPUT: PRINT TRUE SLOPE + INTERCEPT (BASED ON ESTIMATED MODEL)
    output$trueEsts <- renderUI({
        req(input$dataGenButton)
        coeffs <- coef(all()[[2]])
        
        withMathJax(
            paste0('\\( \\alpha =', round(coeffs[1], 3), ',~\\beta_1 =', round(coeffs[2], 3), ' \\)')
        )
    })
    
    # OUTPUT: PRINT ANSWER/TRUE LLH
    output$trueLLH <- renderUI({ 
        withMathJax(
            paste0('\\(', round(all()[[3]], 5), ' \\)')
        )
    })
    
    # DT: ACTUAL DATASET ====
    output$data_table <- DT::renderDataTable({
        dat <- all()[[1]]
        llh <- round(obsLLH()[[1]], 3)
        dat$logLH_i <- obsLLH()[[2]]
        n  <- input$nObs # avoid subset error
        
        # Mod a table
        sketch = htmltools::withTags(table(
            class = 'display',
            
            # header
            DT::tableHeader(cbind("ID"=NA, dat)),
            
            # footer
            tfoot(
                tr(
                    th(colspan="4", paste0("TOTAL (across all ", n, " observations): ", llh))
                )
            )
            
        ))
        
        # Print it
        DT::datatable(dat,
                      escape=TRUE,
                      rownames=TRUE,
                      filter = "none",
                      container = sketch, 
                      options = list(
                          pageLength = ifelse(n==25, 25, 15),
                          dom = ifelse(n<=25, 't', 'l t p'), # hide length + page selector if less than 25, show othw (though see also autoHideNavigation)
                          columnDefs = list(
                              list(className = 'dt-center', targets = c(0))
                          )
                      )
            ) %>% 
              DT::formatRound(c(2:ncol(dat)), 5) 
    })

    # Downloadable csv of selected dataset ====
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("artificial_data.csv", sep = "")
        },
        content = function(file) {
            write.csv(all()[[1]], file, row.names = FALSE)
        }
    )
    
#***********************************************************************
# > MISC HELPER CODE ----
#***********************************************************************     
    # Reset button ====
    observeEvent(input$resetButton, {
        reset("aHat")
        reset("b1Hat")
    })
    
    # Informal/formal language toggle ====
    observeEvent(c(input$lang, input$solnButton), {
        
        # Informal language
        if(input$lang==TRUE){
            shinyjs::show(id = "inf_instrText")
            shinyjs::show(id = "inf_ptTotal")
            shinyjs::show(id = "inf_bestGuess")

            if(input$solnButton!=0){
                shinyjs::show(id = "inf_actual")
                shinyjs::show(id = "inf_bestGuess_ans")
                
                hide(selector = "div.ptTotal_llh")
                hide(id = "inf_ptTotal")
                hide(id = "inf_bestGuess")
                hide(id = "formal_actual")
                hide(id = "restoreButton")
            }
            
            hide(id = "fullLLHButton")
            hide(id = "formal_instrText")
            hide(id = "formal_ptTotal")
            hide(id = "formal_bestGuess")
            hide(id = "formal_bestGuess_ans")
            hide(id = "formal_actual")
            hide(id = "wrapper_lnLGphs")
            
        # Formal language
        } else{
            hide(id = "inf_instrText")
            hide(id = "inf_ptTotal")
            hide(id = "inf_bestGuess")
            hide(id = "inf_bestGuess_ans")
            
            shinyjs::show(id = "fullLLHButton")
            shinyjs::show(id = "formal_instrText")
            shinyjs::show(id = "formal_ptTotal")
            shinyjs::show(id = "formal_bestGuess")
            shinyjs::show(id = "wrapper_lnLGphs")
                        
            if(input$solnButton!=0){
                shinyjs::show(id = "formal_actual")
                shinyjs::show(id = "formal_bestGuess_ans")
                
                hide(selector = "div.ptTotal_llh")
                hide(id = "formal_ptTotal")
                hide(id = "formal_bestGuess")
                hide(id = "inf_actual")
                hide(id = "restoreButton")
            }
        }
    })
    
    # "Show Answer" actions ====
    ## The answer (+ disable sliders)
    observeEvent(input$solnButton, {
        
        coeffs <- coef(all()[[2]])
        
        aMin <- ifelse(coeffs[1]< -6, floor(coeffs[1]), -6)
        aMax <- ifelse(coeffs[1]>  6, ceiling(coeffs[1]),  6)
            
        bMin <- ifelse(coeffs[2]< -6, floor(coeffs[2]), -6)
        bMax <- ifelse(coeffs[2]>  6, ceiling(coeffs[2]),  6)
        
        updateSliderInput(session, "aHat", value=list(coeffs[1]),
                          min=list(aMin), max=list(aMax) )
        updateSliderInput(session, "b1Hat", value=list(coeffs[2]),
                          min=list(bMin), max=list(bMax) )
        
        disable("aHat")
        disable("b1Hat")
        disable("resetButton")

        showElement("wrapper_rslts")
    })
    
    # Results div - red outline kick ====
    ## MISC: put temporary red outline around results, 
    ## if user's navigated from anchor link (then toss
    ## outline once user's moused over table)
    runjs('
        $(\'a[href="#wrapper_rslts"]\').click(function(){ // when the anchor is clicked
            
            $("#wrapper_rslts").css({"boxShadow" : "0 0 20px 5px #d9534f"}); 
            
            // Change everything back once you mouseover the results
            $("#wrapper_rslts").mouseover(function() { 
                $("#wrapper_rslts").css({"boxShadow" : "0 0 20px 5px rgba(255, 0, 0, 0)" })  
            });
        });
    ')
    
    # Restore best guess ====
    observeEvent(input$restoreButton, {
        updateSliderInput(session, "aHat", value=list(bestSoFar$alG))
        updateSliderInput(session, "b1Hat", value=list(bestSoFar$b1G))
    })
    

    ## Min/max div containing lnL graphs
    runjs('
        $("#lnLMinmz").hide();
        $("#lnLGphs").hide();
        
        $("#lnLMinmz").click(function() {
            $("#lnLGphs").slideUp();
            $("#lnLMinmz").hide();
            $("#lnLMaxmz").show();
        });
        
        $("#lnLMaxmz").click(function() {
            $("#lnLGphs").slideDown();
            $("#lnLMaxmz").hide();
            $("#lnLMinmz").show();
        });
    ')
    # Housekeeping
    session$onSessionEnded(stopApp) 
}    