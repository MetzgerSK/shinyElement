# > GRAPH HELPER FUNCTS ----
## Graph font 
font <- "Roboto"
        
## Shift both
shift_axes <- function(p, x=0, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(x=x, y=y)
  
  ay <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  ax <- g[["grobs"]][g$layout$name == "axis-l"][[1]]
  
  p + annotation_custom(grid::grobTree(ay, vp = grid::viewport(y=1, height= sum(ay$height))), 
                        ymax=y, ymin=y) + 
      annotation_custom(grid::grobTree(ax, vp = grid::viewport(x=1, width = sum(ax$height))), 
                        xmax=x, xmin=x)+
    geom_hline(aes(yintercept=y), data = dummy) +
    geom_vline(aes(xintercept=x), data = dummy) +
    theme(axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.y  = element_blank(), 
          axis.ticks.y = element_blank())

}

## Cushion to top and bottom
cushion <- function(var, pct){
    base <- range(var)  
    mult <- max(abs(base))*pct
    
    return(base + c(-mult, mult))
}


# > SERVER FUNCTION ----
server <- function(input, output, session){
    # Load the 
    source("globalPt_mlOptimFunc.R", local=TRUE)
    
    # So that the values persist
    bestSoFar <- reactiveValues(llh = NULL, # best guess for LLH value so far
                                alG = NULL, # intercept corresponding to best guess so far
                                b1G = NULL, # slope corresponding to best guess so far
                                sigG= NULL) # sigma corresponding to best guess so far
    
    # > !!!! STARTS HERE (triggered by button press) !!!! < ----
    all <- eventReactive(input$dataGenButton, {
        # disable the data gen button (force user to reload the entire page for a fresh dataset)
        disable("dataGenButton")
        disable("sigmaMatch")
        disable("nObs")
        removeTooltip(session, "nObs")
        disable("seed")
        shinyjs::show("downloadData")
        shinyjs::hide(selector="h4.simFyiHdr")
        
        # If the checkbox is selected, set best all-time for alpha to true.
        if(input$sigmaMatch==TRUE)      bestSoFar$sigG <- 1.5
        
        # Gen data
        dat <- dataGen()
        
        # Simple hardcoded function for R 3.6.3
        LM_mll_R3.6.3 <- function(b0, b1, sigma) {
          -sum(dnorm(dat$y, (b0 + b1*dat$x1), sigma, log=TRUE))
        }
        
        # Estimate
        mod <- mle(LM_mll_R3.6.3, start=list(b0=0, b1=0, sigma=1), lower=c(-Inf, -Inf, 0.0001))
            ## For >=R 4.0: mle(LM_mll(y ~ x1, data=dat), lower=c(-Inf, -Inf, 0.0001)) 
        mod.glm <- glm(y ~ x1, family=gaussian, data=dat)  # to kludge stargazer later, b/c not in the mood.
        
        # Pull truth
        coeffs <- mod@coef[-3]              # betas
        truth <- as.numeric(logLik(mod))    # true LLH
        
        # Return
        list(data.frame(dat), coeffs, mod, truth, mod.glm)
    })
    
    # > DATA-RELATED ----
    ## Generate data ====
    dataGen <- function() {
        set.seed(input$seed)
        
        aHatTrue <- sample(-20:20, 1)/4       # intercept
        b1HatTrue <- sample(-20:20, 1)/4      # for x
        noise <- ifelse(input$sigmaMatch==FALSE,
                        sample(2:36, 1)/4,	  # how spread out the u_i's are [0.5(0.25)9 as poss range]
                        1.5)
        
        x1 <- rnorm(input$nObs)
        
        e <- rnorm(input$nObs)
        y <- aHatTrue + b1HatTrue*x1 + noise*e

        data.frame(y, x1)
    }
    
    ## LLH for current guess ====
    obsLLH <- reactive({
        xb <- (input$aHat + input$b1Hat*all()[[1]]$x1)
        obs <- dnorm(all()[[1]]$y, mean=xb, sd=input$sigmaHat, log=TRUE)

        c(sum(obs), data.frame(obs))
    })  
    
    ## All-time Max LLH ====
    ## KEEP TRACK OF ALL-TIME MAX LLH
    allTime <- reactive({
        # If it's not the first guess the user's made
        if(!is.null(bestSoFar$llh) & input$solnButton==0){
            if(max(bestSoFar$llh, obsLLH()[[1]])==obsLLH()[[1]]){
                bestSoFar$llh  <- obsLLH()[[1]]
                bestSoFar$alG  <- input$aHat
                bestSoFar$b1G  <- input$b1Hat
                bestSoFar$sigG <- input$sigmaHat
            }
        # If it's the first guess, then by definition, it'll be the best one     
        } else if(is.null(bestSoFar$llh) & input$solnButton==0){
            bestSoFar$llh  <- obsLLH()[[1]]
            bestSoFar$alG  <- input$aHat
            bestSoFar$b1G  <- input$b1Hat
            bestSoFar$sigG <- input$sigmaHat
        }
    })
    
    
#***********************************************************************
# > OUTPUT CODE ----
#***********************************************************************       
    # Eqs, CURRENT LINE EQ ====
    output$eq_lm <- renderUI({
        linCombCalc <- paste0(input$aHat, 
                              ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, "x",
                              "\\text{, with } \\sigma_u = ", input$sigmaHat)
        
        withMathJax(
            paste(
                '\\( y=', linCombCalc, ' \\)'
            )
        )
    })
    
    # Eqs, GENERIC LH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLH <- renderUI({
        
        # do in chunks
        linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x_i')
        num <- paste0('\\exp \\left( \\frac{- \\left( y_i - \\left[', linComb, '\\right] \\right)^{2}} 
                                     {2*', input$sigmaHat, '^2 } \\right)')
        den <- paste0(input$sigmaHat, '\\sqrt{2\\pi}')
        frac <- paste0('\\frac{', 1, '}{', den,'} \\left[', num, '\\right]')

        withMathJax(
            paste('\\(
                  L\\left( \\alpha, \\beta_1, \\sigma_u ~|~y_i, x_i\\right) =
                  \\prod  \\limits_{i=1}^{', input$nObs, '} \\left\\{ ', frac, ' \\right\\} \\)')
        )
    })
    
    # Eqs, GENERIC LLH ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullLLH <- renderUI({
        
        linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x_i')
        num <- paste0('\\exp \\left( \\frac{- \\left( y_i - \\left[', linComb, '\\right] \\right)^{2}} 
                                     {2*', input$sigmaHat, '^2 } \\right)')
        den <- paste0(input$sigmaHat, '\\sqrt{2\\pi}')
        frac <- paste0('\\frac{', 1, '}{', den,'} \\left[', num, '\\right]')
    
        withMathJax(
            paste0(
               '\\( 
                \\begin{align}
                    \\ln L \\left( \\alpha, \\beta_1, \\sigma_u~|~y_i, x_i \\right) &= 
                        \\sum \\limits_{i=1}^{', input$nObs, '} 
                        \\left\\{
                            \\ln \\left(', frac, '\\right)
                        \\right\\} \\\\
                    &= 
                        \\sum \\limits_{i=1}^{', input$nObs, '} \\left\\{ - \\ln\\left(', input$sigmaHat, '\\right) - 0.5 \\ln(2\\pi) - 
                            \\frac{\\left(y_i - \\left[', linComb, '\\right] \\right)^{2}}{2*', input$sigmaHat, '^2}  \\right\\} \\\\
                    &\\equiv 
                        -',input$nObs, '\\ln\\left(', input$sigmaHat, '\\right) - 0.5*', input$nObs, '\\ln(2\\pi) 
                            - \\sum \\limits_{i=1}^{', input$nObs, '} \\left\\{  
                            \\frac{\\left(y_i - \\left[', linComb, '\\right] \\right)^{2}}{2*', input$sigmaHat, '^2}  \\right\\} 
                \\end{align}
                \\)'
            )
        )
    })

    # Eqs, SPECIFIC LLH (SIMPL, EACH OBS) ====
    output$eq_fullLLH_all <- renderUI({
        opener <- paste0('\\ln L\\left( \\alpha=', input$aHat,
                                                        ',~\\beta_1 = ', input$b1Hat, ',~\\sigma_u = ', input$sigmaHat, '
                                                        ~|~y_i, x_i \\right)')      
        equation <- paste0('\\( ', opener, ' = \\\\ ')
        
        # Loop through every single observation      
        for(i in 1:input$nObs){
            # do linear combo, numerator, denominator (in parts, to make syntax errors easier to spot)
            linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, '*', round(all()[[1]][i,2], 3))
            piece <- paste0(' -\\ln \\left(', input$sigmaHat, '\\right) - 0.5 \\ln \\left( 2 \\pi \\right) -')

            piece2 <- paste0('\\left( \\frac{\\left(', round(all()[[1]][i,1], 3), ' - \\left[', linComb, '\\right] \\right)^{2}}
                                         {2*', input$sigmaHat, '^2 } \\right)')

            term <- paste0(piece, piece2)

            # insert sign
            equation <- paste0(equation, ifelse(i!=1, "+", "\\phantom{+}"), ' \\left.
                               \\left( ', term, ' \\right) \\right. ')
        }

        # Insert alignment break
        equation <- paste0(equation, ' \\\\~ \\\\~')

        # Insert LLH value.
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[4]],5))

        equation <- paste0(equation, opener, ' = ', llh)

        # Put MathJax closer tag
        equation <- paste0(equation, '  \\)')
        
        # Print it
        withMathJax(
            equation
        )
    })
    
    # Eqs, SPECIFIC LLH (UNSIMPL, EACH OBS) ====
    output$eq_fullLLH_all_unsimp <- renderUI({

        opener <- paste0('\\ln L\\left( \\alpha=', input$aHat, 
                                                        ',~\\beta_1 = ', input$b1Hat, ',~\\sigma_u = ', input$sigmaHat,'~|~y_i, x_i \\right)')
        equation <- paste0('\\(
                           ', opener, '  = \\\\ ')

        # Loop through every single observation
        for(i in 1:input$nObs){
            # do linear combo, numerator, denominator (in parts, to make syntax errors easier to spot)
            linComb <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, '*', round(all()[[1]][i,2], 3))
            num <- paste0('\\exp \\left( \\frac{- \\left(', round(all()[[1]][i,1],3), 
                                                ' - \\left[', linComb, '\\right] \\right)^{2}} 
                                               {2*', input$sigmaHat, '^2 } \\right)')
            den <- paste0(input$sigmaHat, '\\sqrt{2\\pi}')
            frac <- paste0('\\frac{', 1, '}{', den,'} \\left[', num, '\\right]')
        
            
            # get proper sign depending on whether the DV for this obsv is a 0 or 1 with ifelses
            equation <- paste0(equation, ifelse(i!=1, "+", "\\phantom{+}"), 
                               ' \\left. \\ln \\left( ', frac, ' \\right) \\right. ') 
        }
        
        # Insert alignment break
        equation <- paste0(equation, ' \\\\~ \\\\~')
        
        # Insert LLH value.
        llh <- ifelse(input$solnButton==0, round(obsLLH()[[1]], 5), round(all()[[4]],5))
        equation <- paste0(equation, opener, ' = ', llh)

        # Put MathJax closer tag
        equation <- paste0(equation, ' \\)')
        
        # Print it
        withMathJax(
            equation
        )
    })
    
    # MAIN GRAPH ====
    output$gph<- renderPlot({   
        
        # min/max value
        mm <- 15
        
        # graph
        gg<- ggplot(data=all()[[1]]) +
                geom_point(aes(x=x1, y=y), color="blue", alpha=0.9) +
                geom_abline(slope = input$b1Hat, intercept = input$aHat, col = "red") +
            labs(title="x vs. y",
                 x="x", 
                 y="y") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
                 )
        
        gg %>% shift_axes(.,0,0) 
        
    })
    
    # SIGHAT GRAPH ====
    output$gph.sigmaHat <- renderPlot({
        # Get residuals for all points, based on current proposed line
        xb <- (input$aHat + input$b1Hat*all()[[1]]$x1)
        obs <- (all()[[1]]$y - xb)
        
        # Plot these points, with normal distro overlaid
        ggplot(mapping=aes(x=obs)) + 
            geom_histogram(aes(y=..density..), fill = "gray", color = "black") +
            geom_density(alpha=.2, size = 1.05, color="#104E8B") + 
            geom_rug(alpha=0.5) +  
            stat_function(fun = dnorm, args = list(mean = 0, sd = input$sigmaHat),
                          color = "#FF4040", size = 1, linetype="dashed") + 
            xlim(-3.5*sd(obs), 3.5*sd(obs)) +
            labs(title = "Distance between yHat and x",
                 x     = "Distance (uHat)") + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold", family = paste0(font))
                 )
            })
    
    # RAW MODEL ====
    ## Print the actual results    
    output$modObj <- renderPrint({ 
        summary(all()[[3]])
    })
    
    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
        # number of places to round to
        places <- 2
        
        # Will need to do all this manually, so get model in shorter-to-ref obj
        mod <- all()[[3]]
        
        # And then go super-kludge-y by invoking glm obj, so stargazer doesn't scream
        mod.fake <- all()[[5]]
        mod.fake$coefficients <- c(mod@coef[2], mod@coef[1])
        
        # Dispersion
        disp <- mod@coef[3]^2 # to match what you were pulling from glm
            
        # get the raw HTML
        tab <- stargazer(mod.fake, # to trick stargazer into working
                    type="html", 
                    covariate.labels = c("<em>x</em>", "Constant"),
                    coef = list(c(mod@coef[2], mod@coef[1])),
                    se = list(c(sqrt(vcov(mod)[2,2]), sqrt(vcov(mod)[1,1]))),  
                    title="Model Results",
                    dep.var.labels  = "DV: <em>y</em>",
                    dep.var.caption = "",
                    add.lines = list(c("<em>&sigma;</em>",   
                                           round(sqrt(disp), places)),
                                     c("<em>&sigma;<sup>2</sup></em>",   
                                           round(disp, places)),
                                     c("ln<em>L</em>", 
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
        # + line between lnL and n
        tab <- gsub('<tr><td style="text-align:left">Observations</td>',
                    '<tr style="border-top: 1px solid black"><td style="text-align:left"><em>n</em></td>',
                    tab)
        
        # Bump width of table
        tab <- gsub('<table style="text-align:center">',
                    '<table style="text-align:center; width:10em;">',
                    tab)
        
        # return as wrapped HTML
        HTML(tab)
    })
    
    # CURRENT GUESS' LLH VALUE ====
    output$llh <-  renderUI({
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
            paste0('\\( \\alpha   =', bestSoFar$alG, 
                     ',~\\beta_1  =', bestSoFar$b1G, 
                     ',~\\sigma_u =', bestSoFar$sigG, 
                   ' \\)')
        )
    })
    
    # PRINT TRUE SLOPE + INTERCEPT + RMSE ====
    ## (BASED ON ESTIM MODEL)
    output$trueEsts <- renderUI({
        # grab sigmaHat^2
        mse <- all()[[3]]@coef[3]^2
        
        # print
        withMathJax(
            paste0('\\( 
                        \\alpha ='   , round(all()[[2]][1], 3), 
                     ',~\\beta_1 ='  , round(all()[[2]][2], 3), 
                     ',~\\sigma_u = ', round(sqrt(mse), 3), 
                   '\\)')
        )
    })
       
    # PRINT ANSWER/TRUE LLH ====
    output$trueLLH <- renderUI({ # Otherwise, this evaluates from the get-go, before the button's even pushed.
        withMathJax(
            paste0('\\(', round(all()[[4]], 5), ' \\)')
        )
    })
    
    # DT: ACTUAL DATASET ====
    output$data_table <- DT::renderDataTable({
        # To force the render, in case the user clicks the "Data" tab before gening dataset
        if(input$dataGenButton>0){
            dat <- all()[[1]]
            llh <- round(obsLLH()[[1]], 3)
            dat$logLH_i <- obsLLH()[[2]]
            n  <- input$nObs # avoid subset error
            
            # Custom head/footer
            sketch <- htmltools::withTags(table(
                class = 'display',
                
                # header
                DT::tableHeader(cbind("ID"=NA, dat)),
                
                # footer
                tfoot(
                    tr(
                        th(colspan="4", paste0("TOTAL (across all ", n, " observations): ", round(sum(dat$logLH_i),5) )) 
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
                              dom = ifelse(n<=25, 't', 'ltp'), # hide length + page selector if less than 25, show othw (though see also autoHideNavigation)
                              columnDefs = list(
                                  list(className = 'dt-center', targets = c(0))
                              )
                          )
            ) %>% 
              DT::formatRound(c(1:ncol(dat)), 5) 
        }
    })
    
    # OUTPUT: making dataset downloadable, if people want to fiddle
    # Downloadable csv of selected dataset ----
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
    # Obs slider range change + updateSlider ====
    source("serverPt_nObsDblClk.R", local=TRUE)
    
    # Close callout divs ====
    runjs('$(".bs-close").click(function() {
        $(this).parent().fadeOut("slow");
    });')
    
    # Reset button ====
    observeEvent(input$resetButton, {
        reset("aHat")
        reset("b1Hat")
        reset("sigmaHat")
    })
    
    # obEv: sigma slider ====
    ## MISC: toggle whether sigma slider's enabled or not, based on checkbox
    observeEvent(input$sigmaMatch, {
        toggleState("sigmaHat", condition=input$sigmaMatch==FALSE)
        
        if(input$sigmaMatch==TRUE){
            updateSliderInput(session, "sigmaHat", value=1.5)
            addTooltip(session, "sigmaHat", "Disabled to make DGP <br/> same as <code>leastSq</code>. <br/>-- True <em>&sigma;</em> = 1.5 --")    
        }
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
    
    # Language toggle ====
    ## MISC: toggle between informal and formal language
    ## (officially shifted from toggles to have more control over the headers, once "Show Answer" is clicked)
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
        # Min/max for slider
        mm <- 8
        
        # Store the RMSE, to save time
        rmse <- all()[[3]]@coef[3]
        
        # Update min/maxes, if needed
        aMin <- ifelse(all()[[2]][1]< -mm, floor(all()[[2]][1])  , -mm)
        aMax <- ifelse(all()[[2]][1]>  mm, ceiling(all()[[2]][1]),  mm)
            
        bMin <- ifelse(all()[[2]][2]< -mm, floor(all()[[2]][2])  , -mm)
        bMax <- ifelse(all()[[2]][2]>  mm, ceiling(all()[[2]][2]),  mm)
        
        sigMin <- ifelse(all()[[2]][2]< .5 , 0            , .5)
        sigMax <- ifelse(all()[[2]][2]>  10, ceiling(rmse),  10)
        
        updateSliderInput(session, "aHat", value=list(all()[[2]][1]),
                          min=list(aMin), max=list(aMax) )
        updateSliderInput(session, "b1Hat", value=list(all()[[2]][2]),
                          min=list(bMin), max=list(bMax) )
        updateSliderInput(session, "sigmaHat", value=list(rmse),
                          min=list(sigMin), max=list(sigMax) )
        
        # Disable the sliders
        disable("aHat")
        disable("b1Hat")
        disable("sigmaHat")
        disable("resetButton")

        showElement("wrapper_rslts")
    })
    
    # Restore best guess ====
    observeEvent(input$restoreButton, {
        updateSliderInput(session, "aHat"    , value=list(bestSoFar$alG))
        updateSliderInput(session, "b1Hat"   , value=list(bestSoFar$b1G))
        updateSliderInput(session, "sigmaHat", value=list(bestSoFar$sigG))
    })
    
    # Housekeeping
    session$onSessionEnded(stopApp)  
}    