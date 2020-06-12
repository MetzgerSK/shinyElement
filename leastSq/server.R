# > GRAPH HELPER FUNCTS ----
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
    
    # So that the values persist
    bestSoFar <- reactiveValues(ssr = NULL, # best guess for SSR value so far
                                alG = NULL, # intercept corresponding to best guess so far
                                b1G = NULL) # slope corresponding to best guess so far

    # > !!!! STARTS HERE (triggered by button press) !!!! < ----
    all <- eventReactive(input$dataGenButton, {
        # disable the data gen button (force user to reload the entire page for a fresh dataset)
        disable("dataGenButton")
        disable("nObs")
        removeTooltip(session, "nObs")
        disable("seed")
        shinyjs::show("downloadData")
        shinyjs::hide(selector="h4.simFyiHdr")
                
        # Begin
        dat <- dataGen()
        mod <- lm(y ~ x1, data=dat)
            
        # Pull truth
        truth <- anova(mod) %>%                  # true SSR
                    .["Residuals", "Sum Sq"]     
        
        # Return
        list(data.frame(dat), mod, truth)
    })
    
    # > DATA-RELATED ----
    ## Generate data ====    
    dataGen <- function() {
        set.seed(input$seed)
        
        aHatTrue <- sample(-20:20, 1)/4       # intercept
        b1HatTrue <- sample(-20:20, 1)/4	  # for x
        noise <- 1.5		                  # how spread out the u_i's are 
            
        x1 <- rnorm(input$nObs)
        
        e <- rnorm(input$nObs)
        y <- aHatTrue + b1HatTrue*x1 + noise*e
        
        data.frame(y, x1)
    }
    
    ## SSR for current guess ====
    obsSSR <- reactive({

        xb <- (input$aHat + input$b1Hat*all()[[1]]$x1)
        obs <- (all()[[1]]$y - xb)^2

        c(sum(obs), data.frame(obs))
    })  
    
    ## All-time Min SSR ====
    ## KEEP TRACK OF ALL-TIME MIN SSR
    allTime <- reactive({
        # If it's not the first guess the user's made
        if(!is.null(bestSoFar$ssr) & input$solnButton==0){
            if(min(bestSoFar$ssr, obsSSR()[[1]])==obsSSR()[[1]]){
                bestSoFar$ssr <- obsSSR()[[1]]
                bestSoFar$alG <- input$aHat
                bestSoFar$b1G <- input$b1Hat
            }
        # If it's the first guess, then by definition, it'll be the best one    
        } else if(is.null(bestSoFar$ssr) & input$solnButton==0){
            bestSoFar$ssr <- obsSSR()[[1]]
            bestSoFar$alG <- input$aHat
            bestSoFar$b1G <- input$b1Hat
        }
    })
    
    
#***********************************************************************
# > OUTPUT CODE ----
#***********************************************************************      
      
    # Eqs, CURRENT LINE EQ ====
    output$eq_lm <- renderUI({
        linCombCalc <- paste0(input$aHat, ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, "x")

        withMathJax(
            paste(
                '\\( y=', linCombCalc, ' \\)'
            )
        )
    })
    
    # Eqs, GENERIC SSR ====
    ## (put here so we can insert the number of obs into the eq)
    output$eq_fullSSR <- renderUI({
        # Linear combo
        ## Whatever sign is on proposed parameter val will need to be flipped (b/c subtracting to get residual)
        linComb <- paste0('y_i', ifelse(input$aHat>=0, "-", "+"), abs(input$aHat), ifelse(input$b1Hat>=0, "-", "+"), abs(input$b1Hat), 'x_i')
        linCombOrig <- paste0('y_i - \\left(', ifelse(input$aHat>=0, "", "-"), abs(input$aHat), ifelse(input$b1Hat>=0, "+", "-"), abs(input$b1Hat), 'x_i \\right)')
       
        withMathJax(
            paste(
                '\\(\\begin{align}
                
                        \\sum \\hat{u}^2 &= \\sum\\limits_{i=1}^{', input$nObs, '} \\left\\{ \\left(y - \\hat{y}      \\right) ^2 \\right\\} \\\\
                                         &= \\sum\\limits_{i=1}^{', input$nObs, '} \\left\\{ \\left( ', linCombOrig, '\\right) ^2 \\right\\} \\\\
                                         &= \\sum\\limits_{i=1}^{', input$nObs, '} \\left\\{ \\left( ', linComb,     '\\right) ^2 \\right\\}
                    \\end{align}
                \\)'
            )
        )
    })

    # Eqs, SPECIFIC SSR [all] ====
    output$eq_fullSSR_spec <- renderUI({
        
        equation <- paste0('\\( \\begin{align}
                                    \\sum \\hat{u}^2 &= ')
        
        # Loop through every single observation   
        for(i in 1:input$nObs){

            # do linear combo
            ## Whatever sign is on parameter guess, will need to be flipped (b/c subtracting to get residual)
            linComb <- paste0(round(all()[[1]][i,1], 3), 
                              ifelse(input$aHat>=0, "-", "+"), abs(input$aHat), 
                              ifelse(input$b1Hat>=0, "-", "+"), abs(input$b1Hat), 
                              '*', round(all()[[1]][i,2], 3))
            
            # get proper sign depending on whether the DV for this obsv is a 0 or 1 with ifelses
            equation <- paste0(equation, ifelse(i!=1, "& \\phantom{=} {} +", ""), ' \\left. \\left( ',  
                               linComb, ' \\right)^2 \\right. \\\\~') 
        }
            
        # Insert alignment break
        equation <- paste0(equation, ' \\\\')
        
        # Insert SSR value.
        ssr <- ifelse(input$solnButton==0, round(obsSSR()[[1]], 5), round(all()[[3]],5))
        equation <- paste0(equation, ' \\sum \\hat{u}^2 &=', ssr)

        # Put MathJax closer tag
        equation <- paste0(equation, ' \\end{align} \\)')
        
        # Print it
        withMathJax(
            equation
        )
    })
    
    
    # MAIN GRAPH ====
    output$gph<- renderPlot({        
        # to force the plot to have stableish dimensions w/some cushion
        yRng <- cushion(all()[[1]]$y , .2)
        xRng <- cushion(all()[[1]]$x1, .2) 
        
        # get four corners
        yHat <- input$aHat + input$b1Hat*all()[[1]]$x1
        res <- all()[[1]]$y - yHat
        
        yMax <- apply(cbind(all()[[1]]$y, yHat), 1, max)  
        yMin <- apply(cbind(all()[[1]]$y, yHat), 1, min)  
        xMax <- apply(cbind(all()[[1]]$x1, (all()[[1]]$x1 - res)), 1, max)  
        xMin <- apply(cbind(all()[[1]]$x1, (all()[[1]]$x1 - res)), 1, min)  
        
        df<- data.frame(yMax, yMin, xMax, xMin)
        
        # min/max value
        mm <- 15
        
        # graph
        gg<- ggplot(data=all()[[1]]) +
                geom_rect(aes(ymax=yMax, ymin=yMin,
                              xmax=xMax, xmin=xMin), size=0, alpha=0.3) + 
                geom_point(aes(x=x1, y=y), color="blue", alpha=0.9) + 
                geom_abline(slope = input$b1Hat, intercept = input$aHat, col = "red") +
            expand_limits(x=c(-mm, mm), y=c(-mm,mm)) +
            coord_cartesian(ylim=c(-mm,mm), xlim=c(-mm,mm), expand=FALSE) + 
            coord_equal() + 
            labs(x="x", 
                 y="y") 
        
        gg %>% shift_axes(.,0,0) 

    })
    
    # RAW MODEL ====  
    output$modObj <- renderPrint({ 
        print(summary(all()[[2]]), signif.stars=FALSE)  # to match glm output fmt 
    })
    
    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
        # number of places to round to
        places <- 2
        
        # get the raw HTML
        tab <- stargazer(all()[[2]], type="html",
                    covariate.labels = c("<em>x</em>"),
                    title="Model Results",
                    dep.var.labels  = "DV: <em>y</em>",
                    dep.var.caption = "",
                    add.lines = list(c("<em>&sigma;</em>", 
                                           round(summary(all()[[2]])$sigma, places)),
                                     c("<em>&sigma;</em><sup>2</sup>", 
                                           round(summary(all()[[2]])$sigma^2, places)),
                                     c("SSR", 
                                           round(all()[[3]], places))
                                    ),
                    keep.stat = c("N"),
                    report="vcs",               # just for you, Neal!
                    omit.table.layout="n",
                    digits=places,
                    digits.extra=4
            )
        
        # Nuke final table line
        tab <- gsub('<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr></table>',
                    '<tr><td colspan="2" style="border-bottom: 0px solid black"></td></tr></table>',
                    tab)
        
        # Change wording to n (brute forcing, rather than arguing with stargazer) 
        # ((no added line between SSR and n for this app))
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
    
    # CURRENT GUESS' SSR VALUE ====
    output$ssr <- renderUI({
        withMathJax(
            paste0('\\(', round(obsSSR()[[1]], 5), '\\)')
        )
    })
    
    # PRINT ALL-TIME MIN SSR ====
    output$bestGuess <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\(', round(bestSoFar$ssr, 5), '\\)')
        )
    })
    
    # PRINT ESTIMATES FOR ALL-TIME MIN SSR ====
    output$bestGuess_ests <- renderUI({
        allTime() # to force update JIC, in case last slider values are the winner and the reactive doesn't fire
        withMathJax(
            paste0('\\( \\alpha =', bestSoFar$alG, ',~\\beta_1 =', bestSoFar$b1G, ' \\)')
        )
    })
    
    # PRINT TRUE SLOPE + INTERCEPT ====
    ## (BASED ON ESTM MODEL)
    output$trueEsts <- renderUI({
        
        coeffs <- coef(all()[[2]])
        
        withMathJax(
            paste0('\\( 
                        \\alpha =' , round(coeffs[1], 3), 
                     ',~\\beta_1 =', round(coeffs[2], 3), 
                   '\\)')
        )
    })
    
    # PRINT ANSWER/TRUE SSR ====
    output$trueSSR <- renderUI({ # Otherwise, this evaluates from the get-go, before the button's even pushed.
        withMathJax(
            paste0('\\(', round(all()[[3]], 5), ' \\)')
        )
    })
    
    # DT: ACTUAL DATASET ====
    output$data_table <- DT::renderDataTable({
        # To force the render, in case the user clicks the "Data" tab before gening dataset
        if(input$dataGenButton>0){
            dat <- all()[[1]]
            ssr <- round(obsSSR()[[1]], 3)
            dat$uHatSq <- obsSSR()[[2]]
            n  <- input$nObs # avoid subset error
            
            # Custom head/footer
            sketch <- htmltools::withTags(table(
                class = 'display',
                
                # header
                DT::tableHeader(cbind("ID"=NA, dat)),
                
                # footer
                tfoot(
                    tr(
                        th(colspan="4", paste0("TOTAL (across all ", n, " observations): ", round(sum(dat$uHatSq),3)))
                    )
                )
            ))
            
            # Print it
            DT::datatable(dat,
                          escape=TRUE,
                          rownames=TRUE,
                          filter = "none",
                          container = sketch, 
                          colnames = c("\\(y\\)", "\\(x\\)", "\\(\\hat{u}^2\\)"),
                          options = list(
                              pageLength = ifelse(n==25, 25, 15),
                              dom = ifelse(n<=25, 't', 'l t p'), # hide length + page selector if less than 25, show othw
                              columnDefs = list(
                                  list(className = 'dt-center', targets = c(0))
                              )
                          )
            ) %>% 
              DT::formatRound(c(1:ncol(dat)), 3)
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
    })
    
    # Results div - red outline kick ====
    ## MISC: put temporary red outline around results, 
    ## if user's navigated from anchor link (then toss
    ## outline once user's moused over table)
    runjs('
        $(\'a[href="#wrapper_rslts"]\').click(function(){ // when the anchor is clicked
            
            $("#wrapper_rslts").css({"boxShadow" : "0 0 20px 5px #d9534f"});  //"border"    : "2px solid #d9534f", 
            
            // Change everything back once you mouseover the results
            $("#wrapper_rslts").mouseover(function() { 
                $("#wrapper_rslts").css({"boxShadow" : "0 0 20px 5px rgba(255, 0, 0, 0)" })  //"border"    : "rgba(255, 0, 0, 0)", 
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
                
                hide(id = "inf_ptTotal")
                hide(id = "inf_bestGuess")
                hide(id = "formal_actual")
                hide(id = "restoreButton")
            }
            
            hide(id = "fullSSRButton")
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
            
            shinyjs::show(id = "fullSSRButton")
            shinyjs::show(id = "formal_instrText")
            shinyjs::show(id = "formal_ptTotal")
            shinyjs::show(id = "formal_bestGuess")
            
            if(input$solnButton!=0){
                shinyjs::show(id = "formal_actual")
                shinyjs::show(id = "formal_bestGuess_ans")
                
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
    
    # Restore best guess ====
    observeEvent(input$restoreButton, {
        updateSliderInput(session, "aHat", value=list(bestSoFar$alG))
        updateSliderInput(session, "b1Hat", value=list(bestSoFar$b1G))
    })
    
    # Housekeeping
    session$onSessionEnded(stopApp) 
}    