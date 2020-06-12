## Is OK if these load once across multiple R sessions to save time b/c they're static
    # Stata's calc for robust SEs
    s.se.rbst <- function(object, ...) sandwich(object) * nobs(object) / (nobs(object) - 1)

    # Load so accessible to any of the functions immediately
    dat <- read_dta("hmst08.dta") %>%           # read in  
            subset(., complete.cases(.)) %>%    # keep estm sample only
            rename(maritime = mariss,           # rename to more intuitive things
                   river = riveriss, 
                   salience = icowsal, 
                   pMID = recmidwt, 
                   pPeaceF = recnowt, 
                   demDyad = democ6)

    # Load the Stata results
    source("serverPt_stataRes.R", local=TRUE)


# > SERVER FUNCTION ----
server <- function(input, output, session){
    
#***********************************************************************
# INTERMEDIATE CALCULATIONS ----
#***********************************************************************  
    # > !!!! SERVER'S ACTIONS START HERE (triggered by button press) !!!! <
    # Run the regression (from this point forward, all the other calculations will auto-update).
    results <- eventReactive(input$estmButton, {
        cmd<- paste0('glm(', 
                     ifelse(input$dv=="MID", "midissyr", "attpeace"), 
                     ' ~ maritime + river + salience + pMID + pPeaceF + demDyad + relcaps, data=dat, family="binomial")')
        eval(parse(text=cmd))

   })
    
    # Outsource the pred prob calc, so that it happens only once.
    predPrVal <- reactive({
        # To get issue dummy variables set correctly
        setIssVars(is2, is3)
        
        # Gen pred probs for this covariate profile
        val <- predict(results(), type="response", 
                       newdata = data.frame(maritime=is2, river=is3, salience=input$coeffSal, 
                                            pMID=input$coeffPastMID, pPeaceF=input$coeffPastPcFail, 
                                            demDyad=as.numeric(input$coeffDemocDy), relcaps=(input$coeffCINC/100)))    
        # Return the value
        return(val)
    })
    
#***********************************************************************
# OUTPUT OBJECTS ----
#***********************************************************************  
    # OUTPUT: which model text to show
    output$modNum <- renderText({
        paste0("(Replicates table 2, model ", ifelse(input$dv=="MID", "2", "4"),")")
    })
    
    # OUTPUT: the longhand eq w/dynamic components
    output$fFormPrExpr <- renderUI({  
        
        # To get issue dummy variables set correctly
        setIssVars(is2, is3)
        
        # Get coeffs
        betas <- coef(results())
        
        # Begin constructing expression
        linCombCalc <- paste0(signif(betas[1],4),
                                signSF(betas[2]), "*", is2,
                                signSF(betas[3]), "*", is3,
                                signSF(betas[4]), "*", input$coeffSal,
                                signSF(betas[5]), "*", input$coeffPastMID,
                                signSF(betas[6]), "*", input$coeffPastPcFail,
                                signSF(betas[7]), "*", input$coeffDemocDy,
                                signSF(betas[8]), "*", (input$coeffCINC/100)
                       )
        
        # the general functional form
        ff <- paste0('\\( \\begin{align} \\widehat{\\Pr( \\text{', ifelse(input$dv=="MID", "MID", "peaceful attempt"), '} = 1)} &= 
                        \\frac{\\exp \\left(', linCombCalc,'\\right)}
                              {1 + \\exp \\left(', linCombCalc,'\\right)}')
        
        # The actual pred prob value
        predPr <- paste0(
                    '\\\\ \\widehat{\\Pr( \\text{', ifelse(input$dv=="MID", "MID", "peaceful attempt"), '} = 1)} &= ', signif(predPrVal(),5) 
                  )

        # Output the final equation + pred prob value w/MathJax
        withMathJax(  
            paste(
                ff, predPr, ' \\end{align} \\)' 
            )
        )
    })

    # OUTPUT: text description (put into a separate textbox that's then styled as the MathJax font, so that it'll wrap legibly.)
    output$interp <- renderUI({
        
        paste0( 'In a given year, the model predicts there is a ', round(predPrVal()*100,2), '% chance that a ', 
                        ifelse(as.numeric(input$coeffDemocDy==1), "", "non-"), 'democratic dyad, 
                        where the stronger state has ', input$coeffCINC, '% of the dyad\'s military capabilities,
                        would experience a ', ifelse(input$dv=="MID", "militarized", "peaceful "), ' settlement attempt over a ',
                        ifelse(input$coeffIssue=="1", "territorial", ifelse(input$coeffIssue=="2", "maritime", "river")), ' issue with
                        a salience score of ', input$coeffSal, ' and past weighted settlement attempt record of ', 
                        input$coeffPastMID, ' (militarized, past 10 years)
                        and ', input$coeffPastPcFail,' (failed peaceful, past 10 years).')
     })
    
    # MODEL OBJECT - R ====
    output$modObj <- renderPrint({
        coeftest(results(), vcov = s.se.rbst)
    })
    
    # MODEL OBJECT - STATA ====
    output$modObj_stata <- renderPrint({
        cat(stataRes[[input$dv]])    
    })
    
    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
        # number of places to round to
        places <- 2
        
        # Load
        mod <- results()
        
        # Nice covariate list
        varNms <- c("Maritime Issue?",
                    "River Issue?",
                    "Within-Issue Salience",
                    "# Past MIDs",
                    "# Past Failed Peaceful Atts.",
                    "Democratic Dyad?",
                    "Relative Caps",
                    "Constant"
                  )
        
        # Build DV label
        dv <- input$dv
        if(dv=="peaceful") dv <- "Peaceful Att"
        dv <- paste0("<em>DV: ", dv, "?</em>")
        
        # get the raw HTML
        tab <- stargazer(mod,
                    type="html", 
                    covariate.labels = varNms,
                    title="Model Results",
                    
                    dep.var.labels  = dv,
                    dep.var.caption = "",
                    
                    se = list(sqrt(diag(s.se.rbst(mod)))),
                    
                    add.lines = list(c("ln<em>L</em>", 
                                       round(logLik(mod), places))
                                ),
                    keep.stat = c("N"),
                    report="vcs",               # just for you, Neal!
                    star.cutoffs = NA,
                    
                    column.sep.width = "10pt",
                    digits=places,
                    digits.extra=4,
                    
                    notes.label="Robust SEs in parenthesis.",
                    notes.append=FALSE,
                    notes.align="l"
            )
        
        # Merge the notes columns + add border to separate it from model stats
        tab <- gsub('<td style="text-align:left">Robust SEs in parenthesis.',
                    '<td style="text-align:left; border-top: 1px solid black;" colspan="2">Robust SEs in parenthesis.',
                    tab)
        
        # Notes are being dumb and I don't feel like figuring out why, so brute force it.
        tab <- gsub('<td style="text-align:left">NA</td>',
                    '',
                    tab)
        
        # Merge the DV label columns, align right
        tab <- gsub(paste0('<tr><td style="text-align:left"></td><td>', dv, '</td>'),
                    paste0('<tr><td colspan="2" style="text-align:right">', dv, '</td>'),
                    tab)
        
        # Nuke final table line
        tab <- gsub('<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr></table>',
                    '<tr><td colspan="2" style="border-bottom: 0px solid black"></td></tr></table>',
                    tab)
        
        # Change wording to n (brute forcing, rather than arguing with stargazer) 
        tab <- gsub('<tr><td style="text-align:left">Observations</td>',
                    '<tr><td style="text-align:left"><em>n</em></td>',
                    tab)
        
        # Bump width of table (+ reset font size, because theme's shrinking it)
        tab <- gsub('<table style="text-align:center">',
                    '<table style="text-align:center; width:20em; font-size:1em;">',
                    tab)
        
        # Turn all border lines white
        tab <- gsub('black',
                    'white',
                    tab)
        
        # Center the caption + put in lighter color
        tab <- gsub('<caption>',
                    '<caption style="text-align:center; color:#aaa;">',
                    tab)
        
        # return as wrapped HTML
        HTML(tab)
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
    
    # MISC: the helper function for setting issue type dummy variables correctly
    setIssVars <- function(obj1=0, obj2=0){
        if(input$coeffIssue=="1"){
            assign(deparse(substitute(obj1)), 0, env=parent.frame())
            assign(deparse(substitute(obj2)), 0, env=parent.frame())
            
        } else if(input$coeffIssue=="2"){
            assign(deparse(substitute(obj1)), 1, env=parent.frame()) 
            assign(deparse(substitute(obj2)), 0, env=parent.frame())

        } else{
            assign(deparse(substitute(obj1)), 0, env=parent.frame()) 
            assign(deparse(substitute(obj2)), 1, env=parent.frame())
        }
    }
    
    # MISC: gray out the covariate widgets until the "Estimate Model" button's pushed (JavaScript)
    observeEvent(input$estmButton, {
        # Prevent DV from being changed + disable estm button
        disable("estmButton")
        disable("dv")
        
        # Hide note about "estm model first", but show the
        # FYI about refreshing the app to get the other DV
        shinyjs::hide("instrText")
        shinyjs::show("reestText")
        
        # Enable all the covariate value sliders
        enable("coeffIssue")
        enable("coeffSal")
        enable("coeffPastMID")
        enable("coeffPastPcFail")
        enable("coeffDemocDy")
        enable("coeffCINC")
        
        # Show the div w/the mean btn + enable mean btn
        shinyjs::show("setMeanDiv")
        enable("setMeanBtn")            # is wrapped in the same disable tag as the covariate sliders, so need to reenable
    })
    
    # MISC: on "to means" btn push, set everything (except issue vars) to either mean or median   
    observeEvent(input$setMeanBtn, {
        enable("resetSldrBtn")
        shinyjs::show("resetSldrDiv")
        shinyjs::hide("setMeanDiv")

        updateSliderInput(session, "coeffSal", 
            min=0, max=12, step = 0.0001, value=mean(dat$salience) 
        )
        updateSliderInput(session, "coeffPastMID",          
            min = 0, max = 5.4, step = 0.0001, value=mean(dat$pMID) 
        )
        updateSliderInput(session, "coeffPastPcFail", 
            min = 0, max = 10.3, step = 0.0001, value=mean(dat$pPeaceF) 
        )
        updateRadioButtons(session, "coeffDemocDy", 
            selected=median(dat$demDyad) 
        )
        updateSliderInput(session, "coeffCINC", 
            min = 50, max = 100, value=mean(dat$relcaps)*100 
        )
    })

    # MISC: reset the slider increments to usual after mean/median
    observeEvent(input$resetSldrBtn, {
        shinyjs::hide("resetSldrDiv")
        shinyjs::show("setMeanDiv")

        # Salience
        current <- isolate(input$coeffSal)
        updateSliderInput(session, "coeffSal",
            min=0, max=12, step = 1, value=current
        )

        # Past MID
        current <- isolate(input$coeffPastMID)
        updateSliderInput(session, "coeffPastMID",
            min = 0, max = 5.4, step = 0.1, value=current
        )

        # Past failed peaceful atts
        current <- isolate(input$coeffPastPcFail)
        updateSliderInput(session, "coeffPastPcFail",
            min = 0, max = 10.3, step = 0.1, value=current
        )
    })
    
    # kill connection to server once app stops running   
    session$onSessionEnded(stopApp)
}