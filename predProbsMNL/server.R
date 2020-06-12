# Since will be the same for all R sessions, just put data import + reshape out here
## Import
if(exists("dat")==FALSE){
    dat <- read_dta("CSES - Mexico 2012 (trimmed).dta") %>%                     # read in
            mutate(id = seq(1:dim(.)[1])) %>%                                   # create IDs for mlogit.data
            rename(ideol = D3014, relig = religCol, vote = voteCol_str) %>%     # rename to shorter things
            select(vote, relig, ideol, ed1, ed2, ed3, ed4, ed5, id)             # only keep what we need
}
## Reshape
if(exists("mexican")==FALSE){
    mexican <- mlogit.data(dat, shape = "wide", choice = "vote", id.var = "id")
}

# Load saved Stata results
source("serverPt_stataRes.R", local=TRUE)


# Start of server code
server <- function(input, output, session){

    # To flag whether it's the first time through the model or not, plus
    # the list of alternatives (in case you want this in a non-reactive setting.)
    modChars <- reactiveValues(fRun = 0,                # doesn't technically need to be reactive, but to economize.
                               choiceList = NULL,       # choice list
                               updateChoices = FALSE)   # to trigger the observeEvent for the checkboxes
    
#***********************************************************************
# INTERMEDIATE CALCULATIONS 
#***********************************************************************  
    # > !!!! SERVER'S ACTIONS START HERE (triggered by button press) !!!! <
    # Run the regression (from this point forward, all the other calculations will auto-update).
    ## ESTM MODEL ====
    results <- eventReactive(input$estmButton, {  
        # Build string
        cmd <- paste0("mlogit(vote ~ 1 | ed2 + ed3 + ed4 + ed5 + as.numeric(relig) + as.numeric(ideol), 
                              data=mexican, reflevel = '", input$reflevel, "')")
        # Estimate model
        eval(parse(text=cmd))
    })
    
    ## PRED PROB EQS: SPECIFIC ====
    fFormPrExpr <- reactive({
        # To set educ dummy variables
        ed2 <- ifelse(input$coeffEduc==1, 1, 0)
        ed3 <- ifelse(input$coeffEduc==2, 1, 0)
        ed4 <- ifelse(input$coeffEduc==3, 1, 0)
        ed5 <- ifelse(input$coeffEduc==4, 1, 0)

        # Get coeffs
        betas <- coef(results())

        # Calc pred prob
        ## Construct stacked vector with covariate values
        set <- data.frame(ed2 = ed2, ed3 = ed3, ed4 = ed4, ed5 = ed5, relig = input$coeffRelig, ideol = input$coeffIdeo)
        set <- rbind(set, set, set, set, set)
        
        ## Gen pred probs
        vals <- predict(results(), newdata = set) 

        # Start building the prose interpretation pieces here, because it's ultimately going to be very long
        educTxt <- ifelse(as.numeric(input$coeffEduc)==0, "either no education or an early childhood education only",
                   ifelse(as.numeric(input$coeffEduc)==1, "a primary school education",
                   ifelse(as.numeric(input$coeffEduc)==2, "a lower-secondary school education",
                   ifelse(as.numeric(input$coeffEduc)==3, "an upper-secondary school education", "a college education"))))
        
        # Rest of prose interp.  Only thing that needs doing = swapping out NARWHAL with actual outcome descriptor.
        expl <- paste0('(predicted) probability that a ', ifelse(as.numeric(input$coeffRelig)==0, "non-religious", "religious"), 
                        ' voter with ', educTxt,
                        ' and self-reported ', input$coeffIdeo, ' on a 10-point left-right scale 
                        voted for the NARWHAL during the first round of the 2012 Mexican presidential election.')

        # create linear combos for each outcome
        ## If we're running the model for the first time
        if(modChars$fRun==0){
            # pull all choices (based on coeffs, so takes care of baseline poss changing)
            choices <- strsplit(names(betas), ":")  %>%
                        lapply(., function(x) x[2])  %>%
                        unlist() %>%
                        unique()
                               
            # push the choices back to the big reactiveValues obj
            modChars$choiceList <<- choices
        
        # If it's not the first time, can pull values from reactiveValues obj
        } else    choices <- modChars$choiceList
        
        ## Empty holders for loop
        nums  <- NULL # to make it way easier to access everything in the second loop
        denom <- "1"  # For the huge string in the denominator (start of "1 +...")
        
        ## begin looping
        for(c in 1:(length(choices)+1)){
            # If this isn't the reference category, build the numerator
            if(c!=(length(choices)+1)){
                betas.k <- betas[grepl(choices[c], names(betas))]
    
                # Short expression as temp
                linCombCalc <- paste0(signif(betas.k[1],4),
                                        signSF(betas.k[2]), "*", ed2,
                                        signSF(betas.k[3]), "*", ed3,
                                        signSF(betas.k[4]), "*", ed4,
                                        signSF(betas.k[5]), "*", ed5,
                                        signSF(betas.k[6]), "*", input$coeffRelig,
                                        signSF(betas.k[7]), "*", input$coeffIdeo
                                     )
    
                # Numerator: Begin constructing expression
                nums[c] <- linCombCalc
            
                # Denominator
                denom <- paste0(denom, " + \\exp \\left(", linCombCalc, "\\right)")
            
            # If this *is* the ref cat, numerator = 1, denom contrib = 1 (see init denom def)
            } else    nums[c] <- 1

        }

        # Now that you've looped over everything, can construct the actual expressions now
        for(c in 1:(length(choices)+1)){
            # Get choice name
            ch <- choices[c]
            if(is.na(ch))   ch <- input$reflevel # if it's NA, must be that this is the ref cat
            
            # the general functional form
            ff <- paste0('\\widehat{\\Pr(y_\\text{', ch,'} = 1)} &= \\frac{',
                    ifelse(c<=length(choices), 
                           paste0('\\exp \\left(', nums[c],'\\right)'),
                           '1'),
                    '}{', denom, '}')  
            
            # Get temp pred prob
            predPr <- paste0(
                '\\\\ \\widehat{\\Pr(y_\\text{', ch,'} = 1)} &= ', signif(vals[ch],3),
                        ' \\Leftarrow \\text{', ifelse(!grepl("None", ch), gsub("NARWHAL", ch, expl), 
                                                                           gsub("voted for the NARWHAL", 
                                                                                "voted for none of the parties or cast a blank ballot", expl)
                                                      ), 
                '}'
            )

            # Assemble the aligned equations
            eqCmd <- paste0("\\( \\begin{align} ", ff, predPr, " \\end{align} \\)")
            cmd <- paste0("assign('final", c, "', '", gsub("\\", "\\\\", eqCmd, fixed=TRUE), " ') ") 
                # ^ gsub puts in quadruple backslashes -> needed because we need to escape char s.t. two backslashes remain
           
            # Generate
            eval(parse(text=cmd))
        }
        
        # Update reactives
        modChars$fRun <<- 1
        modChars$updateChoices <<- TRUE

        # Post
        return(
            list(c(final1, final2, final3, final4, final5), choices)
        )
    })
    
    # GROUP CHECKBOX UPDATER ====
    observeEvent(isTRUE(modChars$updateChoices), {
        # Pull choices
        choices <- modChars$choiceList
        
        # Get choices into object (b/c need to append ref cat)
        choiceNms <- lapply(choices, function(x) paste0("Pr(",x,")")) %>% unlist
        choiceNms <- c(choiceNms, paste0("Pr(", input$reflevel,")"))
         
        # Create buttons, one for each choice (incl. reference cat)
        updateCheckboxGroupButtons(session, "eqBtns", 
                                   choiceNames = choiceNms,
                                   status = "info", 
                                   choiceValues = seq(1:length(choiceNms)),
                                   selected = "1", 
                                   checkIcon = list(yes = icon("eye-open" , lib = "glyphicon"), 
                                                    no  = icon("eye-close", lib = "glyphicon"))
                                   )
    })
        
        
#***********************************************************************
# EQ OUTPUT OBJECTS ----
#***********************************************************************  
    # OUTPUT: k1 
    output$k1Eq <- renderUI({
        withMathJax(fFormPrExpr()[[1]][1])
    })

    # OUTPUT: k2 
    output$k2Eq <- renderUI({
        withMathJax(fFormPrExpr()[[1]][2])
    })

    # OUTPUT: k3 
    output$k3Eq <- renderUI({
        withMathJax(fFormPrExpr()[[1]][3])
    })

    # OUTPUT: k4
    output$k4Eq <- renderUI({
        withMathJax(fFormPrExpr()[[1]][4])
    })

    # OUTPUT: k5 [ref cat]
    output$k5Eq <- renderUI({
        withMathJax(fFormPrExpr()[[1]][5])
    })

    # OUTPUT: K (contains only non-baseline cats, with how it's been defined here)
    # CHOICE SET MATHJAX ====
    output$choices <- renderUI({
        chList <- paste0(unlist(fFormPrExpr()[[2]]), collapse=", ")
        withMathJax(
            paste0(' \\( K = \\{ \\text{', chList, '} \\} \\)')     
        )
    })

        
#***********************************************************************
# MODEL OBJECT OUTPUT ----
#*********************************************************************** 
    # R ====
    output$modObj <- renderPrint({
        summary(results())
    }, width = 85)

    # STATA ====
    output$modObj_stata <- renderPrint({
        cat(stataRes[[input$reflevel]])    
    })

    # STARGAZER TABLE ====
    output$sgzTable <- renderUI({
        # number of places to round to
        places <- 2

        # Nice variable labels
        varNms <- c("Intercept",
                    "Educ. Lvl.: Primary",
                    "Educ. Lvl.: Lower Sec.",
                    "Educ. Lvl.: Upper Sec.",
                    "Educ. Lvl.: College",
                    "Religiosity",
                    "Ideology"
                    )

        # Load model
        mod <- results()

        # Generate fake model
        modFake <- lm(vote ~ ed2 + ed3 + ed4 + ed5 + as.numeric(relig) + as.numeric(ideol), data = mexican)
        names(modFake$coefficients) <- varNms

        # Break big mlogit obj into separate columns, one for each choice
        ## Get choice list
        choices <- fFormPrExpr()[[2]]

        ## Iterate over the list
        for(c in 1:length(choices)){
            # Pull the coefficients/SEs with this suffix
            ## Coeffs
            coefTemp <- coef(mod) 
            coefTemp <- coefTemp[grepl(paste0(choices[[c]]), names(coefTemp))]
            ## SEs
            seTemp <- sqrt(diag(vcov(mod))) 
            seTemp <- seTemp[grepl(paste0(choices[[c]]), names(seTemp))]

            # Assign
            assign(paste0("b", c) , coefTemp) # coefficients
            assign(paste0("se", c), seTemp)   # SEs
        }       

        # Make all the names match
        nmFix <- function(x){
            # Pull var labels from parent environment
            labs <- environment()  %>%
                      parent.env() %>%
                      .$varNms
            # Name
            names(x) <- labs

            # Return
            return(x)
        }

        # get the raw HTML
        tab <- stargazer(modFake, modFake, modFake, modFake,
                    type="html", 
                    covariate.labels = varNms,

                    coef = list( c(b1) , c(b2) , c(b3) , c(b4) ) %>% lapply(., nmFix),
                    se   = list( c(se1), c(se2), c(se3), c(se4)) %>% lapply(., nmFix),

                    column.labels = lapply(choices, function(x) paste0("Pr(",x,")")) %>% unlist,
                    model.numbers = FALSE,
                    title="Model Results",

                    dep.var.labels  = "<em>DV: Vote Choice</em>",
                    dep.var.caption = "",

                    add.lines = list(c("ln<em>L</em>", 
                                       round(logLik(mod), places)),
                                     c("<em>n</em>", nrow(mod$residuals)),
                                     paste0("SEs in parenthesis, reference category: ", input$reflevel)
                                ),

                    intercept.bottom = FALSE,
                    intercept.top = TRUE,

                    omit.stat = c("all"),
                    report="vcs",               # just for you, Neal!
                    omit.table.layout="n",
                    column.sep.width = "15pt",

                    digits=places,
                    digits.extra=4,

                    notes.label=paste0("SEs in parenthesis, reference category: ", input$reflevel),
                    notes.append=FALSE,
                    notes.align="l"
            )

        # Merge the notes columns + add border to separate it from model stats
        tab <- gsub('<td style="text-align:left">SEs in parenthesis,',
                    '<td style="text-align:left; border-top: 1px solid black;" colspan="5">SEs in parenthesis,',
                    tab)

        # Nuke final table line
        tab <- gsub('<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>',
                    '<tr><td colspan="5" style="border-bottom: 0px solid black"></td></tr></table>',
                    tab)

        # Bump width of table
        tab <- gsub('<table style="text-align:center">',
                    '<table style="text-align:center; width:30em;">',
                    tab)

        # Center the caption
        tab <- gsub('<caption>',
                    '<caption style="text-align:center;">',
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
    
    # MISC: gray out the covariate widgets until the "Estimate Model" button's pushed
    observeEvent(input$estmButton, {
        disable("reflevel")
        enable("coeffEduc")
        enable("coeffIdeo")
        enable("coeffRelig")
        hide("instrText")
        show("refreshInstr")
    })

    # MISC: turning on and off the various equations
    observeEvent(input$eqBtns, {
        toggle(id = "kFake", condition = is.null(input$eqBtns))
        toggle(id = "k1", condition = any(grepl("1", input$eqBtns)))
        toggle(id = "k2", condition = any(grepl("2", input$eqBtns)))
        toggle(id = "k3", condition = any(grepl("3", input$eqBtns)))
        toggle(id = "k4", condition = any(grepl("4", input$eqBtns)))
        toggle(id = "k5", condition = any(grepl("5", input$eqBtns)))
    }, ignoreNULL=FALSE) # So it'll fire when the list is empty.
    
    # kill connection to server once app stops running 
    session$onSessionEnded(stopApp)
}