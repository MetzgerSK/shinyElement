## NOTE: Major portions of the non-Shiny code come from Beauchamp's (2017) replication files. 
## (https://doi.org/10.7910/DVN/RJAUNW)

# load all the data *outside* the server function, will persist for all sessions.
options(future.globals.maxSize=1e9) 

# JIC, still wrapping in future
datStatus <- future({
    # read in election results from Stata file:
    statapolls1 <<- read.dta("election_prediction_from_R_d.dta") %>%
                    .[,c(1,5,6)]
    
    # Load in model results (pre-estimated to save space)
    load("allModEsts.Rdata")
    megaMod <<- megaMod
    
    endtime <<- Sys.time()
})


# > SERVER FUNCTION ----
server <- function(input, output, session){
    # For the data tables
    resFit <- NULL
    resCoeff <- NULL
    
    # For the coefficient results
    coefRes <- list() # Code Ocean likes this as <- NULL (R 4.0),
                      # but Binder likes it as <- list() (R 3.6).
    
    # For loading screen
    rv <- reactiveValues()
    rv$setupComplete <- FALSE
    rv$setupComplete <- finally(datStatus, function() {rv$setupComplete <- TRUE} )

    # Create reactives for the two key params, so as not to rely on slider updates
    rv_alpha  <- reactiveVal(NULL)
    rv_lambda <- reactiveVal(NULL)
    
    # Just for a slight pause, if everything's already loaded, so people can read RQ.
    Sys.sleep(3) 


  # KEY CALCS ---- 
    # > !!!! SERVER'S ACTIONS START HERE (triggered by button press) !!!! <
    estimate <- reactive({
        paste(input$dataGenButton, input$solnButton)
    })

    ### > Estimate the model ====
    all <- eventReactive(estimate(), {
        # Hide the "Note..." on the various pages + graph caption
        shinyjs::hide(selector="h4.simFyiHdr")
        hideElement("gphCapt")
        
        # Force update of lambda and alpha for the calculations
        ## (If this triggers b/c of solnButton, the sliders won't auto-update
        ## until after these calcs finish, which is too late.)
        rv_lambda(ifelse(input$solnButton==1, 0.001, isolate(input$lambda)))
        rv_alpha( ifelse(input$solnButton==1, 1,     isolate(input$alpha)))
        
        # Pull relevant model results
        info <- megaMod[[paste0("l", rv_lambda())]]

        # Insert lambda into info (into list in first element, as third list item)
        info[[1]][[3]] <- rv_lambda()
        
        # Return    
        return(info)
    })

    ### MISC KEY FUNCTIONS ====
    # Round + force dp display + unlist function (to make print code lines less cluttered)
    roundUnl <- function(obj, pl){
        return(format(round(unlist(obj),pl), nsmall=pl))   
    }
    
    # AIC/BIC calc code (and also MSE while at it; R^2 easier to do from calling function)
    ic.fit <- function(model, df, yHat, y, lam){
       
        # Looks at predicted elect result vs actual result
        
        # Compute MSE
        resid <- y - yHat
        mse <- mean(resid^2)
        
        # Compute AIC, BIC
        n <- length(y)
        k <- df + 1
        bic <- n*log(mse) + k*log(n)
        aic <- n*log(mse) + 2*k
        
        return(list(aic, bic, mse))
    }
    
    
#***********************************************************************
# OUTPUT CODE ----
#***********************************************************************      
        
    # > OUTPUT: Eqs, CURRENT FUNCT ====
    output$eq_lasso_gen <- renderUI({
        # Kludge (will have to fix in earnest if you ever reenable alpha buttons)
        if(is.null(input$alpha)){
            holder <- 1
        }
        else holder <- 1 
        
        # Write out the correct penalty expression, depending on alpha value
        penExp <- ifelse(holder,
                         paste0(input$lambda, "\\left( \\left| \\beta_1 \\right| + \\left| \\beta_2 \\right| \\right)"),
                         paste0(input$lambda, "\\left(\\beta_1^2 + \\beta_2^2 \\right)")
                  )
            
        linCombCalc <- paste0( "\\underbrace{\\left( y - \\beta_1 x_1 - \\beta_2 x_2 \\right)^2}_{\\text{regular OLS}} 
                              + \\underbrace{ ", penExp, "  }_{\\text{the ", ifelse(holder==0, "ridge reg.", "LASSO"), " penalty}}")
         
        # Loss function's equation
        withMathJax(
            paste(
                '\\( \\operatorname*{arg min}\\limits_{\\beta_1, \\beta_2} \\biggl\\{ ', linCombCalc, ' \\biggr\\}  \\)'
            )
        )
    })
    
    # > OUTPUT: generalized loss function (in the sense that X and b are all matrices/vectors now)
    # > OUTPUT: Eqs, C. FUNCT AS MATRIX ====
    output$eq_lasso_spec <- renderUI({
        # Kludge (will have to fix in earnest if you ever reenable alpha buttons)
        if(is.null(input$alpha)){
            holder <- 1
        }
        else holder <- 1 #input$alpha
        
        # Write out the correct penalty expression, depending on alpha value
        penExp <- ifelse(holder==1,
                         paste0(rv_lambda(), " \\sum_{k=1}^{K} \\left( \\left| \\beta_k \\right|  \\right)"),
                         paste0(rv_lambda(), " \\sum_{k=1}^{K} \\left( {\\beta_k}^2 \\right)")  
                  )
            
        linCombCalc <- paste0( "\\left( y - \\mathbf{X} B \\right)^2 + ", penExp)
         
        # Loss function's equation 
        withMathJax(
            paste(
                '\\( \\operatorname*{arg min}\\limits_{B} \\biggl\\{ ', linCombCalc, ' \\biggr\\}  \\)'
            )
        )
    })
    
    # > OUTPUT: Graph, PRED VS REAL ====
    ## Graph actual
    output$predVsReal <- renderPlotly({

        req(input$dataGenButton!=0)

        rects <- data.frame(xstart = c(0, 0.5), xend = c(0.5, 1), col = c("Romney", "Obama")) 

        # Bash data into shape
        prednpoll2 <-  data.frame(all()[2]) %>% 
            mutate(state = gsub("-", " ", statev)) %>%
            mutate(state = tools::toTitleCase(state)) %>%
            mutate(state = gsub("Dc", "DC", state)) # to deal with DC
        pred.dem <- subset(prednpoll2, prednpoll2$textpart>0.5) 
        pred.rep <- subset(prednpoll2, prednpoll2$textpart<0.5)

        # Define tooltip once
        tooltipInfo <- 'paste0(                                                         
                               "<b>State</b>: ", state, "\n" ,
                               "<b>Predicted</b>: ", round(100*textpart, 2), "% \n",
                               "<b>Actual</b>: ", round(100*partypct, 2), "% \n",
                               "<b>In LASSO Sample?</b>: ", ifelse(ourstates==1, "Yes", "No"), "\n"
                        )'

        # Cast as ggplot
        p <- ggplot() +
            geom_rect(data = rects, aes(ymin = xstart, ymax = xend, xmin = min(pred.rep$textpart) - 0.1, 
                                        xmax = max(pred.dem$textpart) + 0.1, 
                                        fill = col #can't do -inf, inf b/c plotly freaks out.
                                    ), 
                      fill=c("#F8766D", "#619CFF"), alpha = 0.4) +  
            geom_point(data = pred.dem, color = "#0c4c8a", shape = 19,
                        aes(x = textpart, y = partypct,
                            text = eval(parse(text=tooltipInfo))
                        )
                      ) +
            geom_point(data = pred.rep, color = "#ef3b2c", shape = 17,
                        aes(x = textpart, y = partypct,
                            text = eval(parse(text=tooltipInfo))
                        )
                      ) +
            geom_vline(xintercept=0.5) + geom_hline(yintercept=0.5) +
            scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent) +
            coord_cartesian(xlim=c(min(pred.rep$textpart) -0.01 , max(pred.dem$textpart) + 0.01)) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  text = element_text(family = "PT Sans Narrow", size=15)) +   
            labs(title = "2012 US Presidential Election Results",
                 x = "Predicted Obama Vote Share", 
                 y = "Actual Obama Vote Share", 
                 caption = "scatter colors/shapes: predicted winner \n shaded areas: actual winner"
            ) 

        # Show the caption, now that graph's about to be rendered
        shinyjs::show("gphCapt")

        # Kick out the plotly
        ggplotly(p, tooltip = "text") %>%
            style(hoverlabel = list(font = list(family="PT Sans Narrow", size="15"))) %>%   
            style(hoverinfo = "none", traces = c(0,1,2) )
    })

    
    # > OUTPUT: Graph, four-panel plot of lambda vs. (EACH FIT STAT HERE) ====
    output$lamFitPlot <- renderPlotly({
        # Make sure this updates any time the buttons get pushed
        req(input$dataGenButton!=0)
        input$solnButton
        
        # Get the data
        d <- tbl_df(resFit) %>% select(-bin1.2.3) %>% type_convert()
        
        # Define all the panels 
        aic <- plot_ly(d, x = ~lambda, y = ~in.aic, name="In Sample", type="scatter", mode="lines+markers", 
                                marker=list(color="#1f78b4"), line=list(color="#1f78b4")) %>%   
                add_trace(x = ~lambda, y = ~oos.aic, name="Out of Sample", type="scatter", mode="lines+markers",
                                marker=list(color="#d95f02"), line=list(color="#d95f02")) %>%
                layout(xaxis = list(title="Lambda"), yaxis = list(title="AIC"))
        
        bic <- plot_ly(d, x = ~lambda, y = ~in.bic, name="In Sample", type="scatter", mode="lines+markers", showlegend = FALSE, 
                                marker=list(color="#1f78b4"), line=list(color="#1f78b4")) %>%
                add_trace(x = ~lambda, y = ~oos.bic, name="Out of Sample", type="scatter", mode="lines+markers", showlegend = FALSE,
                                marker=list(color="#d95f02"), line=list(color="#d95f02")) %>%
                layout(xaxis = list(title="Lambda"), yaxis = list(title="BIC"))

        mse <- plot_ly(d, x = ~lambda, y = ~in.mse, name="In Sample", type="scatter", mode="lines+markers", showlegend = FALSE, 
                                marker=list(color="#1f78b4"), line=list(color="#1f78b4")) %>%
                add_trace(x = ~lambda, y = ~oos.mse, name="Out of Sample", type="scatter", mode="lines+markers", showlegend = FALSE,
                                marker=list(color="#d95f02"), line=list(color="#d95f02")) %>%
                layout(xaxis = list(title="Lambda"), yaxis = list(title="MSE"))

        r2 <- plot_ly(d, x= ~lambda, y = ~in.r2, name="In Sample", type="scatter", mode="lines+markers", showlegend = FALSE, 
                                marker=list(color="#1f78b4"), line=list(color="#1f78b4")) %>%
                add_trace(x = ~lambda, y = ~oos.r2, name="Out of Sample", type="scatter", mode="lines+markers", showlegend = FALSE,
                                marker=list(color="#d95f02"), line=list(color="#d95f02")) %>%
                layout(xaxis = list(title="Lambda"), yaxis = list(title="R^2"))
        
        # Define and link the subplots
        subplot(aic, bic, mse, r2, nrows=2, shareX=TRUE, titleY=TRUE, margin = 0.05) 
        
    })
        
# > OUTPUT: Graph, lambda vs. # non-zero coeffs ====
    output$lamCoeffPlot <- renderPlotly({
        
        req(input$dataGenButton!=0)
        input$solnButton
        
        dta <- data.frame(lambda=as.numeric(resFit[,"lambda"]), nCoeffs=as.numeric(resFit[,ncol(resFit)]), stringsAsFactors = FALSE)
            
        # Cast as ggplot
        p <- ggplot(data = dta, aes(x = lambda, y = nCoeffs, group=1,
                            text = paste0(                                          
                               "Lambda: ", lambda, "\n",
                               "# Coeffs: ", nCoeffs, "\n"
                            ))
                    ) + 
            geom_line() +
            geom_point() + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  text = element_text(family = "PT Sans Narrow", size=15)) +   
            scale_y_continuous(trans="log10", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
            labs(x = "Lambda",
                 y = "# Non-Zero Coefficients"
            )

        # Kick out the plotly
        ggplotly(p, tooltip = "text") %>%
                style(hoverlabel = list(font = list(family="PT Sans Narrow", size="15")))
    })        
        
    # > OUTPUT: PRINT + APPEND FIT STATS ====
    output$fit.stats <- renderUI({ 
        req(input$dataGenButton!=0) 
        
        mod.coeffs  <- all()[[1]][[1]]
            numCoeffs <- sum(mod.coeffs!=0)
        mod.df <- all()[[1]][[2]]
        in2  <- data.frame(all()[2]) %>% filter(ourstates==1)
        out2 <- data.frame(all()[2]) %>% filter(ourstates==0)
        
        # In sample
        is  <- ic.fit(mod.coeffs, mod.df, in2$textpart , in2$partypct , rv_lambda()) 
            # R^2
            is.r2 <- cor(in2$textpart, in2$partypct)^2  
            is <- list(is, is.r2) %>% unlist()
                
        # OOS
        oos <- ic.fit(mod.coeffs, mod.df, out2$textpart, out2$partypct, rv_lambda())
            # R^2
            oos.r2 <- cor(out2$textpart, out2$partypct)^2
            oos <- list(oos, oos.r2) %>% unlist()
        
        # Deal with bin values here.
        b1 <- ifelse(input$solnButton==0, isolate(input$bin1), 2)
        b2 <- ifelse(input$solnButton==0, isolate(input$bin2), 3)
        b3 <- ifelse(input$solnButton==0, isolate(input$bin3), 4)
        
        # Append to the data table
        resFit <<- rbind(resFit,
                        cbind(rv_alpha(), 
                              rv_lambda(),
                              paste0("(", b1, ",", b2, ",", b3,")"),
                              roundUnl(is[1],2), roundUnl(oos[1],2), 
                              roundUnl(is[2],2), roundUnl(oos[2],2), 
                              roundUnl(is[3],3), roundUnl(oos[3],3),
                              roundUnl(is[4],3), roundUnl(oos[4],3),
                              as.numeric(isolate(input$solnButton)),
                              numCoeffs)
                   )
        colnames(resFit) <<- c("alpha" , "lambda", "bin1.2.3", 
                               "in.aic", "oos.aic", 
                               "in.bic", "oos.bic", 
                               "in.mse", "oos.mse",
                               "in.r2" , "oos.r2",
                               "replic", "#Non0Bs")  
        
        # Nuke any duplicates in data table (e.g., if user's tried the same combo already)
        resFit <<- unique(resFit)
        
        # Check specifically for the replication lambda observation; toss any
        # user-run observations with that lambda
        if(input$solnButton>0){
            resFit <<- resFit %>% data.frame %>% filter(., !(replic==0 & lambda=="0.001"))
        }
         
        # Print the stats for the main tab
        table <- NULL
        tabLabs <- c("\\text{AIC}", "\\text{BIC}", "\\text{MSE}", "R^2")
        # For each row
        for(i in 1:4){
            # Go over both columns
            for(d in c("is", "oos")){
                # depending on which column it is, insert the proper spacer to get the table
                sep <- ifelse(d=="is", 
                                ifelse(i==1, 
                                       paste0(tabLabs[i], " & "), 
                                       paste0(" \\\\ ", tabLabs[i], " & ")), 
                                " & ")
                
                # add cell to table
                table <- paste0(table, sep, roundUnl(eval(parse(text=d[1]))[i], ifelse(i<=2, 2, 3))) 
            }    
        }

        # Show the header and footer
        shinyjs::show("fitHdr") 
        shinyjs::show("fitFtr")
        
        # Render eq.
        withMathJax(
            paste0(" \\( 
                        \\begin{array}{r | c c} 
                            \\textit{Stat} & \\textit{In Sample} & \\textit{Out of Sample} \\\\
                            \\hline",
                            table,
                        "\\end{array}
                     \\)")
        )

    })    
  
    
    # > OUTPUT: FIT STAT DT ====
    output$dt_fit <- DT::renderDataTable({
        req(input$dataGenButton!=0)
        input$solnButton # to force the update when the replication model's run
        
        # Locally scoped only, note.
        resFit_tbl <- tbl_df(resFit) %>% select(-c(alpha, bin1.2.3)) %>% type_convert()
        
        # Table
        DT::datatable(resFit_tbl, rownames=FALSE, 
                      colnames=c("lambda",  "in.AIC", "oos.AIC", 
                                            "in.BIC", "oos.BIC", 
                                            "in.MSE", "oos.MSE",
                                            "in.R2" , "oos.R2",
                                            "Replic?", "#Non0Bs"),
                    options = list(
                        dom = 't' ,   
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                            "}"),
                        pageLength = nrow(resFit_tbl),
                        # hide last column
                        columnDefs = list( 
                                        list(targets = c(9), visible = FALSE), 
                                        list(className = 'dt-center', targets = '_all')
                                    )
                        )
                    )    %>%
            formatRound(2:5, digits=2) %>%
            formatRound(8:9, digits=3) %>%
            formatRound(6:7, digits=5) %>%
            formatStyle(
              'in.aic',
              backgroundColor = styleEqual(min(resFit_tbl[,'in.aic']), 'silver')
            ) %>%
            formatStyle(
              'oos.aic',
              backgroundColor = styleEqual(min(resFit_tbl[,'oos.aic']), 'silver')
            ) %>%
            formatStyle(
              'in.bic',
              backgroundColor = styleEqual(min(resFit_tbl[,'in.bic']), 'silver')
            ) %>%
            formatStyle(
              'oos.bic',
              backgroundColor = styleEqual(min(resFit_tbl[,'oos.bic']), 'silver')
            ) %>%
            formatStyle(
              'in.mse',
              backgroundColor = styleEqual(min(resFit_tbl[,'in.mse']), 'silver')
            ) %>%
            formatStyle(
              'oos.mse',
              backgroundColor = styleEqual(min(resFit_tbl[,'oos.mse']), 'silver')
            ) %>%
            formatStyle(
              'in.r2',
              backgroundColor = styleEqual(max(resFit_tbl[,'in.r2']), 'silver')
            ) %>%
            formatStyle(
              'oos.r2',
              backgroundColor = styleEqual(max(resFit_tbl[,'oos.r2']), 'silver')
            ) %>%
            formatStyle(  # for replication row
              'replic',
              target = 'row',
              backgroundColor = styleEqual(1, 'rgb(173, 216, 230)'),  
              color = styleEqual(1, 'rgb(40, 96, 144)')
            )
    })
    
    # > OUTPUT: COEFF DT ====
    output$dt_coeffs <- DT::renderDataTable({
        req(input$dataGenButton!=0)
        input$solnButton # to force the update when the replication model's run

        # just subset to these two only
        dat <- data.frame(lambda=resFit[,"lambda"], non0Coeffs=resFit[,ncol(resFit)], stringsAsFactors=FALSE)
        
        # Table
        DT::datatable(dat, rownames=FALSE, colnames=c("lambda", "# non-zero coefficients"), 
                    options = list(
                        dom = 't' ,   
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                            "}"),
                        pageLength = nrow(resFit),
                        columnDefs = list(list(className = 'dt-center', targets = '_all'))
                    )
        )
    })

    # > OUTPUT: COEFFICIENT RESULTS TABLE ====
    output$coefRsltTable <- DT::renderDataTable({
        ## For code to update the selector box, see chunk under "MISC HELPER CODE"
        
        # Reactive for either soln button or reg estm
        estimate()
        
        # Get model ests
        mod <- all()[[1]][[1]]
        lambda <- all()[[1]][[3]]
        
        # Append current res table in memory to global object, if not there already
        if(is.null(coefRes[[paste0(lambda)]])){
            coefRes[[paste0(lambda)]] <<- mod  
        }
        
        # Retrieve result of interest
        res <- coefRes[[paste0(input$covarLamSel)]] %>% 
                as.matrix %>% data.frame %$% 
                cbind(names=row.names(.),.) 

        # Put intercept on top
        intc <- res %>% filter(names=="(Intercept)")
        notInt <- res %>% filter(!(names=="(Intercept)")) %>% 
                    plyr::arrange(desc(abs(.$s0))) 

        # Re-form
        res <- rbind(intc, notInt)
     
        # Clean up names
        res$names <- gsub("^X\\.", "#", res$names, perl=TRUE)
        res$names <- gsub("^X([0-9]*[^.]...)", "#\\1", res$names, perl=TRUE)  
        
        # Output
        DT::datatable(res, 
                      rownames=FALSE, colnames=c("Unigram", "Estimate"), 
                        options = list(
                            dom = 'l f t p' ,   
                            initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                "}"),
                            pageLength = 10,
                            columnDefs = list(
                                            list(className = 'dt-center', targets = '_all')
                                        )
                        )
                    ) %>%
            formatStyle(  # for replication row
              'names',
              target = 'row',
              backgroundColor = styleEqual("(Intercept)", 'rgb(186, 186, 186)')) %>% 
            formatRound(2, digits=2)
    })
    
    # # > OUTPUT: MAP ===
    # output$mapStates <- renderPlotly({
    # 
    #     # (Actual elec results are already in global data object)
    #     
    #     # Plot.
    #     elecRes %>%
    #         plot_ly(
    #             source = "stateMap",
    #             type = "choropleth",
    #             locations = ~stAbbr,
    #             locationmode = "USA-states",
    #             z = ~partypct,
    #             hoverinfo = "text",
    #             hoverlabel = list(font = list(family="PT Sans Narrow", size="15")),
    #             text = ~paste0(stName, ": ", scales::percent(partypct), "\nOutcome: ", 
    #                            ifelse(partypct>=.5, "Obama win", "Romney win")),
    #             colorscale=list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 0, 255)")), 
    #             zmin=0.4, 
    #             zmax=0.6 
    #         ) %>%
    # 
    #         layout(geo  = list(scope = "usa"),
    #                font = list(family="PT Sans Narrow")) %>%
    #         colorbar(title="Obama 2012 Vote\nShare, Actual")   
    # })
    # 
    # 
    # # > OUTPUT: HISTOGRAM of word freq on state/day ===
    # output$wordFreqs <- renderPlotly({
    #     # if there is no click data, render nothing!
    #     clickData <- event_data("plotly_click", source = "stateMap")
    #     if (is.null(clickData) | is.null(input$desc_date)) return(NULL)
    #     
    #     # Get clicked state
    #     state <- elecRes[clickData$pointNumber+1, "statev"]
    #     stateFull <- openintro::state2abbr(state) %>% openintro::abbr2state()
    #     
    #     # Get clicked date
    #     date <- input$desc_date
    #     dateFmt <- format(as.Date(date, format = '%Y-%m-%d'), '%m/%d/%Y')
    #     
    #     # Subset data
    #     ## Get all the rows for this state + date in both datasets (JIC,
    #     ## so you don't have to mindread where the stuff's located)
    #     in.state <- dataGen() %>%
    #                     filter(., statev==!!state) %>%
    #                     filter(., datev==!!date)
    #     oos.state <- polltwit2 %>%
    #                     filter(., statev==!!state) %>%
    #                     filter(., datev==!!date)
    #     info <- rbind(in.state, oos.state) %>% 
    #                 select(., -c(statev, datev, voteshareo, vsointerpol)) %>% 
    #                 distinct(.) 
    #     
    #     # Check if state has tweet data
    #     if(nrow(info)==0) {         
    #         # Display message in plotting area about no data for this state-date
    #         plot_ly() %>% 
    #             layout(xaxis= list(visible = FALSE),
    #                    yaxis= list(visible = FALSE),
    #                    annotations =list(
    #                        x="paper", 
    #                        y="paper", 
    #                        showarrow=FALSE,
    #                        text=paste0("No tweet data for ", stateFull, ", ", dateFmt),
    #                     ),
    #                    font = list(family="PT Sans Narrow")
    #             )    
    #     }
    #     
    #     info <- info %>%                                        # chuck the duplicate rows (which there maybe some of)
    #                 t(.) %>%                                    # put variables in row names for plotting purposes
    #                 data.frame(.) %>% 
    #                 rename(., perc = .) %>%                     # give the freq col a useful name, for your sake
    #                 arrange(., desc(perc)) %>%                  # sort from high to low
    #                 filter(., perc>0) %>%                       # toss rows with 0s
    #                 tibble::rownames_to_column(., var = "word") 
    #     info$word <- gsub("^X\\.", "#", info$word, perl=TRUE)   # reinserting hashtags
    #     info$word <- gsub("^X([0-9]+[[:alnum:]]*)", "#\\1", info$word, perl=TRUE)    
    #     
    #     # plot
    #     info %>%
    #         plot_ly(
    #             type = "bar",
    #             x = ~word,
    #             y = ~perc,
    #             hoverinfo = "text",
    #             hoverlabel = list(font = list(family="PT Sans Narrow", size="15")),
    #             text = ~paste0("<b>Word</b>: ", word, "\n(", scales::percent(perc), ")")
    #         ) %>% 
    #         layout(title = list(text=paste0("<b>Tweet Unigrams: ", stateFull, ", ", dateFmt, "</b><br>(Click and drag to zoom in; double-click to zoom out)")
    #                             ),
    #                xaxis = list(title = "", tickangle = -45, 
    #                             categoryorder = "array",
    #                             categoryarray = ~desc(perc)),
    #                yaxis = list(title = paste0("Use in ", openintro::state2abbr(stateFull),
    #                                            " on ", dateFmt, "<br>(% of top 10k words on state-day)"), tickformat = ".1%"),
    #                font = list(family="PT Sans Narrow"),
    #                margin = list(pad=1)
    #                
    #         )
    #     
    # })
    # # > OUTPUT: STATE SELECTION ===
    # output$selState <- renderUI({
    #     state <- "none"
    #     
    #     # if there is no click data, render nothing!
    #     clickData <- event_data("plotly_click", source = "stateMap")
    #     if (!is.null(clickData))  state <- elecRes[clickData$pointNumber+1, "statev"] %>% 
    #                                             openintro::state2abbr(.) %>% openintro::abbr2state(.)
    #     
    #     # Output
    #     HTML(
    #         paste0("<span style='font-size:1.25em; padding:5px; border-radius:7px;background-color:#eee;'>
    #                 <strong>Selected</strong>: ", state, 
    #                "</span>")
    #     )
    # })
    # 
    # # > OUTPUT: DATE SELECTION ===
    # output$selDate <- renderUI({
    #     # if there is no click data, render nothing!
    #     if (is.null(input$desc_date)) date <- "none"
    #     else                          date <- format(as.Date(input$desc_date, format = '%Y-%m-%d'), '%m/%d/%Y')
    #     
    #     HTML(paste0("<span style='font-size:1.25em; padding:5px; border-radius:7px;background-color:#eee;'>
    #                  <strong>Selected</strong>: ", date, 
    #                 "</span>")
    #     )
    # })

    
#***********************************************************************
# MISC HELPER CODE ----
#***********************************************************************     
    ## > MISC: update dropdown choices for lambda selector (covariate results) ====
    observeEvent(estimate(), {
        # Update
        updateSelectInput(session, "covarLamSel",
                          choices = c(resFit[, "lambda"]) %>% unname %>% as.numeric %>% sort %>% as.character,
                          selected= NULL # just select last added, for lack of better default
                         )  
    })

    
    # > MISC: enforce bin value rules ====
    ## specifically, that bin 1 <= bin2 <= bin3
    ## (do three sep obsv events, for now -> have to call everything repeatedly in case, in changing one of the sliders, the others' values go invalid)
    
    ## BIN 1
    observeEvent(input$bin1, {
        if(input$bin1>input$bin2){
            updateSliderInput(session, "bin1",              
                value=input$bin2
            )    
        }
    })
    
    ## BIN 2
    observeEvent(input$bin2, {
        if(input$bin1>input$bin2){
            updateSliderInput(session, "bin1",              
                value=input$bin2
            )    
        }
        
        if(input$bin2>input$bin3){
            updateSliderInput(session, "bin2",              
                value=input$bin3
            )    
        }
    })
    
    ## BIN 3
    observeEvent(input$bin3, {
        if(input$bin2>input$bin3){
            updateSliderInput(session, "bin2",              
                value=input$bin3
            )    
        }
        
        if(input$bin1>input$bin2){
            updateSliderInput(session, "bin1",              
                value=input$bin2
            )    
        }
    })
    
    # > MISC: the answer (+ disable sliders) ====
    observeEvent(input$solnButton, {
        hide("gphCapt")
        updateRadioGroupButtons(session, "alpha", selected = 1)
        
        updateSliderInput(session, "lambda", value=0.001,
                           min = 0, max = 0.022)
        
        updateSliderInput(session, "bin1", value=2,
                           min = 1, max = 10)
        updateSliderInput(session, "bin2", value=3,
                           min = 1, max = 10)
        updateSliderInput(session, "bin3", value=4,
                           min = 1, max = 10)
  
        disable("alpha")
        disable("lambda")
        disable("bin1")
        disable("bin2")
        disable("bin3")
        disable("dataGenButton")
        
        showElement("DT_solnLegend")
    })
    
    # > MISC: loading screen reactive ====
    output$setupComplete <- reactive({
        return(rv$setupComplete)
    })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
    
    # > MISC: enable replication button after you've estimated at least one thing ====
    observeEvent(input$dataGenButton, {
        enable("solnButton")
    })
    
    # Housekeeping
    session$onSessionEnded(stopApp)  
}    