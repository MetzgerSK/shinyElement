# To automate the building of the more repetitive tabs, to save time.

#************************************************
## >> MAIN TAB: COPY BUTTON CHUNK ----
#*********************
copyBtn <- function(stub) {
    expr <- paste0('
        div(class="inline-block-center",
            div(
                actionButton("copyButton_', stub, '", "No Violation Scenario", icon("copy")), 
                bsTooltip("copyButton_', stub, '", noViolTooltip,
                          placement="bottom", trigger="hover")
            )
        )
    ')
    
    eval(parse(text=expr))
}
        

#************************************************
## >> MAIN TAB: RESULT CHUNK----
#*********************
mainTabChunkBuild <- function(stub){
    # Get estm choices
    ifelse(stub=="end", {choices <- "estmChoices_endog"}, {choices <- "estmChoices"})
    
    # Get table key
    ifelse(stub=="ac", {tKCh<- "tableKeyAC"}, {tKCh <- "tableKey"})
    
    # Get non-present suffix for equation
    ifelse(stub=="reg", {eqSt <- ""}, {eqSt <- paste0("_", stub)})
    
    # Build
    expr <- paste0('
        fluidRow(
            h4("True DGP:", align = "left"),
            uiOutput("equation', eqSt, '"),
            br(),br(),
            h4("Simulation Results"),
            simulateBriefInstrs,
            br(),br(),
            shinyjs::hidden(radioGroupButtons(
               inputId = "estSel_', stub, '",
               label = "Select Estimate to View", 
               choices = ', choices, ',
               selected = "b1",
               status = "primary"
            )),
            tableOutput("data_', stub, '") %>% withSpinner(., type = 6, color = "#33b5e5"),
            shinyjs::hidden(
                div(id="tabKey_', stub, '", ', tKCh, ')
            ),
            br(),
            shinyjs::hidden(
                downloadButton("datDwn_', stub, '", "Download a Fake Dataset", icon("file-download"),
                          style="color:#EEE; background-color:#718C6A;"
                ),
                bsTooltip("datDwn_', stub, '", "Save an example dataset from one draw of these simulations (CSV)",
                      placement="bottom", trigger="hover")
            )
        )            
    ')
    
    eval(parse(text=expr))
    
}

               
#************************************************
## >> RAW SIM OUTPUT TAB ----
#*********************
rawSimBuild <- function(stub){
    
    # Get model labels
    ifelse(stub=="ac", {modL <- "modListAC"}, {modL <- "modList"})
    
    expr <- paste0('
        mainPanel(
            h4(class="simFyiHdr_', stub, '", "NOTE: must click \'Simulate!\' on \'Main\' tab first.", align = "left"),
            div(class="inline-block-center rawRes",
                div(
                    shinyjs::hidden(
                        downloadButton("rawDwn_', stub, '", "Download Results", icon("file-download"),
                                  style="color:#EEE; background-color:#4d8e88;"
                        ),
                        bsTooltip("rawDwn_', stub, '", "Save the raw simulation results from the table below (CSV)",
                              placement="right", trigger="hover")
                    )
                )
            ),
            shinyjs::hidden(div(id="rawResTblDisp_', stub, '",
                selectInput("modRaw_', stub, '", "Model",',
                        modL, ', selected=1),
                DT::dataTableOutput("table_', stub, '")
            ))
        )
    ')
    
    eval(parse(text=expr))
}


#************************************************
## >> ESTIMATE DISTRO TAB ----
#*********************
estDistBuild <- function(stub){
    
    # Get estm choices
    ifelse(stub=="end", {choices <- "estmChoices_endog"}, {choices <- "estmChoices"})
    
    # Deal with true/reg stub diffs
    ifelse(stub=="reg", {stub2 <- "true"}, {stub2 <- stub})
    
    expr <- paste0('
        mainPanel(
            h4(class="simFyiHdr_', stub, '", "NOTE: must click \'Simulate!\' on \'Main\' tab first.", align = "left"),
            hidden(div(id="distGphDisp_', stub, '",
                fluidRow(
                    column(12, align="center",
                        radioGroupButtons(
                           inputId = "paramG_', stub2, '",
                           label = "Select Estimate to View", 
                           choices = ', choices, ',
                           selected = "b1",
                           status = "primary"
                        )
                    )
                ),
                fluidRow(
                    column(12, align="center",
                       
                        plotOutput("distPlot_', stub2, '", height="600px", width="105%") %>%
                            addSpinner(., spin = "folding-cube", color = "#33b5e5")
                        
                    )
                )
            ))
        )
    ')
    
    eval(parse(text=expr))
    
}


#************************************************
## >> EFFICIENCY CHECK TAB ----
#*********************
effChkBuild <- function(stub){
    # Get modList stub
    ifelse(stub=="ac", {modL <-"modListAC"}, {modL <- "modList"})
    
    # If this is an endog instance (other than meas err y) OR nlin, 
    # flash the bias warning
    if(stub %in% c("end", "ar", "endSim", "endMeasErrX", "nlin")){
        biasWrn <- ' 
                bsCallout("<i class=\'fas fa-exclamation-triangle\'></i> Biased Estimates (OLS)",
                          "Interpret efficiency checks with caution.",
                          "danger"
                )'
    } else  biasWrn <- "br()"
    
    expr <- paste0('
        fluidRow(
            h4(style="padding-left:30px;", class="simFyiHdr_', stub, '", 
                "NOTE: must click \'Simulate!\' on \'Main\' tab first.", align = "left"),
            shinyjs::hidden(
                div(id="effInfo_', stub, '", style="margin-top:10px;",
                    HTML("<div class=\'inline-block-center\'><div>"), ',
                    biasWrn, ',
                    HTML("</div>
                    <div style=\'width:85%;\'> <p> Efficiency\'s textbook
                    definition for multivariate regression involves the
                    (theoretical) variance-covariance matrix (VCE) of two
                    estimators. Informally, an unbiased estimator is more
                    efficient than another unbiased estimator if its variances
                    are smaller.  The key is how we conceptualize
                    \\"smaller.\\"</p>
                    
                    <p>There are five mathematically equivalent
                    ways of defining \\"smaller\\" for a multivariate estimator 
                    (Kennedy 2003, 37-39).  Checking the <em>trace</em> of the
                    estimator\'s VCE is one of them, and the one
                    <code>linRegEstms</code> opts to use. Formally, then,
                    Estimator A is said to be <em>more efficient</em> than
                    estimator B if (a) both Estimator A and B are unbiased for
                    all their parameter estimates and (b) the trace of A\'s
                    VCE matrix is smaller than the trace of B\'s.
                    
                    <p>The unbiased estimator whose VCE has the smallest trace
                    compared to all other linear estimators is said to be
                    \\"BLUE\\" (best linear unbiased estimator).  More
                    broadly, an unbiased estimator whose VCE has the smallest
                    trace compared to <em>any</em> other unbiased estimator is
                    said to be \\"BUE\\" (best unbiased estimator).</p>

                    <p>While we can\'t check every possible estimator here in
                    the app to speak to \\"best,\\" we <em>can</em> compare
                    the variance-covariance (VCE) matrices for two estimators
                    for illustrative purposes.  You can either view
                    the <em>estimated</em> VCE matrix, based on a model\'s SE
                    estimates,<a id=\'fn1_ret\'></a><sup><a href=\'#fn1\'>[1]</a></sup> or the
                    <em>\\"theoretical\\"</em> VCE matrix, based on the
                    variances and covariances of the simulated coefficients
                    (in quotes because it\'s not truly the theoretical VCE,
                    but our simulated approximation of it).  The theoretical
                    VCE is what gets used in all the official BLUE/BUE
                    proofs.</p></div>"),
                    br(),                   
                    radioGroupButtons(
                       inputId = "vceSel_', stub, '",
                       label = "VCE Type", 
                       choices = c(\' \\"Theoretical\\" \'=2, \'Estimated\'=1),
                       selected = 2,
                       status = "primary"
                    ),
                    HTML("</div>"),
                    
                    fluidRow(
                        column(6,
                            
                            selectInput("iMat_', stub, '", "Model A", ',
                                        modL,'[-c(which(', modL, '==4), which(', modL, '==6))], selected=1),  # since selected is 4 for j
                            verbatimTextOutput("matI_', stub, '")
                        ),
                        column(6,
                            selectInput("jMat_', stub, '", "Model B", ',
                                        modL,'[-c(which(', modL, '==1), which(', modL, '==6))], selected=4),  # since selected is 1 for i
                            verbatimTextOutput("matJ_', stub, '")
                        )
                    ),
                    fluidRow(
                        column(6,
                            h4("Matrix Traces"),
                            verbatimTextOutput("matDiff_', stub, '")
                        ),
                        column(6,
                            h4(style=\'color:#F39C12;\', "Implies:"),
                            verbatimTextOutput("matImpl_', stub, '")
                        )
                    ),
                    hr(),
                    HTML("<p style=font-size:80%;><a id=\'fn1\'></a>1: For the estimated
                    VCEs, a model\'s displayed VCE matrix is the average of its estimated VCEs
                    across all the simulations. <a href=\'#fn1_ret\'>&larrhk;</a></p>")
                )
            )
        )
        
    ')
    
    eval(parse(text=expr))
}
