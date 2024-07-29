ui <- fluidPage(
    
    # To force the MathJax to wrap (correctly, this time)
    tags$head(tags$script(type = "text/x-mathjax-config", 
                            'MathJax.Hub.Config({
            "HTML-CSS": { linebreaks: { automatic: true } },
             SVG: { linebreaks: { automatic: true } }
            });'),
        # to deal with the JS crash while testing
        HTML(
          "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
        )
    ),

    # Custom CSS
    tags$style(HTML(
       "@import url('//fonts.googleapis.com/css?family=PT+Sans+Narrow:400,400italic,700,700italic');",  # Google Font: PT Sans Narrow
       "@import url('//fonts.googleapis.com/css?family=PT+Sans:400,400italic,700,700italic');",         # Google Font: PT Sans
       "@import url('//fonts.googleapis.com/css?family=Sanchez:400italic,400')",                        # Google Fonts: Sanchez (for blockquote)    
       "@import url('//fonts.googleapis.com/css?family=Roboto:100');"                                   # Google Fonts: Roboto (for blockquote)   
        )               
    ),
    
    # Main sheet's CSS
    includeCSS("style.css"),


# SOURCE THE MODALS ----
## (both the prediction modal + model used modal)
source("uiPt_bsModals.R", local=TRUE)$value,    
    
# > LOADING SCREEN ----
conditionalPanel(condition = "!output.setupComplete",
    div(id = "loadScreen", style = "text-align: center;",
        h2("Loading data, please wait.", style="font-family: 'PT Sans';"),
        br(),
        quoteBox("RQ",
                 "[T]his article combines 1,200 state-level polls during the 2012 presidential 
                    campaign with over 100 million state-located political tweets [and] models 
                    the polls as a function of the Twitter text...",
                 "Beauchamp (2017), p. 490"
        )
    ) %>% withSpinner(type=6)
),

# > ACTUAL APP ----
## (once loaded)
conditionalPanel(condition = "output.setupComplete",

titlePanel(HTML("LASSO: Beauchamp 2017 <em>(Lite Version)</em>")),   
    # -- Any "enable this thing" functions --
    useShinyjs(),
    withMathJax(),
    
    # Start tab panel
    tabsetPanel(     
        tabPanel("Main", value="main_true",
            sidebarLayout(
                # Left sidebar ----
                sidebarPanel(
                    h3("Options", style="margin-top:0px;"),

                    # Easier to hide than to comment out and then root out all the references in server
                    shinyjs::hidden(radioGroupButtons(
                        inputId = "alpha",
                        label = "Model Type", 
                        choiceNames = c("Ridge Reg.", "LASSO"),
                        choiceValues = c(0, 1),
                        status = "primary",
                        selected = 1,
                        checkIcon = list(
                            yes = tags$i(class = "fa fa-check-square", 
                                         style = "color: lightblue"),
                            no  = tags$i(class = "fa fa-square-o", 
                                         style = "color: lightblue")  
                        )
                    )),
                    sliderInput("lambda", label = "Penalty Coefficient \\(\\left(\\lambda \\right) \\)", 
                                min = 0, max = 0.021, step = 0.001, value = 0.02),  
                    conditionalPanel("input.lambda == 0", 
                        p("\\( \\Rightarrow \\lambda = 0 \\) is equivalent to classic OLS",
                           style="font-size: 75%; font-style: italic;")
                    ),
                    br(),

                    # Easier just to leave it here, and then never unhide, than to root out all the references in the server.
                    hidden(div(id="advDiv",
                        p("Beauchamp smoothes his poll data using \"the average of the 2-8 nearest polls\" (2017, 493).  
                          Here, you can vary the size of the window, if desired.",
                          style="font-size: 85%;"),
                        sliderInput("bin1", label = "State w/Few Polls \\( \\left( < 20  \\text{ in entire temporal range}\\right) \\)",
                                min = 1, max = 8, step = 1, value = 2),
                        sliderInput("bin2", label = "State w/Moderate # of Polls \\( \\left( 20 \\leq \\text{# polls} < 40 \\right) \\)",
                                min = 1, max = 8, step = 1, value = 3),
                        sliderInput("bin3", label = "State w/Many Polls \\( \\left( \\geq 40 \\right) \\)",
                                min = 1, max = 8, step = 1, value = 4),
                        p(" \\( \\text{Bin 1's value} \\leq \\text{Bin 2's value} \\leq \\text{Bin 3's value} \\)"),
                        p("Sliders will auto-adjust to meet these constraints.",
                          style="font-size: 75%; font-style: italic;"),
                        
                        style="border-radius: 8px; padding: 2px 15px 1px 15px; background-color:#dcdcdc; p{ font-size: 75%; }"
                    )),
                    
                    actionButton("dataGenButton", "Estimate", icon("table"), 
                                 style="margin-top: 2px;"),
                    br(),
                    disabled(actionButton("solnButton", "Show Replication", icon("flask"),
                                          style="color: #ffffff; background-color: #337ab7;")
                    ),
                    br(),br(),
                    HTML("<p style='text-align:center; font-size: 75%; font-style: italic;'>For various technical details about the dataset, model specification, and sample splits, see Beauchamp\'s 
                         <a href='https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/RJAUNW/DGQ0TR&version=1.1'>codebook</a>.  The app uses interpolated poll values.</p>")
                ),

                # Main panel ----
                mainPanel(
                    h4("Two-Predictor Proposed Line's Loss Function"),
                    uiOutput("eq_lasso_gen"),
                    bsTooltip("eq_lasso_gen",
                              HTML("For a model with (arbitrarily) two covariates only, to show what the loss function would look like before we generalize the expression to the <em>k</em>-predictor case."),
                              trigger="hover", placement="left"),
                    
                    h4("\\(k\\)-Predictor Proposed Line's Loss Function", class="topMarg25"),
                    uiOutput("eq_lasso_spec"),
                    p("where \\(K\\) is the number of predictors, \\(\\mathbf{X}\\) is a \\(n \\times K\\) matrix containing all 
                      \\( \\sim\\!\\!10000 (= K)\\) of Beauchamp's possible predictors, and \\(B\\) is the \\(K \\times 1\\) vector 
                      containing each predictor's estimated coefficient \\( \\left( \\beta_k \\right) \\).",
                      style="margin-left: 10px;"),

                    hidden(h4(id="fitHdr", "Estimated Model's Fit Statistics", class="topMarg25")),
                    uiOutput("fit.stats"),
                    hidden(div(id="fitFtr",
                             HTML("<p>* calculated using predicted Obama vote share vs. actual Obama vote share
                                  (<a id='benchLink' href='' style='color:#960000;'>Why is this relevant?</a>)</p>"),
                             style="font-size: 75%; font-style: italic;")),
                    
                
                    plotlyOutput("predVsReal") %>% withSpinner(), 
                    hidden(div(id = "gphCapt",
                            HTML("data pt. colors/shapes: predicted winner <br> shaded areas: actual winner"),
                            style = "text-align: right; font-family: 'PT Sans Narrow'; font-size: 85%;"
                          )
                    ),

                    br(),
                    div(id="inside_explain",
                            actionButton("explainButton", "Is this the model Beauchamp actually uses?", icon("question-circle"))
                        ),
                        
                    br()
                )    
            ),
            # Footer ----
            fluidRow(
                column(4, " "),
                column(8,
                    div(id="footer", style="text-align:left;",
                        HTML(paste0(strong("Data Source"), ": Beauchamp, Nicholas.  2017. ",  
                                    a(href="https://doi.org/10.1111/ajps.12274", 
                                      '\"Predicting and Interpolating State-Level Polls Using Twitter Textual Data.\" ' ), 
                                    em("American Journal of Political Science "), " 61 (2): 490&ndash;503."
                        )))
                )
            )
        ),
        tabPanel("Fit Stats", value="data_fit",
            h4(class="simFyiHdr", "NOTE: Must estimate at least one model on Main tab."),
            br(),
            conditionalPanel('input.dataGenButton != 0',
                             
                div(class="inline-block-center", div(
                    radioGroupButtons(
                        inputId = "gofSel",
                        label = "Choose Format",
                        choices = c("Table"=1, "Graph"=2),
                        selected = 1,
                        status = "primary"
                    )
                )),

                conditionalPanel("input.gofSel == 1", 
                    HTML('<p><span style="background-color:#C0C0C0">shaded</span> cells = best current value (lowest = best for all except \\( R^2 \\), where highest = best)</p>'),

                    hidden(
                        div(id="DT_solnLegend",
                            HTML('<p><span style="background-color:rgb(173, 216, 230); color:rgb(40, 96, 144);">shaded</span> row = replication</p>')
                        )
                    ),
                    DT::dataTableOutput("dt_fit"),
                    HTML("<p>* <code>oos.mse</code> is synonymous with the mean squared prediction error (MSPE).</p>")
                ),
                conditionalPanel("input.gofSel == 2",
                    plotlyOutput("lamFitPlot") %>% withSpinner()
                )
            )
        ),
        tabPanel("# Covariates", value="data_coeff",
            h4(class="simFyiHdr", "NOTE: Must estimate at least one model on Main tab."),
            br(),
            conditionalPanel('input.dataGenButton != 0',
                             
                div(class="inline-block-center", div(
                    radioGroupButtons(
                        inputId = "coeffSel",
                        label = "Choose Format",
                        choices = c("Table"=1, "Graph"=2),
                        selected = 1,
                        status = "primary"
                    )
                )),

                conditionalPanel("input.coeffSel == 1",
                    div(style="text-align: center;",
                        div(DT::dataTableOutput("dt_coeffs") , style="width: 45%; display:inline-block;")
                    )
                ),
                conditionalPanel("input.coeffSel == 2",
                    plotlyOutput("lamCoeffPlot") %>% withSpinner()
                )
            )
        ),
        tabPanel("Coefficient Estimates",
            h4(class="simFyiHdr", "NOTE: Must estimate at least one model on Main tab."),
            conditionalPanel('input.dataGenButton != 0',
                h4("Coefficient Metric"),
                p("Coefficients reported in original metric."), 
                             
                h4("Covariate Coding Rules", style="margin-top:20px;"),
                HTML("<blockquote style='margin-bottom:0px;'>
                            To extract the textual features of the tweets, the top 10,000 unigrams (including
                            hashtags, URLs, etc.) were retained, and for each state&ndash;day unit, the percentage of that unigram in that unit was 
                            calculated (e.g., 0.02 for \"obama\" would mean that <span style='color:#b22222; font-weight:bold;'>2% of all words used in that day in that state</span> were \"obama,\" 
                            at least from among the top 10,000).
                            <footer style=\"font-style:italic;font-family: \"PT Sans Narrow\";\">Beauchamp (2017), p. 493, emph. added</footer>
                      </blockquote>"
                ),
                h4("Standard Errors?", style="margin-top:20px;"),
                p("There are no standard errors in the table below because SEs are not available from LASSO. Were we interested in obtaining them, 
                we would need to run bootstraps that appropriately acknowledge the dependencies in Beauchamp's data."),
                
                # B/c the current quick div trick will mess up the select box contents, just be inelegant with columns.
                fluidRow(
                    column(4, ""),
                    column(4,
                        selectInput("covarLamSel", "Choose \\(\\lambda\\):", 
                                    choices = c(0.001, 0.02), 
                                    selected= 0.02)
                    ),
                    column(4, "")
                ),
                DT::dataTableOutput("coefRsltTable") %>% withSpinner()
            )
                 
        )#,
      # --- PRESENT IN FULL APP VERSION ONLY (SEE CODE OCEAN CAPSULE) --- 
#         tabPanel("Data: Tweet Info",
#             
#             br(),
#             div(class="inline-block-center", 
#                 style="/*border-top: 1px solid #bbb;
#                        border-bottom: 1px solid #bbb;*/
#                        background-color: #eee;",
#                 
#                 div(
#                     HTML("<h4>Choose state and date to display state&ndash;date tweet data</h4>")
#                 )
#             ),
#             br(),
#             
#             # Have users click map, then click day -> show histogram with unigrams, sorted H to L.
#             fluidRow(
#                 column(7, plotlyOutput("mapStates")),
#                 column(4, 
#                     # manually do the div-in-div trick, to prevent messing up DoW alignment
#                     div(style="text-align: center;",
#                         div(style="display: inline-block;
# 	                               text-align: left;",
#                             airDatepickerInput(
#                                 inputId = "desc_date",
#                                 label = NULL, 
#                                 value = NULL, 
#                                 startView = "2012-09-02",# should be 09-01, but everything's displaying as one day earlier b/c of TZ issues
#                                 minDate = "2012-09-02",  # ^ see above note
#                                 maxDate = "2012-11-07",  # should be 11-06, but see above note
#                                 inline = TRUE
#                             )
#                         )
#                     )
#                 ),
#                 column(1, "")
#             ),
#             fluidRow(
#                 column(7,
#                     div(class="inline-block-center", 
#                         style="margin-top:10px;",
#                         
#                         div(
#                             uiOutput("selState")
#                         )
#                     )
#                 ),
#                 column(4,                     
#                     div(class="inline-block-center", 
#                         style="margin-top:10px;",
#                         
#                         div(
#                             uiOutput("selDate")
#                         )
#                     )
#                 ),
#                 column(1, "")
#             ),
#             hr(),
#             
#             plotlyOutput("wordFreqs") %>% withSpinner()
# 
#         )
        
        
    ),
    fluidRow(align="center", 
        br(), 
        HTML(paste0(strong("Author: "), a(href="http://www.shawnakmetzger.com", "Shawna K. Metzger"), ", ",
                  a(href="mailto:shawna@shawnakmetzger.com", "shawna@shawnakmetzger.com"))), br(), 
        HTML("<em>Using Shiny to Teach Econometric Models</em>, Cambridge University Press")
    )  
)

)
