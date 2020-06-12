#************************************************
## >> SUBPAGE BUILDER FUNCTS ----
#*********************
source("uiPt__tabBuildFuncts.R", local=TRUE)$value


#************************************************
## >> START OF DISPLAYED UI ----
#*********************
ui <- fluidPage( theme=shinytheme("darkly"),

    # Loading up the CSS
    includeCSS("style.css"),
    
    # To force the MathJax to wrap
    tags$head(
        tags$script(type = "text/x-mathjax-config", 
                            'MathJax.Hub.Config({
        "HTML-CSS": { linebreaks: { automatic: true, width: "container" } },
               SVG: { linebreaks: { automatic: true } }
            });')
    ),

    # -- Any "enable this thing" functions --
    useShinyjs(),   # From shinyjs package, to gray the input widgets out until button's clicked
    withMathJax(),  # Can appear anywhere in UI. I usually put it as one of the first lines in fluidPage(), to make it easy to spot later on

    # Begin UI    
    navbarPage("Scenarios", id = "mainNav",                   
        
        # Menu item 1
        tabPanel("No Violations", value="true",    
            source("uiPt_1_noViol.R", local=TRUE)$value
        ) ,

        # Menu item 2
        navbarMenu("Endogeneity",                
            tabPanel("\\( E \\left( u \\right) \\neq 0 \\)", value="ar",
                source("uiPt_2_endog_Eu0.R", local=TRUE)$value
            ) ,
            tabPanel("Omitted Var", value = "ov",
                source("uiPt_2_endog_OV.R", local=TRUE)$value
            ) ,
            tabPanel("Simultaneity", value = "simult",
                source("uiPt_2_endog_simult.R", local=TRUE)$value
            ),
            tabPanel("Meas. Err: \\( x \\)", value = "measErrX",
                source("uiPt_2_endog_measErrX.R", local=TRUE)$value
            ),
            tabPanel("Meas. Err: \\( y \\)", value = "measErrY",
                source("uiPt_2_endog_measErrY.R", local=TRUE)$value
            )
        ),

        # Menu item 3       
        navbarMenu("Errors",                         
            tabPanel("Heteroscedasticity", value="hetero",
                source("uiPt_3_err_het.R", local=TRUE)$value
            ),
            tabPanel("Autocorrelation",
                source("uiPt_3_err_ac.R", local=TRUE)$value
            )
        ),

        # Menu item 4
        tabPanel("Non-Linear in Parameters", value="nlin",        
            source("uiPt_4_nlin.R", local=TRUE)$value
        ),

        # Menu item 5
        navbarMenu("Normality",
            tabPanel("Heavy-Tailed Errors", value="heavy",
                source("uiPt_5_norm_heavy.R", local=TRUE)$value
            ),
            tabPanel("Skewed Errors",
                source("uiPt_5_norm_skewed.R", local=TRUE)$value
            ),
            tabPanel("Multi-Modal Errors",
                source("uiPt_5_norm_mmodal.R", local=TRUE)$value
            )
        ),
               
       # Footer info
       footer = column(12, align="center", br(), 
                       HTML(paste0(strong("Author: "), a(href="http://www.shawnakmetzger.com", "Shawna K. Metzger"), ", ",
                                  a(href="mailto:shawna@shawnakmetzger.com", "shawna@shawnakmetzger.com"))), br(), 
                       HTML("<em>Using Shiny to Teach Econometric Models</em>, Cambridge University Press")) 
    )
)
