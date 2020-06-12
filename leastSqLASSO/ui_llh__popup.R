# Spinner settings
spinColor <- "#325D88"
spinType <- 7

jqui_draggable(
    ui=bsModal("modal_fullSSR", "The full penalized sum of squares calculation", "fullSSRButton", size = "large",
                                            
        div(class="bs-callout bs-callout-warning", style="margin-top:5px;",
            div(class="bs-close", icon("times-circle")),
            HTML("<h4><i class='fas fa-exclamation-circle'></i> NOTE</h4>"),
            HTML(
                "It may take a moment for all the equations to render properly, 
                depending on how many observations your dataset has.  All the lines 
                will wrap, once the rendering is complete.")
        ),
    
        h4("Generic: loss function"),
        uiOutput("eq_fullSSR") %>% withSpinner(., type = spinType, color = spinColor),
        
        hr(),
        
        h4("Specific: loss function"),
        p("(For this specific dataset and your proposed parameter values.)"),
        uiOutput("eq_fullSSR_spec") %>% withSpinner(., type = spinType, color = spinColor)
        
    )
)