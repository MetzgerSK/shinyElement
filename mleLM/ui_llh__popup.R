# Spinner settings
spinColor <- "#325D88"
spinType <- 7

jqui_draggable(
    bsModal("modal_fullLLH", "The full log-likelihood calculation", "fullLLHButton", size = "large",
        div(class="bs-callout bs-callout-warning", style="margin-top:5px;",
            div(class="bs-close", icon("times-circle")),
            HTML("<h4><i class='fas fa-exclamation-circle'></i> NOTE</h4>"),
            HTML(
                "It may take a moment for all the equations to render properly, 
                depending on how many observations your dataset has.  All the lines 
                will wrap, once the rendering is complete.")
        ),
    
        h4("Generic: the likelihood expression"),
        uiOutput("eq_fullLH") %>% withSpinner(., type = spinType, color = spinColor),
        
        h4("Generic: the log-likelihood expression"),
        uiOutput("eq_fullLLH") %>% withSpinner(., type = spinType, color = spinColor),
        
        hr(),
        
        h4("Specific: log-likelihood expression, simplified"),
        HTML("<a id='simp'></a>
              <p>
            (For this specific dataset and your proposed parameter values, after 
            simplifying the expression.  See <a href='#unsimp'>next section</a> for unsimplified.)
             </p>"),
        uiOutput("eq_fullLLH_all") %>% withSpinner(., type = spinType, color = spinColor),
        
        br(),
        
        h4("Specific: log-likelihood expression, unsimplified"),
        HTML("<a id='unsimp'></a>
              <p>
            (For this specific dataset and your proposed parameter values, WITHOUT 
            simplifying the expression.  See <a href='#simp'>previous section</a> for simplified.)
             </p>"),
        uiOutput("eq_fullLLH_all_unsimp") %>% withSpinner(., type = spinType, color = spinColor)
        
    )
)