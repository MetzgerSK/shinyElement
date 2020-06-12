# NO VIOLATIONS ====
output$equation <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat, 
            ifelse(input$b1Hat>=0, "+", ""), input$b1Hat, 'x', 
            ifelse(input$b2Hat>=0, "+", ""), input$b2Hat, 'z', 
            '+', input$noise, '\ u\\)'
        )
    )
})

# OV BIAS ====
output$equation_end <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_end, 
            ifelse(input$b1Hat_end>=0, "+", ""), input$b1Hat_end, 'x', 
            ifelse(input$b2Hat_end>=0, "+", ""), input$b2Hat_end, 'z', 
            '+', input$noise_end, '\ u\\)'
        )
    )
})

# E(u)!=0 ====
output$equation_ar <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_ar, 
            ifelse(input$b1Hat_ar>=0, "+", ""), input$b1Hat_ar, 'x', 
            ifelse(input$b2Hat_ar>=0, "+", ""), input$b2Hat_ar, 'z', 
            '+ \\left(', input$noise_ar,'u +', input$meanU, ' \\right)\\)'
        )
    )
})

# SIMULT ====
output$equation_endSim <- renderUI({
    
    # To cut down on repetitiveness later
    linCombo1 <- paste0(
                        input$aHat_endSim, 
                        ifelse(input$b1Hat_endSim>=0, "+", ""), input$b1Hat_endSim, 'x', 
                        ifelse(input$b2Hat_endSim>=0, "+", ""), input$b2Hat_endSim, 'z', 
                        '+ ', input$noise_endSim,'u_{y} '
                    )
        
    linCombo2 <- paste0(
                        input$aHatX_endSim, 
                        ifelse(input$alpha2_endSim>=0, "+", ""), input$alpha2_endSim, 'y', 
                        ifelse(input$b2HatX_endSim>=0, "+", ""), input$b2HatX_endSim, 'z',
                        ifelse(input$xInstr_endSim>=0, "+", ""), input$xInstr_endSim, 'z_2',
                        '+ ', input$noiseX_endSim,'u_{x}'
                    )
    
    
    gamma <- paste0( ifelse(input$b1Hat_endSim>=0, "+", ""), input$b1Hat_endSim,
                    ifelse(input$alpha2_endSim>=0, "+", ""), input$alpha2_endSim,
                    "- \\left[", ifelse(input$b1Hat_endSim>=0, "+", ""), input$b1Hat_endSim, "*",
                                 ifelse(input$alpha2_endSim>=0, "+", ""), input$alpha2_endSim,"\\right] \\right)^{-1} "
        )
        
    
    withMathJax(
        ## STRUCTURAL EQs
        paste("
            \\( \\begin{align} y &= ", linCombo1, " \\\\
                               x &= ", linCombo2, " \\\\
             \\end{align}
            \\)
        ")
    )
})

# MEAS ERR: X ====
## true
output$equation_endMeasErrX <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_endMeasErrX, 
            ifelse(input$b1Hat_endMeasErrX>=0, "+", ""), input$b1Hat_endMeasErrX, 'x',
            ifelse(input$b2Hat_endMeasErrX>=0, "+", ""), input$b2Hat_endMeasErrX, 'z', 
            '+ ', input$noise_endMeasErrX,'u \\)'
        )
    )
})

## estimated
output$equationEst_endMeasErrX <- renderUI({
    withMathJax(
        paste(
            '\\(y = \\hat{\\alpha} + \\hat{\\beta}_1 \\left (x + ', input$noiseX_endMeasErrX, 'v  \\right) + \\hat{\\sigma} u
            \\text{, where } v \\sim \\mathcal{N}(0,1) \\)'
        )
    )
})


# MEAS ERR: Y ====
## true
output$equation_endMeasErrY <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_endMeasErrY, 
            ifelse(input$b1Hat_endMeasErrY>=0, "+", ""), input$b1Hat_endMeasErrY, 'x',
            ifelse(input$b2Hat_endMeasErrY>=0, "+", ""), input$b2Hat_endMeasErrY, 'z', 
            '+ ', input$noise_endMeasErrY,'u \\)'
        )
    )
})

## estimated
output$equationEst_endMeasErrY <- renderUI({
    withMathJax(
        paste(
            '\\(\\left(y ', ifelse(input$bonusNoise_endMeasErrY>=0, "+", ""), input$bonusNoise_endMeasErrY, 'v  \\right) 
                        = \\hat{\\alpha} + \\hat{\\beta}_1 x + \\hat{\\sigma} u
                        \\text{, where } v \\sim \\mathcal{N}(0,1) \\)'
        )
    )
})            

# HETERO ====
output$equation_hetero <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_hetero, 
            ifelse(input$b1Hat_hetero>=0, "+", ""), input$b1Hat_hetero, 'x', 
            ifelse(input$b2Hat_hetero>=0, "+", ""), input$b2Hat_hetero, 'z', 
            '+', input$noise_hetero, '\ ux\\)'
        )
    )
})

# AUTOCORRELATION ====
output$equation_ac <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_ac, 
            ifelse(input$b1Hat_ac>=0, "+", ""), input$b1Hat_ac, 'x', 
            ifelse(input$b2Hat_ac>=0, "+", ""), input$b2Hat_ac, 'z', 
            '+', input$noise_ac, '\ u_{AC} \\text{,} \\\\ ~~~~~ \\text{in which } u_{AC} = ', 
            ifelse(input$acChoice=="ar", "AR", "MA"), '(1) \\text{ process w/value of }', input$arVal, '\\text{.}\\)'
        )
    )
})

# NONLINEAR ====
output$equation_nlin <- renderUI({
    withMathJax(
        paste(
            '\\(y = \\exp \\left(', input$aHat_nlin, 
            ifelse(input$b1Hat_nlin>=0, "+", ""), input$b1Hat_nlin, 'x', 
            ifelse(input$b2Hat_nlin>=0, "+", ""), input$b2Hat_nlin, 'z', 
            '+', input$noise_nlin, '\ u\\right)\\)'
        )
    )
})

# NON-NORMAL: HEAVY-TAILED ====
output$equation_heavy <- renderUI({
    withMathJax(
        paste(
            '\\(y = ', input$aHat_heavy, 
            ifelse(input$b1Hat_heavy>=0, "+", ""), input$b1Hat_heavy, 'x', 
            ifelse(input$b2Hat_heavy>=0, "+", ""), input$b2Hat_heavy, 'z', 
            '+', input$noise_heavy, ' u \\text{, where } u \\sim t_{', input$tDFVal, '}(0,1) \\)'
        )
    )
})

# NON-NORMAL: SKEWED ====
output$equation_skewed <- renderUI({
    
    errDistro <- ifelse(input$distChoice=="chi2",
                        paste0('\\chi^2_{', input$snShape,'}'),
                        paste0('\\mathcal{N}_{\\text{skew}}(0,1,', input$snShape,')')
                 )
    withMathJax(
        paste(
            '\\(y = ', input$aHat_skewed, 
            ifelse(input$b1Hat_skewed>=0, "+", ""), input$b1Hat_skewed, 'x', 
            ifelse(input$b2Hat_skewed>=0, "+", ""), input$b2Hat_skewed, 'z', 
            '+', input$noise_skewed, ' u \\text{, where } u \\sim ', errDistro, ' \\)'
        )
    )
})

# NON-NORMAL: MULTI-MODAL ====
output$equation_mmodal <- renderUI({
    
    rounded <- round(100/input$numModes, 1)

    withMathJax(
        paste(
            '\\(y = ', input$aHat_skewed, 
            ifelse(input$b1Hat_skewed>=0, "+", ""), input$b1Hat_skewed, 'x', 
            ifelse(input$b2Hat_skewed>=0, "+", ""), input$b2Hat_skewed, 'z', 
            '+', input$noise_skewed, ' u \\text{,} \\\\ ~~~~~\\text{where } u \\text{ is a ', 
                paste(rep(rounded,input$numModes), collapse="-"),' mixture of different normal distributions.} \\)'
        )
    )
})