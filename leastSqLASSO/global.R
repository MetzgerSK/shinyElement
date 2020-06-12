library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(magrittr)
library(shinyjqui)

# server
library(ggplot2)
library(stargazer)
library(glmnet)
library(plotly)

#****************************************************
# Pop standard deviation
sdpop <- function(x){
    sd(x)*sqrt((length(x)-1)/length(x))
}