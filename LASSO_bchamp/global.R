# Packages ----

## UI
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)

## Server
library(glmnet)
library(zoo)
library(foreign)
library(plm)
library(scales)
library(dplyr)
library(psych)
library(DT)
library(crosstalk)
library(readr)
library(magrittr)

# adding promises JIC on live server
library(promises)
library(future)

#************************************************************************

# FUNC: Quick function for quote box ----
quoteBox <- function(id, text, source, align= ""){
    str <- paste0("HTML('<blockquote id=\"", id, "\", class=\"brooks\", style=\"text-align:", align, "; \">",
                text,
                "<footer style=\"font-style:italic;font-family: \"PT Sans Narrow\";\">", source, "</footer>
                </blockquote>'
           )"
    )

    eval(parse(text=str))
}
