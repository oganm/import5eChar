#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

source('charDescription.R')

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        includeCSS('www/style.css'),
        theme = shinythemes::shinytheme('cosmo'),
        characterDescription(char),
        knobInput(
            inputId = "knob6",
            label = "Cursor mode:",
            value = 50,
            thickness = 0.3,
            width = 50,
            height = 50
        )
    ))
