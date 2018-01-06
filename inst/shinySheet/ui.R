#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source('charDescription.R')

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        theme = shinythemes::shinytheme('cosmo'),
        characterDescription(char)
    ))
