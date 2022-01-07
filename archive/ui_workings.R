#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(tableHTML)
source("functions.R")

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

disclaimer_string <- "The information on this website is for general information only and should not be taken as financial advice nor any professional advice from the website owner. We are not liable for any loss caused from information provided directly or indirectly. The tools on this website are illustrative and should not be taken as a substitute for professional advice. All reasonable care has been taken in development; however, we provide absolutely no warranty."


# Define UI for application that draws a histogram
dashboardPage(
  
  # Skin
  skin = "purple",
  
  # Header
  dashboardHeader(title = "stonks"),
  
  # Sidebar
  dashboardSidebar(
    disable = TRUE
  ),
  
  # Tabs
  dashboardBody(
    fluidRow(
      column(width = 3,
             box(
               width = NULL,
               status = "warning",
               # Selectize tickers
               selectizeInput(
                 "tickersInput",
                 "Enter up to 10 stock tickers with a minimum of two",
                 choices = NULL,
                 multiple = TRUE,
                 options = list(create = TRUE, maxItems = 10)
               ),
               # Maximum percentage for any one stock
               sliderInput(
                 "MaxAllo",
                 "Enter a maximum threshold for any one stock",
                 min = 0,
                 max = 100,
                 value = 100,
                 step = 5,
                 post = "%"
               ),
               # Specify number of samples for sampling approach
               selectInput(
                 "n_samples",
                 "Select the number of portfolio samples",
                 choices = list(
                   "100" = 100, "1000" = 1000, "1000" = 1000,
                   "10000" = 10000,"100000" = 100000, "1000000" = 1000000
                 ),
                 selected = 1
               ),
               actionButton("go", "Start the Analysis")
             ),
             box(width = NULL, title = "Disclaimer",
                 p(disclaimer_string, class = "text-muted", style = "font-size:10px")
             )
      ),
      column(width = 9,
             # Intro
             fluidRow(
               column(
                 width = 12,
                 img(src = "stonks-banner-thin.png", width = "100%", align = "center")
                 # box(width = NULL, title = "Intro", p(intro, style = "font-size:16px"))
               )
             )
      )
    )
  )
)
