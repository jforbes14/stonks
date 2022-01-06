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

intro <- HTML("
Stonks is a purpose-built app to help you decide how you should allocate money to stocks you are considering investing in.
<br>
<br>
Once you tell us which ASX stocks you want to invest in, we can tell you how much of each to hold so that you get the greatest return relative to risk - the optimal portfolio.
<br>
<br>
The dashboard below will display the optimal portfolio along with supporting analysis illustrating the performance and relationships between the stocks.")

annual_returns_text <- HTML("The annual return for each of your selected stocks.")

annual_risk_text <- HTML("The annual risk (variance) for each of your selected stocks.")

optimal_portfolio_text <- HTML("Your optimal portfolio with the percentage allocation for each stock. This is the portfolio that has had the greatest return relative to its risk.")

minimum_variance_portfolio_text <- HTML("The portfolio with the least risk. This is likely to have lower returns.")

candidate_portfolios_text <- HTML("Each point here is an alternative portfolio showing it's risk and return. The optimal portfolio is the shaded point. Hover to see the breakdown of stocks.")

price_plot_text <- HTML("The historical returns for each stock.")

stock_correlations_text <- HTML("The correlations between returns for each pair of stocks.")

disclaimer_string <- "The information on this website is for general information only and should not be taken as financial advice nor any professional advice from the website owner. We are not liable for any loss caused from information provided directly or indirectly. The tools on this website are illustrative and should not be taken as a substitute for professional advice. All reasonable care has been taken in development; however, we provide absolutely no warranty."

# # Reset all
# useShinyjs(),                                           # Include shinyjs in the UI
# extendShinyjs(text = jsResetCode, functions = "reset"), # Add the js code to the page
# actionButton("reset_button", "Reset Page"),

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
      
      useShinyjs(),
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
                       ),
                   # Space
                   br(),
                   # Everything else only appears once go is clicked
                   conditionalPanel(
                       condition = "input.go",
                   # Annual stock returns
                   fluidRow(
                       column(
                           width = 12,
                           box(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                               width = NULL, title = "Optimal Portfolio",
                               p(optimal_portfolio_text, class = "text-muted"),
                               includeCSS('www/mycss.css'),
                               uiOutput("max_sharpe_ratio_table")
                           )
                           )
                       ),
                   # Split column: tables and efficient frontier
                   fluidRow(
                       column(
                           width = 6,
                           box(width = NULL, title = "Annual Return",
                               p(annual_returns_text, class = "text-muted"),
                               uiOutput("annual_returns")
                           ),
                           box(width = NULL, title = "Annual Risk",
                               p(annual_risk_text, class = "text-muted"),
                               uiOutput("annual_risk")
                           ),
                           box(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                               width = NULL, title = "Minimum Variance Portfolio",
                               uiOutput("min_risk_table")
                               )
                           ),
                       column(width = 6,
                              box(width = NULL, title = "Candidate Portfolios",
                                  p(candidate_portfolios_text, class = "text-muted"),
                                  plotlyOutput("portfolio_plot")
                                  )
                              )
                       ),
                   # price plot and correlation matrix
                   fluidRow(
                       column(width = 6,
                              box(width = NULL, title = "Price Plot",
                                  p(price_plot_text, class = "text-muted"),
                                  plotlyOutput('relative_returns')
                              )
                       ),
                       column(width = 6,
                              box(width = NULL, title = "Stock Correlations",
                                  p(stock_correlations_text, class = "text-muted"),
                                  plotOutput("correlation_plot")
                              )
                       )
                   )
                   )
                   # ,
                   # # Disclaimer
                   # fluidRow(
                   #     column(width = 12,
                   #            box(width = NULL, title = "Disclaimer",
                   #                p(disclaimer_string, class = "text-muted")
                   #                )
                   #            )
                   #     )
                   )
            )
        )
    )

