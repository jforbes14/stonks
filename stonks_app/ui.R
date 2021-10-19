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
source("functions.R")

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

intro <- HTML("The Stonks App, is a purpose-built web-app created to help you optimise your portfolio allocation. Investing for the first time is tricky, the investment landscape can seem fragmented and answers are often hard to find. We've all been there, your best friends' uncle went to school with someone who was tipped off about a stock that's going to explode. You want to invest before it's too late, but you also know that it's not totally responsible to allocate all your hard earned money into it, so you want to balance it out with some other stable more stable investments. So right now, you've got the assets in mind, but how do you know how much money to allocate to each? Well, fortunately, we want to help you, and we want to start by introducing you to the <a href='https://www.morningstar.com.au/learn/article/investing-bWes-modern-portfolio-theory-expl/204228'>Modern Portfolio Theory</a>. A method of structuring your portfolio by optimising it for the highest return-risk trade off. We offer some recommended reading above to help you understand the nuances of this approach, but we will delve into some of the details below.
<br>
<br>
When we talk about a portfolio, typically we're referring to the combination of two or more assets. In this instance those assets are stocks. Analysing a portfolio is complicated business, and while the profile of stocks may seem homogenous it is important to understand the level of interplay between these assets differ and are key to establshing the optimal structure of the portfolio. This may all seem like a bunch of words on a page, but there are many inputs required into identifying the optimal portfolio in accordance with the Modern Portfolio Theory described above. Don't fret however, we'll do all the hard-work, but understanding the basis of the recommendation is always a good start!
<br>
<br>
Before the analysis kicks off, a few points of discussion are noted. The first being that this analysis is backwards looking, in that, establishing the optimal portfolio is done by observing the risk, return and covariance of the assets over the last <strong> n </strong> years. What may be the optimal portfolio now might be different to the optimal portfolio (of the same assets) in a year. The second quandary is such that the optimal portfolio presented is identified through a method of sampling. This is different to what is considered an analytical outcome. This is because, in some circumstances, the analytical approach will conclude a portfolio, consisting of negatively weighted assets is the optimal one, and whilst this is possible in practice, through shorting, it is not practical for most investors.
<br>
<br>
Before we go though, we need help with one thing. That's for you to plug in the codes, those three to four letter symbols which represent the company name. Right now, the platform supports only ASX listed companies so ensure that you only jot down companies which are listed right here in Australia. There is also an option to place a maximum threshold on the proportion of an asset within the portfolio. This will limit the amount, as a percentage of the portfolio the asset will take up. For example, if you select 70% as the maximum threshold, no asset in the portfolio will take up more than 70%, even if the optimal portfolio suggests so.
<br>"
)

disclaimer_string <- "The information on this website is for general information only. It should not be taken as constituting professional advice from the website owner. We are not liable for any loss caused from information provided directly or indirectly. The tools on this website are illustrative and should not be taken as a substitute for professional advice. All reasonable care has been taken in development; however, we provide absolutely no warranty."

# # Reset all
# useShinyjs(),                                           # Include shinyjs in the UI
# extendShinyjs(text = jsResetCode, functions = "reset"), # Add the js code to the page
# actionButton("reset_button", "Reset Page"),

# Define UI for application that draws a histogram
dashboardPage(
    
    # Skin
    skin = "purple",
    
    # Header
    dashboardHeader(title = "Stonks"),

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
                           "Enter tickers for up to 10 stocks (at least 2)",
                           choices = NULL,
                           multiple = TRUE,
                           options = list(create = TRUE, maxItems = 10)
                       ),
                       # Maximum percentage for any one stock
                       sliderInput(
                           "MaxAllo",
                           "Enter a maximum percentage for any one stock",
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
                       p(
                           class = "text-muted",
                           disclaimer_string
                           )
                   )
            ),
            column(width = 9,
                   # RHS
                   fluidRow(
                       # intro
                       box(width = NULL, title = "Intro",
                           intro
                           ),
                       # annual stock returns
                       box(width = NULL, title = "Annual returns on selected stocks",
                           verbatimTextOutput("selected_tickers")
                       ),
                       # split column: tables and efficient frontier
                       fluidRow(
                           column(
                               width = 6,
                               box(width = NULL, title = "Optimal Portfolio",
                                   tableOutput("max_sharpe_ratio_table")
                                   ),
                               box(width = NULL, title = "Minimum Variance Portfolio",
                                   tableOutput("min_risk_table")
                                   )
                               ),
                           column(width = 6,
                                  box(width = NULL, title = "Candidate Portfolios",
                                      plotlyOutput("portfolio_plot")
                                      )
                                  )
                           ),
                       
                       fluidRow(
                           column(width = 6,
                                  box(width = NULL, title = "Price Plot",
                                      plotlyOutput('relative_returns')
                           ),
                           # price plot and correlation matrix
                            column(width = 6,
                                  box(width = NULL, title = "Stock Correlations",
                                      plotOutput("correlation_plot")
                                  )
                           )
                       )
                   )
                )
            )
        )
    )
)
