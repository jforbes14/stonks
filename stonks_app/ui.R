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

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("STONKS APP"),
    
    # # Reset all
    # useShinyjs(),                                           # Include shinyjs in the UI
    # extendShinyjs(text = jsResetCode, functions = "reset"), # Add the js code to the page
    # actionButton("reset_button", "Reset Page"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            # Selectize tickers
            selectizeInput(
                "tickersInput"
                , "Enter tickers for up to 10 stocks (at least 2)"
                , choices = NULL
                , multiple = TRUE
                , options = list(create = TRUE, maxItems = 10)
            ),
            
            # Specify date you want to get data from
            sliderInput("start_date", h3("Start date for analysis"),
                        min = as.Date("2015-01-01","%Y-%m-%d"),
                        max = as.Date("2021-01-01","%Y-%m-%d"),
                        value=as.Date("2015-01-01"),
                        timeFormat="%d/%m/%Y"),
            
            # Specify number of samples for sampling approach
            selectInput("n_samples", h3("Number of samples"), 
                        choices = list("100" = 100, "1000" = 1000,
                                       "1000" = 1000, "10000" = 10000,
                                       "100000" = 100000, "1000000" = 1000000), selected = 1),
            
            actionButton("go", "Press GO")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Print tickers
            h4("You have selected")
            , verbatimTextOutput("selected_tickers")
            
            # Display plot of correlations
            , h4("Correlation of stocks selected in portfolio")
            , plotOutput("correlation_plot")
            
            # Display analytical MVP
            , h4("Analytical: Minimum Variance Portfolio")
            , tableOutput("analytical_MVP")
            
            # Display analytical OP
            , h4("Analytical: Optimal Portfolio")
            , tableOutput("analytical_OP")
            
            # Display maximum sharpe ratio
            , h4("Sampled: Most Optimal Portfolios (max. sharpe ratio)")
            , tableOutput("max_sharpe_ratio_table")
            
            # Display maximum sharpe ratio
            , h4("Sampled: Minimum Variance Portfolios")
            , tableOutput("min_risk_table")
            
            # Display plot of portfolios
            , h4("Sampled: Portfolio Risk, Return and Sharpe Ratio")
            , plotOutput("portfolio_plot")
            
            , h4("Random number")
            , verbatimTextOutput("runif")
        )
    )
))
