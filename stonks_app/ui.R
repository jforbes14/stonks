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
            # Selectize
            selectizeInput(
                "tickersInput"
                , "Enter tickers for up to 10 stocks (at least 2)"
                , choices = NULL
                , multiple = TRUE
                , options = list(create = TRUE, maxItems = 10)
            ),
            
            actionButton("go", "Press GO")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Print tickers
            h4("You have selected")
            , verbatimTextOutput("selected_tickers")
            , h4("All prices header")
            , tableOutput("all_prices_table")
            , h4("Daily returns summary")
            , verbatimTextOutput("daily_returns_summary")
            
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
            
            , h4("Random number")
            , verbatimTextOutput("runif")
        )
    )
))
