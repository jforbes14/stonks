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

asx_stocks <- read_csv('data/asx_cons_cleaned.csv')
stocks_vect <- as.vector(asx_stocks[[1]])
asx_etf <- read_csv('data/ETF_data_cleaned.csv')
etf_vect <- as.vector(asx_etf[[1]])
codes_vect <- c(stocks_vect, etf_vect)



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("STONKS APP"),
    img(src = "pw.png", height = 250, width = 250, align = "center"),
    br(),
    br(),
    br(),
    
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
                , choices = codes_vect
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
            
            sliderInput("MaxAllo", h3("Option to choose a percentage"),
                        min = 0,
                        max = 100,
                        value = 100,
                        step = 5,
                        post = "%"),
            
            actionButton("go", "Start the Analysis")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            # Print tickers
            h4("Stocks in your portfolio")
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
            , h4("Sampled: Most Optimal Portfolios (Maximum Sharpe Ratio)")
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
