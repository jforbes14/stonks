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
    # img(src = "pw.png", height = 250, width = 250, align = "center"),
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
            
            HTML("
            <html>
            <head>
            <style>
            h1 {color:#006080;
                background-color: #1ac6ff;
                padding: 5px;
                border: 1px solid black;
                font-family:var(--chalet-book);}
            p {color:black;
               background-color: white;
               padding: 5px;
               border: 1px solid black;
               font-family:Tahoma;}
            </style>
            </head>
            <body>
                    
            <h1>
            How it works
            </h1>
            
            <br>
                 
            <p>
            The Stonks App, is a purpose-built web-app created to help you optimise your portfolio allocation. Investing for
            the first time is tricky, the investment landscape can seem fragmented and answers are often hard to find. What we're aiming for
            is to show you how you can optimse the structure of your portfolio by implementing a method established in 1952, and coined
            
            <a href='https://www.morningstar.com.au/learn/article/investing-basics-modern-portfolio-theory-expl/204228'>Modern Portfolio Theory</a>. 
            
            <br>
            <br>
            
            When we talk about a portfolio, typically we're referring to the combination of two or more assets and in this instance those two or more assets
            are stocks. Analysing a portfolio is complicated business, and while the behaviour of stocks may seem similar it is important to establish
            the relationship that stocks have with eachother. Establishing the level of the interaction between the assets is key to figuring out
            the optimal structure of the portfolio. Fortunately however, we've done all this pesky work for you and you don't have to worry about the complicated 
            math because that's all done behind the scenes. 
            
            <br>
            <br>
            
            A significant note to make is that this analysis is backwards looking, in that, establishing the optimal portfolio is done by observing the risk, 
            return and covariance of the assets over the last <strong> n </strong> years. What may be the optimal portfolio now might be different to the optimal 
            portfolio (of the same assets) in a year.
            
            <br>
            <br>
            
            Before we go though, we need help with one thing. That's for you to plug in the codes, those three to four letter symbols
            which represent the company name. Right now, the platform supports only ASX listed companies so ensure that you only jot down companies which are listed
            right here in Australia. There is also an option to place a maximum threshold on the proportion of an asset within the portfolio. This will limit the amount,
            as a percentage of the portfolio the asset will take up. For example, if you select 70% as the maximum threshold, no asset in the portfolio will take up
            more than 70%, even if the optimal portfolio suggests so.
            
            <br>
            <br>
            
            <strong> OTHER NOTES </strong>
            
            <br>
            <br>
            
            * disclaimer?
            <br>
            * confirm the attached link is appropriate reading
            <br>
            * try align the title to the top of the user input panel
            <br>
            * change colour scheme, clearly
            <br>
        
            </p>
            </body>
            </html>"),

        
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
