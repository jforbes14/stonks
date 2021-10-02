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

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

asx_stocks <- read_csv('data/asx_cons_cleaned.csv')
stocks_vect <- as.vector(asx_stocks[[1]])
asx_etf <- read_csv('data/ETF_data_cleaned.csv')
etf_vect <- as.vector(asx_etf[[1]])
codes_vect <- c(stocks_vect, etf_vect)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    

    # Application title
    titlePanel
    (
        h1("STONKS APP", align = "center")
    ),
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
            h1 {
                font-family: Gill Sans, sans-serif);
            }
            p {
                color:black;
               background-color: white;
               padding: 5px;
               border: 1px solid black;
               font-family: Gill Sans, sans-serif;
            }
            </style>
            </head>
            <body>
                 
            <p>
            The Stonks App, is a purpose-built web-app created to help you optimise your portfolio allocation. Investing for
            the first time is tricky, the investment landscape can seem fragmented and answers are often hard to find. We've all been there,
            your best friends' uncle went to school with someone who was tipped off about a stock that's going to explode. You want to invest before 
            it's too late, but you also know that it's not totally responsible to allocate all your hard earned money into it, so you want to balance it out with
            some other stable more stable investments. So right now, you've got the assets in mind, but how do you know how much money to allocate to each? Well,
            fortunately, we want to help you, and we want to start by introducing you to the
    
             <a href='https://www.morningstar.com.au/learn/article/investing-bWes-modern-portfolio-theory-expl/204228'>Modern Portfolio Theory</a>. 
            
            A method of structuring your portfolio by optimising it for the highest return-risk trade off. We offer some recommended reading above
            to help you understand the nuances of this approach, but we will delve into some of the details below.
            
            <br>
            <br>
            
            When we talk about a portfolio, typically we're referring to the combination of two or more assets. In this instance those assets are stocks. 
            Analysing a portfolio is complicated business, and while the profile of stocks may seem homogenous it is important to understand
            the level of interplay between these assets differ and are key to establshing the optimal structure of the portfolio. This may all seem like a bunch of
            words on a page, but there are many inputs required into identifying the optimal portfolio in accordance with the Modern Portfolio Theory
            described above. Don't fret however, we'll do all the hard-work, but understanding the basis of the recommendation is always a good start!
            
            <br>
            <br>
            
            Before the analysis kicks off, a few points of discussion are noted. The first being that this analysis is backwards looking, in that, establishing the optimal portfolio is done by observing the risk, 
            return and covariance of the assets over the last <strong> n </strong> years. What may be the optimal portfolio now might be different to the optimal 
            portfolio (of the same assets) in a year. The second quandary is such that the optimal portfolio presented is identified through a method of sampling. This is different
            to what is considered an analytical outcome. This is because, in some circumstances, the analytical approach will conclude a portfolio, consisting of negatively weighted
            assets is the optimal one, and whilst this is possible in practice, through shorting, it is not practical for most investors.
            
            <br>
            <br>
            
            Before we go though, we need help with one thing. That's for you to plug in the codes, those three to four letter symbols
            which represent the company name. Right now, the platform supports only ASX listed companies so ensure that you only jot down companies which are listed
            right here in Australia. There is also an option to place a maximum threshold on the proportion of an asset within the portfolio. This will limit the amount,
            as a percentage of the portfolio the asset will take up. For example, if you select 70% as the maximum threshold, no asset in the portfolio will take up
            more than 70%, even if the optimal portfolio suggests so.
            
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
            , h4("Sampled: Optimal Portfolio (Maximum Sharpe Ratio)")
            , tableOutput("max_sharpe_ratio_table")
            
            # Display maximum sharpe ratio
            , h4("Sampled: Minimum Variance Portfolio")
            , tableOutput("min_risk_table")
            
            # Display plot of portfolios
            , h4("Sampled: Portfolio Risk, Return and Sharpe Ratio")
            , plotlyOutput("portfolio_plot")
            
            , h4("Random number")
            , verbatimTextOutput("runif")
            
            , hr()
            , print
            (
            "The information on this website is for general information only. It should not be taken as constituting professional advice from the website owner.
            We are not financial advisors. You should consider seeking independent legal, financial, taxation or other advice to check how the website information relates to your unique circumstances.
            We are not liable for any loss caused, whether due to negligence or otherwise arising from the use of, or reliance on, the information provided directly or 
            indirectly, by use of this website. The tools on this website are provided for your information and to illustrate scenarios. The results should not be taken as a substitute for professional advice. 
            All reasonable care has been taken in preparing and designing the calculators and tools; however, We provides no warranties and makes no representation that the 
            information provided by tools is appropriate for your particular circumstances or indicates you should follow a particular course of action."
            )
        )
    )
))
