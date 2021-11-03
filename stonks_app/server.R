#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(scales)
library(shinybusy)
library(tableHTML)
# library(shinyjs)
# 
# jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

source("functions.R")

# Valid tickers
asx_stocks <- read_csv('data/asx_cons_cleaned.csv')
stocks_vect <- as.vector(asx_stocks[[1]])
#manual add during testing
remove_vect <- c("1AG", "HGM", "AKN", "AHK", "BIN", "APD", "CGM", "AHN", "CDH", "ALT", "CYQDF", "EUR")
stocks_vect <- stocks_vect[! stocks_vect %in% remove_vect]
asx_etf <- read_csv('data/ETF_data_cleaned.csv')
etf_vect <- as.vector(asx_etf[[1]])
codes_vect <- c(stocks_vect, etf_vect)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # # Reset all
    # observeEvent(input$reset_button, {js$reset()})
    
    # Server-side inputs for selectizeInput
    updateSelectizeInput(session, "tickersInput", choices = codes_vect, server = TRUE)
    
    # Execute once user clicks go
    observeEvent(
        input$go,
        {
            ############################################################################
            
            # Setup
            
            ############################################################################
            
            # Loading gif
            # show_modal_spinner() # show the modal window
            show_modal_gif(
                # Banana
                # src='https://jeroen.github.io/images/banana.gif'
                # Rocket
                src='rocket launch GIF.gif'
                )
            
            # Start date
            start_date <- as.Date("2015-01-01","%Y-%m-%d")
            
            # Input tickers
            stonks <- input_validation(stonks_vect = input$tickersInput)
            
            ############################################################################
            
            # Data computations
            
            ############################################################################
            
            # Add .AX suffix to tickers and order
            tickers <- add_AX_to_tickers(stonks)
            
            # Fetch daily returns
            df <- get_prices_for_all_tickers(tickers, from=start_date)
            daily_df <- daily_returns(df)
            
            # Compute annual returns
            annual_df <- annual_returns(df)
            
            # Compute annualised mean array and covariance matrix
            mr <- mean_returns(daily_df)
            cv <- cov_returns(daily_df)
            
            # Compute analytical MVP and OP
            mvp <- global_minimum_variance_portfolio(mr, cv)
            op <- global_optimal_portfolio(mr, cv)
            
            # Generate samples for portfolio split
            sampled_splits <- random_splits(tickers, n=as.numeric(input$n_samples), thres=as.numeric(input$MaxAllo / 100))
            
            # Annual returns for each sampled split
            a <- sampled_splits %*% mr
            sampled_returns <- sampled_splits %*% mr
            
            # Annual risk for each sampled split
            sampled_risk <- sampled_splits %>% apply(1, compute_risk, cov_returns=cv)
            
            # Combine sampled portfolios into data frame
            sampled_portfolio_risk_return <- portfolios_summary_df(
                splits=sampled_splits,
                returns=sampled_returns,
                risk=sampled_risk
            )
            
            ############################################################################
            
            # Define outputs
            
            ############################################################################
            
            # Graph showing risk, return and sharpe ratio for each portfolio
            output$portfolio_plot <- renderPlotly({
                req(input$tickersInput)
                plot_efficient_frontier(
                        portfolios_df = sampled_portfolio_risk_return,
                        size = 2,
                        alpha = 1
                )
            })
            
            # Graph showing correlations for each pair of stocks in portfolio
            output$correlation_plot <- renderPlot({
                req(input$tickersInput)
                plot_stock_return_correlations(
                    daily_returns_df = daily_df
                )
            })
            
            output$relative_returns <- renderPlotly({
                req(input$tickersInput)
                daily_returns_with_date(
                    prices_df = df
                )
            })
            
            
            # Table showing top values with maximum sharpe ratio
            output$max_sharpe_ratio_table <- renderUI({
                req(input$tickersInput)
                sampled_portfolio_risk_return %>% 
                    arrange(desc(sharpe_ratio)) %>%
                    select(-c(1:3)) %>% 
                    head(1) %>% select_if(is.numeric) %>%
                    mutate_all(~ percent(.)) %>% tableHTML(round = 2, rownames = FALSE, border = 0)
            })
            
            # Table showing top values with minimum risk
            output$min_risk_table <- renderUI({
                req(input$tickersInput)
                sampled_portfolio_risk_return %>%
                    arrange(risk) %>%
                    select(-c(1:3)) %>%
                    head(1) %>% select_if(is.numeric) %>%
                    mutate_all(~ percent(.)) %>% tableHTML(round = 2, rownames = FALSE, border = 0)
            })
            
            # Print out the selected tickers
            output$selected_tickers <- renderPrint({
                req(input$tickersInput)
                tickers
            })
            
            # Print out table of all prices header
            output$all_prices_table <- renderTable({
                req(input$tickersInput)
                df %>%
                    group_by(ticker) %>%
                    top_n(1, date)
            }, digits = 2)
            
            # Print out summary of daily returns
            output$daily_returns_summary <- renderPrint({
                req(input$tickersInput)
                daily_df %>% summary()
            })
            
            # Print out random number
            output$runif <- renderPrint({
                runif(1)
            })
            
            # Render table to display the minimum variance portfolio
            output$analytical_MVP <- renderTable({
                req(input$tickersInput)
                mvp
            }, digits = 2)
            
            # Render table to display the optimal portfolio
            output$analytical_OP <- renderTable({
                req(input$tickersInput)
                op
            }, digits = 2)
            
            # Render table to display annual returns
            output$annual_returns <- renderUI({
                req(input$tickersInput)
                returns <- annual_df %>% 
                    select(ticker, returns) %>% 
                    mutate(ticker = str_replace(ticker, pattern = ".AX", replace = ""),
                           returns = percent(returns)) %>% 
                    column_to_rownames("ticker") %>% 
                    t() %>% tableHTML(round = 2, rownames = FALSE, border = 0)
            })
            
            # Render table to display risk
            output$annual_risk <- renderUI({
                req(input$tickersInput)
                risk_values <- diag(cv) %>% 
                    as.data.frame()
                colnames(risk_values) <- c("annualised_risk") 
                rownames(risk_values) <- rownames(risk_values) %>%
                    str_replace(pattern = ".AX", replacement = "")
                risk_values %>% 
                    mutate(annualised_risk = percent(round(annualised_risk, 2))) %>%
                    t() %>%
                    tableHTML(round = 2, rownames = FALSE, border = 0)
            })
            
            # Generate OP from randomly sampled splits
            output$sampled_OP <- renderTable({
                req(input$tickersInput)
                op
            })
            
            ############################################################################
            
            # Clean up
            
            ############################################################################
            
            # Loading gif
            # remove_modal_spinner() # remove it when done
            remove_modal_gif()
        }
    )
})