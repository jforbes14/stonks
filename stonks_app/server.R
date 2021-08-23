#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(shinyjs)
# 
# jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # # Reset all
    # observeEvent(input$reset_button, {js$reset()})
    
    observeEvent(
        input$go,
        {
            ############################################################################
            
            # Data computations
            
            ############################################################################
            # 
            # stonks_list <- c()
            # 
            stonks <- input_validation(input$tickersInput)
            
            print(stonks)
            
            # Add .AX suffix to tickers and order
            tickers <- add_AX_to_tickers(stonks)
            
            # Fetch returns
            df <- get_prices_for_all_tickers(tickers, from=input$start_date)
            daily_df <- daily_returns(df)
            
            # Compute annualised mean array and covariance matrix
            mr <- mean_returns(daily_df)
            cv <- cov_returns(daily_df)
            
            # Compute analytical MVP and OP
            mvp <- global_minimum_variance_portfolio(mr, cv)
            op <- global_optimal_portfolio(mr, cv)
            
            # Generate samples for portfolio split
            sampled_splits <- random_splits(tickers, n=as.numeric(input$n_samples))
            
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
            output$portfolio_plot <- renderPlot({
                req(input$tickersInput)
                plot_sampled_portfolios(
                    portfolios_df = sampled_portfolio_risk_return,
                    size = 1,
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
            
            
            # Table showing top values with maximum sharpe ratio
            output$max_sharpe_ratio_table <- renderTable({
                req(input$tickersInput)
                sampled_portfolio_risk_return %>%
                    arrange(desc(sharpe_ratio)) %>%
                    head()
            }, digits = 4)
            
            # Table showing top values with minimum risk
            output$min_risk_table <- renderTable({
                req(input$tickersInput)
                sampled_portfolio_risk_return %>%
                    arrange(risk) %>%
                    head()
            }, digits = 4)
            
            # Print out the selected tickers
            output$selected_tickers <- renderPrint({
                req(input$tickersInput)
                cat("As string:\n")
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
            
            # Generate OP from randomly sampled splits
            output$sampled_OP <- renderTable({
                req(input$tickersInput)
                op
            }, digits = 2)
        }
    )
})