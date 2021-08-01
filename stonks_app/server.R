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

## ISSUES
# - Updating tickers does not re-do calculation and throws error
# - Entering 1 stock should just default to 1

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # # Reset all
    # observeEvent(input$reset_button, {js$reset()})
    
    observeEvent(
        input$go,
        {
            # Add .AX suffix to tickers and order
            tickers <- reactive({add_AX_to_tickers(input$tickers_vec)})

            # Fetch returns
            df <- get_prices_for_all_tickers(tickers())

            daily_df <- daily_returns(df)

            # output$mr <- mean_returns(daily_df)
            # output$cv <- cov_returns(daily_df)
            # mvp <- global_minimum_variance_portfolio(mr, cv)
            # op <- global_optimal_portfolio(mr, cv)

            # Print out the selected tickers
            output$selected_tickers <- renderPrint({
                req(input$tickers_vec)
                cat("As string:\n")
                tickers()
            })

            # Print out summary of all prices
            output$all_prices_summary <- renderPrint({
                req(input$tickers_vec)
                df %>% summary()
            })
            
            # Print out table of all prices header
            output$all_prices_table <- renderTable({
                req(input$tickers_vec)
                df %>% 
                    group_by(ticker) %>% 
                    top_n(1, date)
            }, digits = 2)

            # Print out test
            output$test2 <- renderPrint({
                req(input$tickers_vec)
                daily_df %>% summary()
            })

            # Print out random number
            output$runif <- renderPrint({
                runif(1)
            })

            # # Render table to display the minimum variance portfolio
            # output$MVP <- renderTable({
            #     req(input$tickers_vec)
            #     mvp
            # }, digits = 2)

            # # Render table to display the optimal portfolio
            # output$OP <- renderTable({
            #     req(input$tickers_vec)
            #     op
            # }, digits = 2)
        }
    )
})
