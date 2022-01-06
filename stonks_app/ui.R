library(bslib)
library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(tableHTML)
library(shinydashboardPlus)
library(shinydashboard)
library(bsplus)
library(htmltools)
library(jsonlite)
source("functions.R")


annual_returns_text <- HTML("The average annual return for each of your selected stocks. Returns can be compared across assets, but should be considered with reference to their risk.")

annual_risk_text <- HTML("The average annual risk for each of your selected stocks. In this instance, risk is defined as the variance of daily returns.")

optimal_portfolio_text <- HTML("Your optimal portfolio! This portfolio will have the highest level of return for its level of risk. Based on historical data, the following breakdown is the optimal allocation of your assets.")

minimum_variance_portfolio_text <- HTML("This is an alternate view of your portfolio. This portfolio is the one with the lowest level of risk, and can be considered if you are risk averse.")

candidate_portfolios_text <- HTML("Each point here is an alternative portfolio showing it's risk and return. The optimal portfolio is the shaded point. Hover to see the breakdown of stocks.")

price_plot_text <- HTML("The historical returns for each stock.")

stock_correlations_text <- HTML("The correlations between returns for each pair of stocks.")

disclaimer_string <- "The information on this website is for general information only and should not be taken as financial advice nor any professional advice from the website owner. We are not liable for any loss caused from information provided directly or indirectly. The tools on this website are illustrative and should not be taken as a substitute for professional advice. All reasonable care has been taken in development; however, we provide absolutely no warranty."

navbarPage(
  "Stonks",
  fluid=TRUE,
  # tags$head(tags$script(src="www/hello.js")),
  # tags$head(tags$link(rel="stylesheet", 
  #                     type="text/css",
  #                     href="www/mycss.css")),
  theme  = bs_theme(
    version = 5,
    bootswatch = "simplex",
    # bg = '#e0e0e0',
    # fg = 'black',
    primary = "purple", 
    base_font = font_google("Quicksand")
    ),
  tabPanel("Portfolio Optimisation",
              h2("", class="d-block d-md-none"),
              div(class="container-fluid",
                  img(src="mobile-banner.png", 
                      class="d-block d-md-none",
                      alt="Responsive image",
                      height="30%", width="100%"),
                  h2("", class="d-none d-md-block"),
                  h2("", class="d-block d-md-none"),
                  div(class="row",
                      div(class="col-md-3",
                          div(class="well"
                              # Maximum percentage for any one stock
                              , sliderInput(
                                  "MaxAllo",
                                  "Enter a maximum level of concentration for anyone one stock in your portfolio",
                                  min = 0,
                                  max = 100,
                                  value = 100,
                                  step = 5,
                                  post = "%"
                              ),
                              
                              selectizeInput(
                                  "tickersInput",
                                  "Enter up to 10 stock tickers with a minimum of two",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(create = TRUE, maxItems = 10)
                              ),
                              
                              downloadButton("downloadData", "Download"
                              ),
                              
                              br(),
                              
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
                          
                      br(),
                      
                      ),
                      
                      div(class="col-md-9",
                          img(src = "stonks-banner-thin.png", class="d-none d-md-block", alt="Responsive image", 
                              height = "auto", width = "100%", align = "right")
                      ),
                      
                      br(),
                      
                      conditionalPanel(
                          condition = "input.go",
                          
                      br(),
                      
                      dashboardBody(
                      
                          fluidRow(
                            
                            column(6,
                            
                            tabsetPanel(
                                   
                                        tabPanel(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                                                width = 6, title = "Optimal Portfolio",
                                                p(optimal_portfolio_text, class = "text-muted"),
                                                includeCSS('www/mycss.css'),
                                                uiOutput("max_sharpe_ratio_table")),
                                        
                                        tabPanel(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                                                 id = "Minimum Variance Portfolio",
                                                 title = "Minimum Variance Portfolio",
                                                 p(minimum_variance_portfolio_text, class = "text-muted"),
                                                 uiOutput("min_risk_table"))
                                        
                            )),
                            
                            column(6,

                            tabsetPanel(
                                        
                                        tabPanel(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                                                 id = "Annual Return",
                                                 title = "Annual Return",
                                                 p(annual_returns_text, class = "text-muted"),
                                                 uiOutput("annual_returns")),
                                        
                                        tabPanel(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                                                 id = "Annual Risk",
                                                 title = "Annual Risk",
                                                 p(annual_risk_text, class = "text-muted"),
                                                 uiOutput("annual_risk")),
                                        
                                        
                            )),
                            
                            style='width: 100%; height: 50%'
                            
                            ),
                        
                          fluidRow(
                            width = 6,
                            box(width = NULL, title = "Candidate Portfolios",
                                p(candidate_portfolios_text, class = "text-muted"),
                                plotlyOutput("portfolio_plot"))
                            
                            ),
                        
                          fluidRow(
                            
                            width = 12,
                            
                            box(width = 6, title = "Price Plot",
                                p(price_plot_text, class = "text-muted"),
                                plotlyOutput('relative_returns')),
                                
                            box(width = 6, title = "Stock Correlations",
                                p(stock_correlations_text, class = "text-muted"),
                                plotOutput("correlation_plot"))
                                
                          ),
                      
                          br()
                          
                          # div(class =  "col-12 col-md-12",
                          #     htmlTemplate("www/accordion.html")),
                          # 
                          # br(),
                    
                           )
                        )
                    )
                ),
                  
        div(class =  "col-12 col-md-12", htmlTemplate("www/disclaimer-button.html"),
                    
        br(),

        ),
        
        htmlTemplate("www/linkedin.html"),
  
    ),
  
  tabPanel("Contact")
)
