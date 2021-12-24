library(bslib)
library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(tableHTML)
source("functions.R")


annual_returns_text <- HTML("The annual return for each of your selected stocks.")

annual_risk_text <- HTML("The annual risk (variance) for each of your selected stocks.")

optimal_portfolio_text <- HTML("Your optimal portfolio with the percentage allocation for each stock. This is the portfolio that has had the greatest return relative to its risk.")

minimum_variance_portfolio_text <- HTML("The portfolio with the least risk. This is likely to have lower returns.")

candidate_portfolios_text <- HTML("Each point here is an alternative portfolio showing it's risk and return. The optimal portfolio is the shaded point. Hover to see the breakdown of stocks.")

price_plot_text <- HTML("The historical returns for each stock.")

stock_correlations_text <- HTML("The correlations between returns for each pair of stocks.")

disclaimer_string <- "The information on this website is for general information only and should not be taken as financial advice nor any professional advice from the website owner. We are not liable for any loss caused from information provided directly or indirectly. The tools on this website are illustrative and should not be taken as a substitute for professional advice. All reasonable care has been taken in development; however, we provide absolutely no warranty."


bootstrapPage(
    theme  = bs_theme(version = 5),
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
                        "Enter a maximum threshold for any one stock",
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
                # disclaimer
                box(width = NULL, title = "Disclaimer",
                    p(disclaimer_string, class = "text-muted", style = "font-size:10px")
                )
            ),
            div(class="col-md-9",
                img(src = "stonks-banner-thin.png", class="d-none d-md-block", alt="Responsive image", 
                    width = "100%", align = "right")
                ),
            conditionalPanel(
                condition = "input.go",
                
               fluidRow(
                   column(
                       width = 12,
                    box(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                        width = NULL, title = "Optimal Portfolio",
                        p(optimal_portfolio_text, class = "text-muted"),
                        includeCSS('www/mycss.css'),
                        uiOutput("max_sharpe_ratio_table")
                    )
                )
                
            ),
            
            fluidRow(
                column(
                    width = 6,
                    box(width = NULL, title = "Annual Return",
                        p(annual_returns_text, class = "text-muted"),
                        uiOutput("annual_returns")
                    ),
                    box(width = NULL, title = "Annual Risk",
                        p(annual_risk_text, class = "text-muted"),
                        uiOutput("annual_risk")
                    ),
                    box(style = "overflow-x:scroll; max-height: 300px; position:relative; align: centre",
                        width = NULL, title = "Minimum Variance Portfolio",
                        uiOutput("min_risk_table")
                    )
                ),
                column(width = 6,
                       box(width = NULL, title = "Candidate Portfolios",
                           p(candidate_portfolios_text, class = "text-muted"),
                           plotlyOutput("portfolio_plot")
                       )
                )
            ),
            fluidRow(
                column(width = 6,
                       box(width = NULL, title = "Price Plot",
                           p(price_plot_text, class = "text-muted"),
                           plotlyOutput('relative_returns')
                       )
                ),
                column(width = 6,
                       box(width = NULL, title = "Stock Correlations",
                           p(stock_correlations_text, class = "text-muted"),
                           plotOutput("correlation_plot")
                       )
                    )
                )
            )
        )
    )
)
