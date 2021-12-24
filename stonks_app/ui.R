library(bslib)
library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(tableHTML)
source("functions.R")

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
                )
                # disclaimer
            ),
            div(class="col-md-9",
                img(src = "stonks-banner-thin.png", class="d-none d-md-block", alt="Responsive image", 
                    width = "100%", align = "right")
                ),
            conditionalPanel(
                condition = "input.go",
                
            )    
                
        )
    )
)
