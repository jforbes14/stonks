
#
# Defining functions for the stonks app
#
library(tidyquant)
library(tidyverse)
rdirichlet <- MCMCpack::rdirichlet
options("getSymbols.warning4.0"=FALSE)

# Test function
# Input
# Returns
# "hello world" text
hello <- function() {
  return("hello world")
}


asx_stocks <- read_csv('data/asx_cons_cleaned.csv')
stocks_vect <- as.vector(asx_stocks[[1]])
#manual add during testing
remove_vect <- c("1AG")
stocks_vect <- stocks_vect[! stocks_vect %in% remove_vect]
asx_etf <- read_csv('data/ETF_data_cleaned.csv')
etf_vect <- as.vector(asx_etf[[1]])
codes_vect <- c(stocks_vect, etf_vect)



input_validation <- function(stonks_vect=NULL){
  stonks_list <- c()
  stonks_vect <- unique(toupper(stonks_vect))
  stonks_vect <<- stonks_vect
  for (ticker in stonks_vect) {
    if (ticker %in% codes_vect == TRUE){
      print(paste(ticker, 'on the ASX'))
      stonks_list <- append(stonks_list, ticker)
    }else{
      print(paste(ticker, 'not on the ASX'))
      # break
    }
  }
  return (stonks_list)
}

# Add .AX prefix to stocks
# Input
# tickers: vector of ASX tickers
# Returns
# Vector of ASX tickers with .AX suffix
add_AX_to_tickers <- function(tickers) {
  tickers = toupper(tickers)
  for (ticker in tickers) {
    if (!grepl('.AX', ticker)) {
      tickers[tickers == ticker] = paste0(ticker, '.AX')
    }
  }
  return(sort(tickers))
}

# Get prices for a given stock
# Input
# ticker: ASX ticker
# from: date to fetch stock prices from
# to: date to fetch stock prices up to
# Returns
# Data frame of daily adjusted stock price
get_prices_for_ticker <- function(ticker, from="2015-01-01", to=Sys.Date()) {
  x <- suppressWarnings(
    getSymbols(ticker,
               src = 'yahoo',
               from=from, 
               to=Sys.Date(),
               auto.assign = FALSE
    ) %>% as.data.frame()
  )
  colnames(x) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
  extract <- data.frame(ticker = ticker, date = rownames(x), price = x$Adjusted) %>% 
    fill(price, .direction = "up")
  return(extract)
}

# Get data frame of prices for a set of stocks
# Input
# tickers: vector of ASX tickers
# from: date to fetch stock prices from
# to: date to fetch stock prices up to
# Returns
# Data frame of daily adjusted stock price
get_prices_for_all_tickers <- function(tickers, from="2015-01-01", to=Sys.Date()) {
  for (ticker in tickers) {
    print(paste("Getting prices for", ticker))
    prc = get_prices_for_ticker(ticker, from, to)
    if (ticker != tickers[1]) {
      prices_df = prices_df %>% 
        bind_rows(prc)
    } else {
      prices_df = prc
    }
  }
  
  prices_df <- prices_df %>% 
    mutate(date = as.Date(date))
  
  return(prices_df)
}

# Get daily returns for each stock (common dates only)
# Input
# prices_df: data frame of daily stock prices
# Returns
# Data frame of daily returns, for each stock, keeping only common dates
daily_returns <- function(prices_df) {
  tickers = unique(prices_df$ticker)
  
  daily_returns <- prices_df %>%
    group_by(ticker) %>% 
    arrange(date) %>% 
    mutate(return = (price - lag(price, 1))/lag(price, 1),
           log_return = log(price) -log(lag(price, 1)))
  
  valid_dates <- daily_returns %>%
    filter(!is.na(return)) %>%
    group_by(date) %>%
    summarise(n = n()) %>%
    filter(n == length(tickers)) %>%
    select(date)
  
  daily_returns <- daily_returns %>%
    filter(date %in% valid_dates$date) %>%
    select(-c(price, return)) %>%
    spread(key = ticker, value = log_return) %>%
    select(-date)
  
  return(daily_returns)
}

# Get annual returns for each stock using all available dates
# Input
# prices_df: data frame of daily stock prices
# Returns
# Data frame of annual returns, for each stock, over the dates it is available
annual_returns <- function(prices_df) {
  annual_returns <- prices_df %>% 
    group_by(ticker) %>% 
    filter(date == min(date)) %>% 
    rename(start_price = price, start_date = date) %>% 
    left_join(
      prices_df %>% 
        group_by(ticker) %>% 
        filter(date == max(date)) %>% 
        rename(end_price = price, end_date = date),
      by = 'ticker'
    ) %>% 
    mutate(
      days_between = as.numeric(end_date - start_date, d = days),
      years_between = days_between/365,
      returns = (end_price/start_price)^(1/years_between) - 1
    )
  return(annual_returns)
}


# Get vector of mean returns, and covariance matrix, for the portfolio
# Input
# daily_returns: data frame of daily returns for each stock in the portfolio
# Returns
# mean_returns: vector of annualized mean returns for each stock
# cov_returns: covariance matrix of (annualized) returns for the portfolio

# Mean
mean_returns <- function(daily_returns) {
  mean_daily_returns <- colMeans(daily_returns)
  mean_returns <- (mean_daily_returns + 1)^252 - 1
  return(mean_returns)
}

# Covariance
cov_returns <- function(daily_returns) {
  return(cov(daily_returns) * 252)
}

# Global minimum variance portfolio (analytical)
# Input
# mean_returns: vector of annualized returns
# cov_returns: covariance matrix of annualized returns
# Returns
# Vector of weights for global minimum variance portfolio

global_minimum_variance_portfolio <- function(mean_returns, cov_returns) {
  one_vec = rep(1, length(mean_returns))
  cov_returns_inv = solve(cov_returns)
  top.mat = cov_returns_inv%*%one_vec
  bot.val = as.numeric((t(one_vec)%*%cov_returns_inv%*%one_vec))
  mvp = top.mat[,1]/bot.val
  mvp_df = t(mvp) %>% as.data.frame()
  return(mvp_df)
}

# Global optimal portfolio, for maximum sharpe ratio (analytical)
# Input
# mean_returns: vector of annualized returns
# cov_returns: covariance matrix of annualized returns
# rf: risk-free rate
# Returns
# Vector of weights for global optimal portfolio (maximum sharpe ratio)

global_optimal_portfolio <- function(mean_returns, cov_returns, rf=0) {
  mean_returns_less_rf = mean_returns - rf
  one_vec = rep(1, length(mean_returns))
  cov_returns_inv = solve(cov_returns)
  top.mat = cov_returns_inv%*%mean_returns_less_rf
  bot.val = as.numeric(t(one_vec)%*%top.mat)
  op = top.mat[,1]/bot.val
  op_df = t(op) %>% as.data.frame()
  return(op_df)
}

# Generate random portfolio splits for the tickers entered
# sampling from dirichlet distribution
# Input
# tickers: vector of ASX tickers, of length t
# n: number of random splits to generate
# thres: maximum weight for any ticker
# Returns
# Array of random splits, with n rows and t columns
random_splits <- function(tickers, n=100, thres = 0.55) {
  random_wts <- MCMCpack::rdirichlet(n=n, rep(1, length(tickers)))
  colnames(random_wts) = tickers
  rounded_wts <- round(random_wts, 2)

  # Discard any that don't add to 1 exactly
  out <- rounded_wts[rowSums(rounded_wts) == 1,] %>%
    unique() %>% 
    as_tibble()

  in_thres <- filter_all(out, all_vars(. < thres)) %>% 
    as.matrix()

  return(in_thres)
}

# Compute risk values for a candidate portfolio (sd of returns) 
# Input
# cov_matrix: covariance matrix for portfolio
# wts: portfolio split vector for a given sample
# Returns
# Risk value for the given portfolio split
compute_risk <- function(cov_returns, wts) {
  return(sqrt(t(wts) %*% (cov_returns %*% wts)))
}

# Combine sampled portfolios into data frame and calculate sharpe ratio
# Input
# splits: array of portfolio splits with tickers as colnames
# returns: array of annual returns for each potential portfolio
# risk: array of annual risk for each potential portfolio
# Returns
# Risk value for the given portfolio split
portfolios_summary_df <- function(splits, returns, risk) {
  
  a <- data_frame(
    return = returns %>% as.vector(),
    risk = risk %>% as.vector()
  ) %>%
    mutate(sharpe_ratio = return/risk) %>%
    bind_cols(splits %>% as.data.frame())
  
  names(a) <- gsub(".AX", "", names(a))
  
  return(a)
}



# Summarise the sampled portfolios into increments that trace the efficient frontier
# Input
# risk: a vector of risk values
# n_increments: number of desired increments along the efficient frontier
# Returns
# Corresponding increments for the risk values vector
round_risk_into_increments <- function(risk, n_increments = 100) {
  # Calculate risk increment size
  min_risk <- min(risk) %>% signif(2)
  max_risk <- max(risk) %>% signif(2)
  incr <- (max_risk - min_risk)/n_increments %>% signif(3)
  
  # Get increments
  risk_increments <- seq(from = min_risk, to = max_risk, by = incr)
  
  # Assign risk values to nearest increment
  nearest_increment = function(x, values) {
    return(values[which.min(abs(values - x))])
  }
  risk_nearest_increment <- risk %>% sapply(
    FUN = nearest_increment,
    values = risk_increments
  )
  
  return(risk_nearest_increment)
}



# Plotly plot of the efficient frontier, with sampled portfolios along it
# Input
# portfolios_df: dataframe containing each portfolios return, risk and sharpe ratio,
# as returned from portfolios_summary_df()
# Returns
# ggplot containing points that trace the efficient frontier
plot_efficient_frontier <- function(portfolios_df, size, alpha){
  
  # Get risk increments
  portfolios_df$risk_increments <- round_risk_into_increments(
    portfolios_df$risk, n_increments = 25
    )
  
  # Create plot data
  plot_data <- portfolios_df %>% 
    group_by(risk_increments) %>%
    filter(sharpe_ratio == max(sharpe_ratio)) %>%
    ungroup() %>%
    mutate(optimal_portfolio = sharpe_ratio == max(sharpe_ratio)) %>%
    arrange(-return)
  
  # Isolate the stock splits
  stocks_splits <- plot_data %>% select(-c("return", "risk", "sharpe_ratio", "risk_increments", "optimal_portfolio"))
  names(stocks_splits) <- gsub('.AX', '', names(stocks_splits))
  stocks_splits[] <- Map(paste, names(stocks_splits), stocks_splits, sep = ':')
  plot_data$split <- paste(
    "</br></br>", 
    apply(stocks_splits, 1, paste, collapse = " </br> ")
    )
  
  # Plot
  p <- plot_data %>%
    mutate(optimal_portfolio = ifelse(optimal_portfolio, "Optimal", "Alternate")) %>% 
    ggplot(aes(x=risk_increments, y=return, col=sharpe_ratio, label = split)) +
    geom_point(size=size, alpha=alpha, aes(shape=optimal_portfolio)) +
    # geom_path() +
    scale_color_gradient(low = 'orange', high = 'purple', name = 'Sharpe Ratio') +
    scale_shape_manual(values = c(1, 50), name = 'Optimal portfolio') +
    xlab('Risk') +
    ylab('Return') +
    guides(shape = "none")
  
  # Make plotly
  p_plotly <- ggplotly(p, tooltip = c("split")) %>% 
    layout(
      xaxis = list(fixedrange=TRUE),
      yaxis = list(fixedrange=TRUE),
      legend = list(orientation = "h",
                    xanchor = "center",
                    x = 0,
                    y = -0.25)
    )
  return(p_plotly)
}


# Plot all sampled portfolios with their risk/return
# Input
# portfolios_df: dataframe containing each portfolios return, risk and sharpe ratio,
# as returned from portfolios_summary_df()
# Returns
# ggplot containing a scatterplot of portfolios return vs. risk, coloured by sharpe ratio
plot_sampled_portfolios <- function(portfolios_df, size, alpha){
  return(
    portfolios_df %>% 
      ggplot(aes(x=risk, y=return, col=sharpe_ratio)) +
      geom_point(size=size, alpha=alpha) +
      scale_color_gradient(low = 'orange', high = 'purple', name = 'Sharpe ratio') +
      ggtitle(label = "Risk, return and sharpe ratio for each sampled portfolio") +
      xlab('Risk') +
      ylab('Return')
  )
}


# Plot the correlations of stock returns
# Input
# daily_returns_df: dataframe containing daily returns for all selected stocks
# as returned from daily_returns()
# Returns
# ggplot containing a heatmap and labels of correlations for each pair of stocks
plot_stock_return_correlations <- function(daily_returns_df){
  p <- daily_returns_df %>% 
    cor() %>% 
    round(2) %>% 
    as.data.frame() %>% 
    rownames_to_column('Ticker1') %>% 
    pivot_longer(cols = ends_with('.AX'), names_to = 'Ticker2', values_to = 'cor') %>%
    mutate(Ticker1 = str_replace(Ticker1, pattern = ".AX", replace = ""),
           Ticker2 = str_replace(Ticker2, pattern = ".AX", replace = "")) %>% 
    ggplot(aes(x = reorder(Ticker1, desc(Ticker1)), y = Ticker2, label = cor)) + 
    geom_tile(aes(fill = cor)) + 
    scale_fill_gradient2(
      name = 'Correlation',
      low = "orange",
      mid = "white",
      high = "purple",
      midpoint = 0,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill"
    ) +
    geom_label() +
    xlab('Ticker') + 
    ylab('Ticker') + 
    theme(text = element_text(size = 16))
  return(p)
}



daily_returns_with_date <- function(prices_df = df) {
  tickers = unique(prices_df$ticker)
  
  daily_returns <- prices_df %>%
    group_by(ticker) %>% 
    arrange(date) %>% 
    mutate(ret = (((price / lag(price)))))
  
  daily_returns$ticker <- gsub(".AX", "", as.character(daily_returns$ticker))
  
  daily_returns <- daily_returns %>% drop_na()
  
  
  daily_returns <- daily_returns %>%
    rename(Ticker = ticker) %>%
    group_by(Ticker) %>%
    arrange(date) %>% 
    mutate(cumret = ((cumprod(ret) - 1) * 100))
  
  
  daily_data <- daily_returns %>% select(-c("price"))
  xlim1 <- min(daily_data$date)
  xlim2 <- max(daily_data$date)
  
  p <- daily_data %>% ggplot(aes(x = date, y = cumret, color = Ticker, group = Ticker, 
                                 text = paste0( 'Ticker: ', Ticker,
                                               '<br>Date: ', as.Date(date),
                                               '<br>Cumulative Return: ', round(cumret,2), '%'))) + geom_line() +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    coord_x_date(xlim = c(xlim1, xlim2)) +
    labs(title = "Cumulative Returns", y = "Return", x = "Date") +
    scale_color_brewer(palette = "PuOr") + theme(legend.key=element_blank()) +
    theme_bw()
  
  r_plotly <- ggplotly(p, tooltip = c("text")) %>%
    layout(
      yaxis = list(fixedrange=TRUE),
      xaxis = list(fixedrange=TRUE),
      legend = list(orientation = "h",
                    xanchor = "left",
                    x = 0,
                    y = -0.25)
      )
  return(r_plotly)
}
