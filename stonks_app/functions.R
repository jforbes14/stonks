#
# Defining functions for the stonks app
#
library(tidyquant)
library(tidyverse)
options("getSymbols.warning4.0"=FALSE)

# Test function
# Input
# Returns
# "hello world" text
hello <- function() {
  return("hello world")
}

# Add .AX prefix to stocks
# Input
# tickers: vector of ASX tickers
# Returns
# Vector of ASX tickers with .AX suffix
add_AX_to_tickers <- function(tickers) {
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
get_prices_for_ticker <- function(ticker, from="1900-01-01", to=Sys.Date()) {
  x <- suppressWarnings(
    getSymbols(ticker,
                    src = 'yahoo',
                    from="1900-01-01", 
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
get_prices_for_all_tickers <- function(tickers, from="1900-01-01", to=Sys.Date()) {
  for (ticker in tickers) {
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
