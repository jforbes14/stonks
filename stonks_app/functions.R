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
get_prices_for_ticker <- function(ticker, from="1900-01-01", to=Sys.Date()) {
  x <- suppressWarnings(
    getSymbols(ticker,
                    src = 'yahoo',
                    from="2015-01-01", 
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
# Input
# tickers: vector of ASX tickers, of length t
# n: number of random splits to generate
# Returns
# Array of random splits, with n rows and t columns

random_splits <- function(tickers, n=100) {
  random_wts <- matrix(runif(n*length(tickers)), ncol=length(tickers))
  colnames(random_wts) = tickers
  standardised_wts <- random_wts / rowSums(random_wts)
  rounded_wts <- round(standardised_wts, 2)
  
  # Discard any that don't add to 1 exactly
  out <- rounded_wts[rowSums(rounded_wts) == 1,] %>% 
    unique()
  
  return(out)
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
  return(
    data_frame(
      return = returns %>% as.vector(),
      risk = risk %>% as.vector()
    ) %>%
      mutate(sharpe_ratio = return/risk) %>%
      bind_cols(splits %>% as.data.frame())
  )
}

# Plot the sampled portfolios with their risk/return
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
  return(
    daily_returns_df %>% 
      cor() %>% 
      round(2) %>% 
      as.data.frame() %>% 
      rownames_to_column('Ticker1') %>% 
      pivot_longer(cols = ends_with('.AX'), names_to = 'Ticker2', values_to = 'cor') %>% 
      ggplot(aes(x = Ticker1, y = Ticker2, label = cor)) + 
      geom_tile(aes(fill = cor)) + 
      scale_fill_gradient(name = 'Correlation') +
      geom_label() +
      xlab('Ticker') + ylab('Ticker') + ggtitle('Correlations between stocks')
  )
}
