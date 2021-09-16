library(tidyverse)
library(tidyquant)
library(readxl)
library(xts)
library(tsibble)

asx_stocks <- read_csv('asx_cons_cleaned.csv')
stocks_vect <- as.vector(asx_stocks[[1]])
asx_etf <- read_csv('ETF_data_cleaned.csv')
etf_vect <- as.vector(asx_etf[[1]])
codes_vect <- c(stocks_vect, etf_vect)

input_validation <- function(stonks_vect=NULL){
  stonks_vect <- unique(toupper(stonks_vect))
  stonks_vect <<- stonks_vect
  for (ticker in stonks_vect) {
    if (ticker %in% codes_vect == TRUE){
      print(paste(ticker, 'on the ASX'))
    }else{
      print(paste(ticker, 'not on the ASX'))
      break
    }
  }
}

dirty_codes <- c('BHP', 'CBA', 'TCL', 'ASIA', 'CUMROCKET', 'NAB')
clean_codes <- c('BHP', 'TCL', 'Z1P', 'TLS', 'CSL', 'RFG', 'CBA', 'PBH')
forbes_codes <- c('A200', 'ASIA', 'F100')
input_validation(c('TEK', 'TOP', 'SYD', 'NDQ'))
input_validation(dirty_codes) 
input_validation(clean_codes) 
suffix <- '.AX'

# for (ticker in dirty_codes){
#   print(paste(ticker, suffix, sep = ""))
# }

# getSymbols("BHP.AX", from = '2020-07-01',
#            to = Sys.Date(),
#            auto.assign = FALSE)

# for (ticker in dirty_codes)
#   getSymbols(paste(ticker, suffix, sep = ""), from = '2020-07-01',
#              to = Sys.Date(),
#              auto.assign = TRUE) %>% as.data.frame()
#
# stock_prices <- lapply(paste(forbes_codes, suffix, sep = ""),
#                        getSymbols, auto.assign=FALSE,
#                        from = '2015-01-01',
#                        end = Sys.Date())


prices <- lapply(paste(clean_codes, suffix, sep = ""), 
                 getSymbols, auto.assign=FALSE,
                 from = '2015-01-01',
                 end = Sys.Date())


for (ticker in prices){
  print(nrow(ticker))
}


stocks_adjusted <- lapply(prices, function(x) x[,6])
stocks_adjusted <- lapply(stocks_adjusted, function(x) data.frame(x) %>% rownames_to_column())
combined_df <- Reduce(function(...) merge(..., by = c('rowname')), stocks_adjusted)
combined_df <- combined_df %>% rename(Dates = rowname)
combined_df$Dates <- as.Date(combined_df$Dates, format = "%Y-%m-%d")
combined_df <- combined_df %>% as_tibble()
print(head(combined_df))
stonks_vect_1 <- c("Dates", stonks_vect)
colnames(combined_df) <- stonks_vect_1
daily_ret <- ((combined_df[,-1] - lag(combined_df[,-1], 1)) / lag(combined_df[,-1], 1)) %>% drop_na()
mean_daily_ret <- colMeans(daily_ret)
mean_annual_ret <- (mean_daily_ret + 1)^252 -1 
cov_ret <- cov(daily_ret) * 252


# stocks_adjusted <- lapply(stock_prices, function(x) x[,6])
# stocks_adjusted <- lapply(stocks_adjusted, function(x) data.frame(x) %>% rownames_to_column())
# combined_df <- Reduce(function(...) merge(..., by = c('rowname')), stocks_adjusted)
# combined_df <- combined_df %>% rename(Dates = rowname)
# combined_df$Dates <- as.Date(combined_df$Dates, format = "%Y-%m-%d")
# combined_df <- combined_df %>% as_tibble()
# stonks_vect_1 <- c("Dates", stonks_vect)
# colnames(combined_df) <- stonks_vect+1
# # combined_df %>% mutate(combined_df, Return = (combined_df[,-1] - lag(combined_df[,-1], 1)) / lag(combined_df[,-1], 1))
# daily_ret <- ((combined_df[,-1] - lag(combined_df[,-1], 1)) / lag(combined_df[,-1], 1)) %>% drop_na()
# mean_daily_ret <- colMeans(daily_ret)
# mean_annual_ret <- (mean_daily_ret + 1)^252 -1
# cov_ret <- cov(daily_ret) * 252

print(cor(daily_ret))

stock_mean_ret <- colMeans(daily_ret)


  # top.mat <- cbind(2 * cov_ret, rep(1, ncol(cov_ret)))
  # bot.vec <- c(rep(1, ncol(cov_ret)), 0)
  # am.mat <- rbind(top.mat, bot.vec)
  # b.vec <- c(rep(0, ncol(cov_ret)), 1)
  # z.m.mat <- solve(am.mat) %*% b.vec
  # m.vec <- z.m.mat[1:ncol(daily_ret), 1]
  # 

minimum_variance_portfolio <- min_var_port(cov_ret)

random_splits <- function(tickers, n=1000000) {
  random_wts <- matrix(runif(n*length(tickers)), ncol=length(tickers))
  colnames(random_wts) = tickers
  standardised_wts <- random_wts / rowSums(random_wts)
  rounded_wts <- round(standardised_wts, 2)
  
  # Discard any that don't add to 1 exactly
  out <- rounded_wts[rowSums(rounded_wts) == 1,] %>% 
    unique()
  
  return(out)
}

candidate_weights <- random_splits(clean_codes)

port_returns <- (candidate_weights %*% stock_mean_ret + 1)^252 - 1

compute_risk <- function(weights){
  return(sqrt(t(weights) %*% (cov_ret %*% weights)))
}

port_risk <- candidate_weights %>% apply(1, compute_risk)

df <- data_frame(return = port_returns %>% as.vector(),
                  risk = port_risk %>% as.vector()
                  ) %>% mutate(sharpe_ratio = return / risk) %>%
  bind_cols(candidate_weights %>% as.data.frame())

top_50 <- df %>% arrange(desc(sharpe_ratio)) %>% head(50)

df %>% 
  arrange(desc(sharpe_ratio)) %>% 
  head()

df %>%
  ggplot(aes(x=risk, y=return, col=sharpe_ratio)) +
  geom_point(size=0.25) +
  scale_color_gradient(low = 'blue', high = 'red') +
  # geom_curve(aes(x = .2, y = .2, xend = .5, yend = .4,  col = sharpe_ratio ), top_50)
  geom_text(aes(x=risk, y=return, label = 'Optimal Portfolio'), top_50[1,], col = 'black', size = 3) +
  labs(y="Portfolio Return", x = "Portfolio Risk", colour = 'Sharpe Ratio')



