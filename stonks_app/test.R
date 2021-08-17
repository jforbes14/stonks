source("stonks_app/functions.R")

# Test code
tickers_raw_test = c('ANZ', 'NAB')  # c('A200.AX', 'NDQ', 'BHP', 'PBH') #, 'ASIA', 'ANZ', 'NAB')
tickers_test = add_AX_to_tickers(tickers_raw_test)

df_test <- get_prices_for_all_tickers(tickers_test)
df_test %>% 
  group_by(ticker) %>% 
  top_n(1, date)
daily_test <- daily_returns(df_test)
mr_test <- mean_returns(daily_test)
cv_test <- cov_returns(daily_test)
mvp_test <- global_minimum_variance_portfolio(mr_test, cv_test)
op_test <- global_optimal_portfolio(mr_test, cv_test)

print(mvp_test)
print(op_test)