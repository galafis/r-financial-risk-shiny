# Test suite for portfolio functions
library(testthat)
library(rfinancialriskshiny)

context("Portfolio Functions")

# Sample data for testing
set.seed(123)
returns <- matrix(rnorm(500, mean = 0.001, sd = 0.02), ncol = 5)
colnames(returns) <- paste0("Asset", 1:5)
weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)

test_that("calculate_portfolio_returns works correctly", {
  portfolio_returns <- calculate_portfolio_returns(returns, weights)
  
  expect_type(portfolio_returns, "double")
  expect_length(portfolio_returns, nrow(returns))
  
  # Test with weights that don't sum to 1
  weights_not_sum_1 <- c(0.3, 0.3, 0.3, 0.3, 0.3)
  expect_warning(calculate_portfolio_returns(returns, weights_not_sum_1))
  
  # Test with incorrect number of weights
  weights_wrong_length <- c(0.2, 0.2, 0.2, 0.2)
  expect_error(calculate_portfolio_returns(returns, weights_wrong_length))
})

test_that("calculate_portfolio_statistics works correctly", {
  stats <- calculate_portfolio_statistics(returns, weights)
  
  expect_type(stats, "list")
  expect_true(all(c("mean_return", "volatility", "sharpe_ratio", "var_95", "es_95", "max_drawdown") %in% names(stats)))
  
  # Check types of statistics
  expect_type(stats$mean_return, "double")
  expect_type(stats$volatility, "double")
  expect_type(stats$sharpe_ratio, "double")
  expect_type(stats$var_95, "double")
  expect_type(stats$es_95, "double")
  expect_type(stats$max_drawdown, "double")
  
  # Check values are reasonable
  expect_gt(stats$volatility, 0)
  expect_lt(stats$var_95, 0)
  expect_lt(stats$es_95, 0)
  expect_lt(stats$max_drawdown, 0)
  
  # ES should be more extreme than VaR
  expect_lt(stats$es_95, stats$var_95)
})

test_that("optimize_portfolio_mean_variance works correctly", {
  optimized <- optimize_portfolio_mean_variance(returns)
  
  expect_type(optimized, "list")
  expect_true(all(c("weights", "objective_value", "statistics") %in% names(optimized)))
  
  # Check weights
  expect_length(optimized$weights, ncol(returns))
  expect_equal(names(optimized$weights), colnames(returns))
  expect_equal(sum(optimized$weights), 1, tolerance = 1e-6)
  expect_true(all(optimized$weights >= 0))
  
  # Check statistics
  expect_type(optimized$statistics, "list")
  expect_true(all(c("mean_return", "volatility", "sharpe_ratio") %in% names(optimized$statistics)))
  
  # Test with target return
  mean_returns <- colMeans(returns)
  target_return <- mean(mean_returns)
  optimized_target <- optimize_portfolio_mean_variance(returns, target_return = target_return)
  expect_equal(optimized_target$statistics$mean_return, target_return, tolerance = 1e-4)
  
  # Test with constraints
  constraints <- list(max_weight = 0.3)
  optimized_constrained <- optimize_portfolio_mean_variance(returns, constraints = constraints)
  expect_true(all(optimized_constrained$weights <= 0.3 + 1e-6))
})

test_that("generate_efficient_frontier works correctly", {
  frontier <- generate_efficient_frontier(returns, n_portfolios = 10)
  
  expect_s3_class(frontier, "data.frame")
  expect_equal(nrow(frontier), 10)
  
  # Check columns
  expect_true(all(c("target_return", "volatility", "sharpe_ratio") %in% names(frontier)))
  
  # Check asset weight columns
  asset_cols <- paste0("Asset", 1:5)
  expect_true(all(asset_cols %in% names(frontier)))
  
  # Check that weights sum to 1 for each portfolio
  weights_sum <- rowSums(frontier[, asset_cols])
  expect_equal(weights_sum, rep(1, 10), tolerance = 1e-6)
  
  # Check that volatility increases with return
  expect_true(all(diff(frontier$volatility) >= 0) || all(diff(frontier$volatility) <= 0))
})

