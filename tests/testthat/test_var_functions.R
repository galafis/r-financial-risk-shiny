# Test suite for VaR functions
library(testthat)
library(rfinancialriskshiny)

context("Value at Risk Functions")

# Sample data for testing
set.seed(123)
returns <- rnorm(1000, mean = 0.001, sd = 0.02)
returns_matrix <- matrix(rnorm(5000, mean = 0.001, sd = 0.02), ncol = 5)
colnames(returns_matrix) <- paste0("Asset", 1:5)

test_that("calculate_var works correctly with different methods", {
  # Test parametric method
  var_parametric <- calculate_var(returns, p = 0.95, method = "parametric")
  expect_type(var_parametric, "double")
  expect_lt(var_parametric, 0)  # VaR should be negative for typical returns
  
  # Test historical method
  var_historical <- calculate_var(returns, p = 0.95, method = "historical")
  expect_type(var_historical, "double")
  expect_lt(var_historical, 0)
  
  # Test monte carlo method
  var_monte_carlo <- calculate_var(returns, p = 0.95, method = "monte_carlo")
  expect_type(var_monte_carlo, "double")
  expect_lt(var_monte_carlo, 0)
  
  # Test with matrix input
  var_matrix <- calculate_var(returns_matrix, p = 0.95, method = "historical")
  expect_length(var_matrix, ncol(returns_matrix))
  expect_true(all(var_matrix < 0))
})

test_that("calculate_expected_shortfall works correctly with different methods", {
  # Test parametric method
  es_parametric <- calculate_expected_shortfall(returns, p = 0.95, method = "parametric")
  expect_type(es_parametric, "double")
  expect_lt(es_parametric, 0)  # ES should be negative for typical returns
  
  # Test historical method
  es_historical <- calculate_expected_shortfall(returns, p = 0.95, method = "historical")
  expect_type(es_historical, "double")
  expect_lt(es_historical, 0)
  
  # Test monte carlo method
  es_monte_carlo <- calculate_expected_shortfall(returns, p = 0.95, method = "monte_carlo")
  expect_type(es_monte_carlo, "double")
  expect_lt(es_monte_carlo, 0)
  
  # Test with matrix input
  es_matrix <- calculate_expected_shortfall(returns_matrix, p = 0.95, method = "historical")
  expect_length(es_matrix, ncol(returns_matrix))
  expect_true(all(es_matrix < 0))
  
  # ES should be more extreme than VaR
  var_historical <- calculate_var(returns, p = 0.95, method = "historical")
  es_historical <- calculate_expected_shortfall(returns, p = 0.95, method = "historical")
  expect_lt(es_historical, var_historical)
})

test_that("calculate_stress_var works correctly", {
  # Test with default stress factor
  var_normal <- calculate_var(returns, p = 0.95, method = "parametric")
  stress_var <- calculate_stress_var(returns, stress_factor = 1.5, p = 0.95)
  expect_equal(stress_var, var_normal * 1.5)
  
  # Test with custom stress factor
  stress_var_2 <- calculate_stress_var(returns, stress_factor = 2, p = 0.95)
  expect_equal(stress_var_2, var_normal * 2)
})

test_that("calculate_component_var works correctly", {
  # Test with equal weights
  weights <- rep(1/5, 5)
  component_var <- calculate_component_var(returns_matrix, weights, p = 0.95)
  expect_length(component_var, ncol(returns_matrix))
  expect_equal(names(component_var), colnames(returns_matrix))
  
  # Sum of component VaRs should approximately equal portfolio VaR
  portfolio_returns <- returns_matrix %*% weights
  portfolio_var <- calculate_var(portfolio_returns, p = 0.95, method = "parametric")
  expect_equal(sum(component_var), portfolio_var[1], tolerance = 1e-6)
})

test_that("calculate_maximum_drawdown works correctly", {
  # Create a series with a known drawdown
  custom_returns <- c(0.01, 0.02, -0.03, -0.05, 0.02, 0.03, 0.01)
  drawdown_result <- calculate_maximum_drawdown(custom_returns)
  
  expect_type(drawdown_result, "list")
  expect_true("max_drawdown" %in% names(drawdown_result))
  expect_true("peak_index" %in% names(drawdown_result))
  expect_true("trough_index" %in% names(drawdown_result))
  expect_true("drawdowns" %in% names(drawdown_result))
  
  # Check max drawdown value
  expect_lt(drawdown_result$max_drawdown, 0)
  
  # Check that peak comes before trough
  expect_lt(drawdown_result$peak_index, drawdown_result$trough_index)
})

