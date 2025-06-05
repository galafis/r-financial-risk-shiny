#' Portfolio Optimization Functions
#'
#' @description Functions for portfolio optimization and analysis
#' @author Gabriel Demetrios Lafis
#' @importFrom stats cov cor sd var
#' @importFrom utils head tail
#' @importFrom quadprog solve.QP
#' @importFrom PerformanceAnalytics VaR ES
#' @importFrom xts as.xts
#' @importFrom zoo index
#'
#' @name portfolio_functions
NULL

#' Calculate portfolio returns
#'
#' @param returns A matrix or data frame of asset returns
#' @param weights A numeric vector of portfolio weights
#'
#' @return A numeric vector of portfolio returns
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- matrix(rnorm(500), ncol = 5)
#' weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#' portfolio_returns <- calculate_portfolio_returns(returns, weights)
#' }
calculate_portfolio_returns <- function(returns, weights) {
  if (length(weights) != ncol(returns)) {
    stop("Number of weights must match number of assets")
  }
  
  if (abs(sum(weights) - 1) > 1e-6) {
    warning("Weights do not sum to 1, normalizing")
    weights <- weights / sum(weights)
  }
  
  # Calculate portfolio returns
  portfolio_returns <- returns %*% weights
  return(as.vector(portfolio_returns))
}

#' Calculate portfolio statistics
#'
#' @param returns A matrix or data frame of asset returns
#' @param weights A numeric vector of portfolio weights
#' @param risk_free_rate The risk-free rate (default: 0)
#'
#' @return A list containing portfolio statistics
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- matrix(rnorm(500), ncol = 5)
#' weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#' stats <- calculate_portfolio_statistics(returns, weights)
#' }
calculate_portfolio_statistics <- function(returns, weights, risk_free_rate = 0) {
  portfolio_returns <- calculate_portfolio_returns(returns, weights)
  
  # Calculate statistics
  mean_return <- mean(portfolio_returns)
  volatility <- sd(portfolio_returns)
  sharpe_ratio <- (mean_return - risk_free_rate) / volatility
  
  # Calculate downside risk measures
  returns_xts <- as.xts(portfolio_returns, order.by = as.Date(index(returns)))
  var_95 <- as.numeric(VaR(returns_xts, p = 0.95, method = "historical"))
  es_95 <- as.numeric(ES(returns_xts, p = 0.95, method = "historical"))
  
  # Calculate maximum drawdown
  cumulative_returns <- cumprod(1 + portfolio_returns)
  running_max <- cummax(cumulative_returns)
  drawdowns <- cumulative_returns / running_max - 1
  max_drawdown <- min(drawdowns)
  
  return(list(
    mean_return = mean_return,
    volatility = volatility,
    sharpe_ratio = sharpe_ratio,
    var_95 = var_95,
    es_95 = es_95,
    max_drawdown = max_drawdown
  ))
}

#' Optimize portfolio weights using mean-variance optimization
#'
#' @param returns A matrix or data frame of asset returns
#' @param target_return The target portfolio return (optional)
#' @param risk_free_rate The risk-free rate (default: 0)
#' @param constraints A list of constraints (optional)
#'
#' @return A list containing optimized weights and portfolio statistics
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- matrix(rnorm(500), ncol = 5)
#' optimized <- optimize_portfolio_mean_variance(returns)
#' }
optimize_portfolio_mean_variance <- function(returns, target_return = NULL, 
                                           risk_free_rate = 0, constraints = NULL) {
  n_assets <- ncol(returns)
  
  # Calculate mean returns and covariance matrix
  mean_returns <- colMeans(returns)
  cov_matrix <- cov(returns)
  
  # Set up quadratic programming problem
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, n_assets)
  
  # Constraints: weights sum to 1
  Amat <- cbind(rep(1, n_assets), diag(n_assets))
  bvec <- c(1, rep(0, n_assets))  # Sum to 1 and non-negative weights
  
  # Add target return constraint if specified
  if (!is.null(target_return)) {
    Amat <- cbind(Amat, mean_returns)
    bvec <- c(bvec, target_return)
  }
  
  # Add custom constraints if specified
  if (!is.null(constraints)) {
    if ("max_weight" %in% names(constraints)) {
      max_weight <- constraints$max_weight
      for (i in 1:n_assets) {
        constraint_vec <- rep(0, n_assets)
        constraint_vec[i] <- 1
        Amat <- cbind(Amat, constraint_vec)
        bvec <- c(bvec, max_weight)
      }
    }
  }
  
  # Solve quadratic programming problem
  solution <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  
  # Extract weights
  weights <- solution$solution
  names(weights) <- colnames(returns)
  
  # Calculate portfolio statistics
  stats <- calculate_portfolio_statistics(returns, weights, risk_free_rate)
  
  return(list(
    weights = weights,
    objective_value = solution$value,
    statistics = stats
  ))
}

#' Generate efficient frontier
#'
#' @param returns A matrix or data frame of asset returns
#' @param n_portfolios Number of portfolios to generate (default: 50)
#' @param risk_free_rate The risk-free rate (default: 0)
#'
#' @return A data frame containing portfolio weights and statistics along the efficient frontier
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- matrix(rnorm(500), ncol = 5)
#' frontier <- generate_efficient_frontier(returns)
#' }
generate_efficient_frontier <- function(returns, n_portfolios = 50, risk_free_rate = 0) {
  n_assets <- ncol(returns)
  
  # Calculate mean returns and covariance matrix
  mean_returns <- colMeans(returns)
  cov_matrix <- cov(returns)
  
  # Find minimum and maximum returns
  min_return <- min(mean_returns)
  max_return <- max(mean_returns)
  
  # Generate target returns
  target_returns <- seq(min_return, max_return, length.out = n_portfolios)
  
  # Initialize results
  results <- data.frame(
    target_return = numeric(n_portfolios),
    volatility = numeric(n_portfolios),
    sharpe_ratio = numeric(n_portfolios)
  )
  
  # Initialize weights matrix
  weights_matrix <- matrix(0, nrow = n_portfolios, ncol = n_assets)
  colnames(weights_matrix) <- colnames(returns)
  
  # Generate portfolios
  for (i in 1:n_portfolios) {
    target_return <- target_returns[i]
    
    # Optimize portfolio
    optimized <- optimize_portfolio_mean_variance(returns, target_return, risk_free_rate)
    
    # Store results
    results$target_return[i] <- target_return
    results$volatility[i] <- optimized$statistics$volatility
    results$sharpe_ratio[i] <- optimized$statistics$sharpe_ratio
    
    # Store weights
    weights_matrix[i, ] <- optimized$weights
  }
  
  # Combine results and weights
  results <- cbind(results, weights_matrix)
  
  return(results)
}

