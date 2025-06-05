#' Risk Analysis Functions
#'
#' @description Functions for financial risk analysis
#' @author Gabriel Demetrios Lafis
#' @importFrom stats qnorm sd quantile
#' @importFrom utils head tail
#' @importFrom PerformanceAnalytics VaR ES
#' @importFrom xts as.xts
#' @importFrom zoo index
#'
#' @name risk_functions
NULL

#' Calculate Value at Risk (VaR)
#'
#' @param returns A numeric vector, matrix, or data frame of returns
#' @param p Confidence level (default: 0.95)
#' @param method Method for calculating VaR ("parametric", "historical", or "monte_carlo")
#' @param n_simulations Number of simulations for Monte Carlo method (default: 10000)
#'
#' @return A numeric vector of VaR values
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- rnorm(1000)
#' var_95 <- calculate_var(returns)
#' }
calculate_var <- function(returns, p = 0.95, method = "historical", n_simulations = 10000) {
  if (is.matrix(returns) || is.data.frame(returns)) {
    # If returns is a matrix or data frame, apply to each column
    result <- apply(returns, 2, calculate_var, p = p, method = method, n_simulations = n_simulations)
    return(result)
  }
  
  # Convert to numeric vector
  returns <- as.numeric(returns)
  
  if (method == "parametric") {
    # Parametric VaR (assuming normal distribution)
    mean_return <- mean(returns)
    sd_return <- sd(returns)
    var_value <- mean_return + qnorm(1 - p) * sd_return
    return(var_value)
  } else if (method == "historical") {
    # Historical VaR
    var_value <- quantile(returns, 1 - p)
    return(as.numeric(var_value))
  } else if (method == "monte_carlo") {
    # Monte Carlo VaR
    mean_return <- mean(returns)
    sd_return <- sd(returns)
    simulated_returns <- rnorm(n_simulations, mean = mean_return, sd = sd_return)
    var_value <- quantile(simulated_returns, 1 - p)
    return(as.numeric(var_value))
  } else {
    stop("Invalid method. Choose 'parametric', 'historical', or 'monte_carlo'.")
  }
}

#' Calculate Expected Shortfall (ES) / Conditional VaR (CVaR)
#'
#' @param returns A numeric vector, matrix, or data frame of returns
#' @param p Confidence level (default: 0.95)
#' @param method Method for calculating ES ("parametric", "historical", or "monte_carlo")
#' @param n_simulations Number of simulations for Monte Carlo method (default: 10000)
#'
#' @return A numeric vector of ES values
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- rnorm(1000)
#' es_95 <- calculate_expected_shortfall(returns)
#' }
calculate_expected_shortfall <- function(returns, p = 0.95, method = "historical", n_simulations = 10000) {
  if (is.matrix(returns) || is.data.frame(returns)) {
    # If returns is a matrix or data frame, apply to each column
    result <- apply(returns, 2, calculate_expected_shortfall, p = p, method = method, n_simulations = n_simulations)
    return(result)
  }
  
  # Convert to numeric vector
  returns <- as.numeric(returns)
  
  # Calculate VaR
  var_value <- calculate_var(returns, p = p, method = method, n_simulations = n_simulations)
  
  if (method == "parametric") {
    # Parametric ES (assuming normal distribution)
    mean_return <- mean(returns)
    sd_return <- sd(returns)
    z_score <- qnorm(1 - p)
    es_value <- mean_return + sd_return * dnorm(z_score) / (1 - p)
    return(es_value)
  } else if (method == "historical") {
    # Historical ES
    threshold <- returns <= var_value
    if (sum(threshold) > 0) {
      es_value <- mean(returns[threshold])
      return(es_value)
    } else {
      return(var_value)
    }
  } else if (method == "monte_carlo") {
    # Monte Carlo ES
    mean_return <- mean(returns)
    sd_return <- sd(returns)
    simulated_returns <- rnorm(n_simulations, mean = mean_return, sd = sd_return)
    threshold <- simulated_returns <= var_value
    if (sum(threshold) > 0) {
      es_value <- mean(simulated_returns[threshold])
      return(es_value)
    } else {
      return(var_value)
    }
  } else {
    stop("Invalid method. Choose 'parametric', 'historical', or 'monte_carlo'.")
  }
}

#' Calculate Stress VaR
#'
#' @param returns A numeric vector of returns
#' @param stress_factor Stress factor to apply (default: 1.5)
#' @param p Confidence level (default: 0.95)
#'
#' @return A numeric value representing the stressed VaR
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- rnorm(1000)
#' stress_var <- calculate_stress_var(returns)
#' }
calculate_stress_var <- function(returns, stress_factor = 1.5, p = 0.95) {
  # Calculate standard VaR
  var_value <- calculate_var(returns, p = p, method = "parametric")
  
  # Apply stress factor
  stress_var <- var_value * stress_factor
  
  return(stress_var)
}

#' Calculate Component VaR
#'
#' @param returns A matrix or data frame of asset returns
#' @param weights A numeric vector of portfolio weights
#' @param p Confidence level (default: 0.95)
#'
#' @return A numeric vector of component VaR values
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- matrix(rnorm(500), ncol = 5)
#' weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#' component_var <- calculate_component_var(returns, weights)
#' }
calculate_component_var <- function(returns, weights, p = 0.95) {
  if (length(weights) != ncol(returns)) {
    stop("Number of weights must match number of assets")
  }
  
  # Calculate portfolio returns
  portfolio_returns <- returns %*% weights
  
  # Calculate portfolio VaR
  portfolio_var <- calculate_var(portfolio_returns, p = p, method = "parametric")
  
  # Calculate covariance matrix
  cov_matrix <- cov(returns)
  
  # Calculate portfolio volatility
  portfolio_vol <- sqrt(t(weights) %*% cov_matrix %*% weights)
  
  # Calculate marginal VaR
  marginal_var <- (cov_matrix %*% weights) / portfolio_vol * portfolio_var
  
  # Calculate component VaR
  component_var <- weights * marginal_var
  
  # Set names
  names(component_var) <- colnames(returns)
  
  return(component_var)
}

#' Calculate Maximum Drawdown
#'
#' @param returns A numeric vector of returns
#'
#' @return A list containing maximum drawdown and related information
#' @export
#'
#' @examples
#' \dontrun{
#' returns <- rnorm(1000)
#' drawdown <- calculate_maximum_drawdown(returns)
#' }
calculate_maximum_drawdown <- function(returns) {
  # Calculate cumulative returns
  cumulative_returns <- cumprod(1 + returns)
  
  # Calculate running maximum
  running_max <- cummax(cumulative_returns)
  
  # Calculate drawdowns
  drawdowns <- cumulative_returns / running_max - 1
  
  # Find maximum drawdown
  max_drawdown <- min(drawdowns)
  max_drawdown_index <- which.min(drawdowns)
  
  # Find peak before maximum drawdown
  peak_index <- which.max(cumulative_returns[1:max_drawdown_index])
  
  # Calculate recovery index (if any)
  recovery_index <- NULL
  if (max_drawdown_index < length(cumulative_returns)) {
    recovery_indices <- which(cumulative_returns[(max_drawdown_index + 1):length(cumulative_returns)] >= 
                               cumulative_returns[peak_index])
    if (length(recovery_indices) > 0) {
      recovery_index <- max_drawdown_index + min(recovery_indices)
    }
  }
  
  # Calculate drawdown duration
  drawdown_duration <- max_drawdown_index - peak_index
  
  # Calculate recovery duration
  recovery_duration <- if (!is.null(recovery_index)) recovery_index - max_drawdown_index else NA
  
  return(list(
    max_drawdown = max_drawdown,
    peak_index = peak_index,
    trough_index = max_drawdown_index,
    recovery_index = recovery_index,
    drawdown_duration = drawdown_duration,
    recovery_duration = recovery_duration,
    drawdowns = drawdowns
  ))
}

