#' Value at Risk (VaR) Functions
#'
#' @description A collection of functions for calculating Value at Risk (VaR) using different methods.
#'
#' @importFrom stats quantile sd qnorm rnorm cov cor
#' @importFrom utils head tail
#' @importFrom xts as.xts
#' @importFrom zoo index
#' @importFrom PerformanceAnalytics VaR ES
#' @importFrom dplyr %>% filter select mutate group_by summarise
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs
#'
#' @name var_functions
NULL

#' Calculate Historical Value at Risk (VaR)
#'
#' @param returns A numeric vector, matrix, data frame, or xts object of returns
#' @param p Confidence level (default: 0.95)
#' @param method Method for calculating VaR ("historical", "gaussian", "modified", "cornish_fisher")
#' @param portfolio_value Portfolio value (default: 1000000)
#' @param clean_outliers Whether to clean outliers (default: FALSE)
#' @param winsorize_level Level for winsorizing if cleaning outliers (default: 0.01)
#'
#' @return A list with VaR results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns
#' returns <- rnorm(1000, mean = 0.0005, sd = 0.01)
#'
#' # Calculate VaR using different methods
#' var_hist <- calculate_var(returns, method = "historical")
#' var_gauss <- calculate_var(returns, method = "gaussian")
#' var_cf <- calculate_var(returns, method = "cornish_fisher")
#' }
calculate_var <- function(returns, p = 0.95, method = "historical", portfolio_value = 1000000,
                         clean_outliers = FALSE, winsorize_level = 0.01) {
  # Check if returns is a valid object
  if (!is.numeric(returns) && !is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a numeric vector, matrix, data frame, or xts object")
  }
  
  # Convert to xts if not already
  if (!xts::is.xts(returns)) {
    if (is.matrix(returns) || is.data.frame(returns)) {
      returns <- xts::as.xts(returns)
    } else {
      returns <- xts::as.xts(matrix(returns))
    }
  }
  
  # Clean outliers if requested
  if (clean_outliers) {
    returns <- clean_return_outliers(returns, winsorize_level)
  }
  
  # Calculate VaR based on method
  if (method == "historical") {
    # Historical VaR
    var_value <- -stats::quantile(returns, 1 - p)
  } else if (method == "gaussian") {
    # Parametric VaR (Gaussian)
    mean_return <- mean(returns)
    sd_return <- stats::sd(returns)
    var_value <- -mean_return - stats::qnorm(p) * sd_return
  } else if (method == "modified") {
    # Modified VaR using PerformanceAnalytics
    var_value <- PerformanceAnalytics::VaR(returns, p = p, method = "modified")
  } else if (method == "cornish_fisher") {
    # Cornish-Fisher VaR using PerformanceAnalytics
    var_value <- PerformanceAnalytics::VaR(returns, p = p, method = "modified", modified = TRUE)
  } else {
    stop("Invalid method. Choose from 'historical', 'gaussian', 'modified', or 'cornish_fisher'")
  }
  
  # Calculate VaR in monetary terms
  var_monetary <- as.numeric(var_value) * portfolio_value
  
  # Create result
  result <- list(
    var_pct = as.numeric(var_value),
    var_monetary = var_monetary,
    confidence_level = p,
    method = method,
    portfolio_value = portfolio_value
  )
  
  return(result)
}

#' Calculate Expected Shortfall (ES) / Conditional VaR (CVaR)
#'
#' @param returns A numeric vector, matrix, data frame, or xts object of returns
#' @param p Confidence level (default: 0.95)
#' @param method Method for calculating ES ("historical", "gaussian", "modified")
#' @param portfolio_value Portfolio value (default: 1000000)
#' @param clean_outliers Whether to clean outliers (default: FALSE)
#' @param winsorize_level Level for winsorizing if cleaning outliers (default: 0.01)
#'
#' @return A list with ES results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns
#' returns <- rnorm(1000, mean = 0.0005, sd = 0.01)
#'
#' # Calculate ES using different methods
#' es_hist <- calculate_expected_shortfall(returns, method = "historical")
#' es_gauss <- calculate_expected_shortfall(returns, method = "gaussian")
#' }
calculate_expected_shortfall <- function(returns, p = 0.95, method = "historical", portfolio_value = 1000000,
                                       clean_outliers = FALSE, winsorize_level = 0.01) {
  # Check if returns is a valid object
  if (!is.numeric(returns) && !is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a numeric vector, matrix, data frame, or xts object")
  }
  
  # Convert to xts if not already
  if (!xts::is.xts(returns)) {
    if (is.matrix(returns) || is.data.frame(returns)) {
      returns <- xts::as.xts(returns)
    } else {
      returns <- xts::as.xts(matrix(returns))
    }
  }
  
  # Clean outliers if requested
  if (clean_outliers) {
    returns <- clean_return_outliers(returns, winsorize_level)
  }
  
  # Calculate ES based on method
  if (method == "historical") {
    # Historical ES
    var_value <- -stats::quantile(returns, 1 - p)
    es_value <- -mean(returns[returns <= -var_value])
  } else if (method == "gaussian") {
    # Parametric ES (Gaussian)
    mean_return <- mean(returns)
    sd_return <- stats::sd(returns)
    z_p <- stats::qnorm(1 - p)
    es_value <- -mean_return + sd_return * dnorm(z_p) / (1 - p)
  } else if (method == "modified") {
    # Modified ES using PerformanceAnalytics
    es_value <- PerformanceAnalytics::ES(returns, p = p, method = "modified")
  } else {
    stop("Invalid method. Choose from 'historical', 'gaussian', or 'modified'")
  }
  
  # Calculate ES in monetary terms
  es_monetary <- as.numeric(es_value) * portfolio_value
  
  # Create result
  result <- list(
    es_pct = as.numeric(es_value),
    es_monetary = es_monetary,
    confidence_level = p,
    method = method,
    portfolio_value = portfolio_value
  )
  
  return(result)
}

#' Calculate Component VaR for a portfolio
#'
#' @param returns A matrix or data frame of asset returns
#' @param weights A numeric vector of portfolio weights
#' @param p Confidence level (default: 0.95)
#' @param portfolio_value Portfolio value (default: 1000000)
#'
#' @return A data frame with Component VaR results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns for 5 assets
#' returns <- matrix(rnorm(5000, mean = 0.0005, sd = 0.01), ncol = 5)
#' colnames(returns) <- paste0("Asset", 1:5)
#'
#' # Define portfolio weights
#' weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#'
#' # Calculate Component VaR
#' cvar <- calculate_component_var(returns, weights)
#' }
calculate_component_var <- function(returns, weights, p = 0.95, portfolio_value = 1000000) {
  # Check if returns is a valid object
  if (!is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a matrix, data frame, or xts object")
  }
  
  # Convert to matrix if not already
  if (xts::is.xts(returns)) {
    returns <- as.matrix(returns)
  } else if (is.data.frame(returns)) {
    returns <- as.matrix(returns)
  }
  
  # Check if weights sum to 1
  if (abs(sum(weights) - 1) > 1e-6) {
    warning("Weights do not sum to 1. Normalizing weights.")
    weights <- weights / sum(weights)
  }
  
  # Check if number of weights matches number of assets
  if (length(weights) != ncol(returns)) {
    stop("Number of weights must match number of assets")
  }
  
  # Calculate portfolio returns
  portfolio_returns <- returns %*% weights
  
  # Calculate portfolio VaR
  portfolio_var <- calculate_var(portfolio_returns, p = p, method = "gaussian")$var_pct
  
  # Calculate covariance matrix
  cov_matrix <- stats::cov(returns)
  
  # Calculate portfolio volatility
  portfolio_vol <- sqrt(t(weights) %*% cov_matrix %*% weights)
  
  # Calculate marginal VaR
  marginal_var <- (cov_matrix %*% weights) / portfolio_vol * stats::qnorm(p)
  
  # Calculate component VaR
  component_var <- weights * marginal_var
  
  # Calculate percentage contribution
  pct_contrib <- component_var / sum(component_var) * 100
  
  # Calculate monetary values
  component_var_monetary <- component_var * portfolio_value
  
  # Create result data frame
  result <- data.frame(
    asset = colnames(returns),
    weight = weights,
    marginal_var = as.numeric(marginal_var),
    component_var = as.numeric(component_var),
    pct_contrib = as.numeric(pct_contrib),
    component_var_monetary = as.numeric(component_var_monetary)
  )
  
  return(result)
}

#' Calculate VaR using Monte Carlo simulation
#'
#' @param returns A matrix or data frame of asset returns
#' @param weights A numeric vector of portfolio weights
#' @param p Confidence level (default: 0.95)
#' @param n_sim Number of simulations (default: 10000)
#' @param horizon Forecast horizon in days (default: 1)
#' @param portfolio_value Portfolio value (default: 1000000)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list with Monte Carlo VaR results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns for 5 assets
#' returns <- matrix(rnorm(5000, mean = 0.0005, sd = 0.01), ncol = 5)
#' colnames(returns) <- paste0("Asset", 1:5)
#'
#' # Define portfolio weights
#' weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#'
#' # Calculate Monte Carlo VaR
#' mc_var <- calculate_monte_carlo_var(returns, weights, n_sim = 5000)
#' }
calculate_monte_carlo_var <- function(returns, weights, p = 0.95, n_sim = 10000, horizon = 1,
                                     portfolio_value = 1000000, seed = NULL) {
  # Check if returns is a valid object
  if (!is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a matrix, data frame, or xts object")
  }
  
  # Convert to matrix if not already
  if (xts::is.xts(returns)) {
    returns <- as.matrix(returns)
  } else if (is.data.frame(returns)) {
    returns <- as.matrix(returns)
  }
  
  # Check if weights sum to 1
  if (abs(sum(weights) - 1) > 1e-6) {
    warning("Weights do not sum to 1. Normalizing weights.")
    weights <- weights / sum(weights)
  }
  
  # Check if number of weights matches number of assets
  if (length(weights) != ncol(returns)) {
    stop("Number of weights must match number of assets")
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Calculate mean returns and covariance matrix
  mean_returns <- colMeans(returns)
  cov_matrix <- stats::cov(returns)
  
  # Scale mean and covariance for horizon
  mean_returns_scaled <- mean_returns * horizon
  cov_matrix_scaled <- cov_matrix * horizon
  
  # Generate multivariate normal random returns
  sim_returns <- MASS::mvrnorm(n = n_sim, mu = mean_returns_scaled, Sigma = cov_matrix_scaled)
  
  # Calculate portfolio returns for each simulation
  portfolio_sim_returns <- sim_returns %*% weights
  
  # Calculate VaR
  var_value <- -stats::quantile(portfolio_sim_returns, 1 - p)
  
  # Calculate ES
  es_value <- -mean(portfolio_sim_returns[portfolio_sim_returns <= -var_value])
  
  # Calculate VaR and ES in monetary terms
  var_monetary <- as.numeric(var_value) * portfolio_value
  es_monetary <- as.numeric(es_value) * portfolio_value
  
  # Create result
  result <- list(
    var_pct = as.numeric(var_value),
    var_monetary = var_monetary,
    es_pct = as.numeric(es_value),
    es_monetary = es_monetary,
    confidence_level = p,
    n_simulations = n_sim,
    horizon = horizon,
    portfolio_value = portfolio_value,
    simulated_returns = portfolio_sim_returns
  )
  
  return(result)
}

#' Perform stress testing on a portfolio
#'
#' @param returns A matrix or data frame of asset returns
#' @param weights A numeric vector of portfolio weights
#' @param scenarios A list of stress scenarios
#' @param portfolio_value Portfolio value (default: 1000000)
#'
#' @return A data frame with stress test results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns for 5 assets
#' returns <- matrix(rnorm(5000, mean = 0.0005, sd = 0.01), ncol = 5)
#' colnames(returns) <- paste0("Asset", 1:5)
#'
#' # Define portfolio weights
#' weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
#'
#' # Define stress scenarios
#' scenarios <- list(
#'   "Market Crash" = c(-0.05, -0.06, -0.04, -0.03, -0.05),
#'   "Tech Bubble" = c(-0.02, -0.08, -0.01, -0.03, -0.02),
#'   "Financial Crisis" = c(-0.04, -0.03, -0.06, -0.05, -0.04)
#' )
#'
#' # Perform stress testing
#' stress_results <- stress_test_portfolio(returns, weights, scenarios)
#' }
stress_test_portfolio <- function(returns, weights, scenarios, portfolio_value = 1000000) {
  # Check if returns is a valid object
  if (!is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a matrix, data frame, or xts object")
  }
  
  # Convert to matrix if not already
  if (xts::is.xts(returns)) {
    returns <- as.matrix(returns)
  } else if (is.data.frame(returns)) {
    returns <- as.matrix(returns)
  }
  
  # Check if weights sum to 1
  if (abs(sum(weights) - 1) > 1e-6) {
    warning("Weights do not sum to 1. Normalizing weights.")
    weights <- weights / sum(weights)
  }
  
  # Check if number of weights matches number of assets
  if (length(weights) != ncol(returns)) {
    stop("Number of weights must match number of assets")
  }
  
  # Check if scenarios are valid
  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]
    if (length(scenario) != ncol(returns)) {
      stop("Scenario '", scenario_name, "' has incorrect number of assets")
    }
  }
  
  # Calculate portfolio impact for each scenario
  results <- data.frame(
    scenario = character(),
    portfolio_return = numeric(),
    portfolio_impact = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]
    
    # Calculate portfolio return under scenario
    portfolio_return <- sum(weights * scenario)
    
    # Calculate monetary impact
    portfolio_impact <- portfolio_return * portfolio_value
    
    # Add to results
    results <- rbind(results, data.frame(
      scenario = scenario_name,
      portfolio_return = portfolio_return,
      portfolio_impact = portfolio_impact,
      stringsAsFactors = FALSE
    ))
  }
  
  return(results)
}

#' Calculate VaR using GARCH model
#'
#' @param returns A numeric vector, matrix, data frame, or xts object of returns
#' @param p Confidence level (default: 0.95)
#' @param garch_model GARCH model specification (default: "sGARCH")
#' @param garch_order GARCH order (default: c(1,1))
#' @param distribution Distribution for innovations (default: "norm")
#' @param forecast_horizon Forecast horizon in days (default: 1)
#' @param portfolio_value Portfolio value (default: 1000000)
#'
#' @return A list with GARCH VaR results
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns
#' returns <- rnorm(1000, mean = 0.0005, sd = 0.01)
#'
#' # Calculate GARCH VaR
#' garch_var <- calculate_garch_var(returns)
#' }
calculate_garch_var <- function(returns, p = 0.95, garch_model = "sGARCH", garch_order = c(1, 1),
                               distribution = "norm", forecast_horizon = 1, portfolio_value = 1000000) {
  # Check if rugarch is available
  if (!requireNamespace("rugarch", quietly = TRUE)) {
    stop("Package 'rugarch' is required for GARCH VaR calculation")
  }
  
  # Check if returns is a valid object
  if (!is.numeric(returns) && !is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a numeric vector, matrix, data frame, or xts object")
  }
  
  # Convert to xts if not already
  if (!xts::is.xts(returns)) {
    if (is.matrix(returns) || is.data.frame(returns)) {
      returns <- xts::as.xts(returns)
    } else {
      returns <- xts::as.xts(matrix(returns))
    }
  }
  
  # Specify GARCH model
  garch_spec <- rugarch::ugarchspec(
    variance.model = list(model = garch_model, garchOrder = garch_order),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = distribution
  )
  
  # Fit GARCH model
  garch_fit <- rugarch::ugarchfit(garch_spec, returns)
  
  # Forecast volatility
  garch_forecast <- rugarch::ugarchforecast(garch_fit, n.ahead = forecast_horizon)
  
  # Extract forecasted volatility
  sigma_forecast <- rugarch::sigma(garch_forecast)
  
  # Extract forecasted mean
  mu_forecast <- rugarch::fitted(garch_forecast)
  
  # Calculate VaR
  if (distribution == "norm") {
    # Normal distribution
    var_value <- -mu_forecast - stats::qnorm(p) * sigma_forecast
  } else if (distribution == "std") {
    # Student-t distribution
    nu <- rugarch::coef(garch_fit)["shape"]
    var_value <- -mu_forecast - stats::qt(p, df = nu) * sigma_forecast
  } else {
    # Default to normal for other distributions
    var_value <- -mu_forecast - stats::qnorm(p) * sigma_forecast
  }
  
  # Calculate VaR in monetary terms
  var_monetary <- as.numeric(var_value) * portfolio_value
  
  # Create result
  result <- list(
    var_pct = as.numeric(var_value),
    var_monetary = var_monetary,
    confidence_level = p,
    garch_model = garch_model,
    garch_order = garch_order,
    distribution = distribution,
    forecast_horizon = forecast_horizon,
    portfolio_value = portfolio_value,
    garch_fit = garch_fit,
    garch_forecast = garch_forecast
  )
  
  return(result)
}

#' Clean return outliers
#'
#' @param returns A numeric vector, matrix, data frame, or xts object of returns
#' @param winsorize_level Level for winsorizing (default: 0.01)
#'
#' @return Returns with outliers cleaned
#' @keywords internal
clean_return_outliers <- function(returns, winsorize_level = 0.01) {
  # Check if returns is a valid object
  if (!is.numeric(returns) && !is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a numeric vector, matrix, data frame, or xts object")
  }
  
  # Convert to xts if not already
  if (!xts::is.xts(returns)) {
    if (is.matrix(returns) || is.data.frame(returns)) {
      returns <- xts::as.xts(returns)
    } else {
      returns <- xts::as.xts(matrix(returns))
    }
  }
  
  # Winsorize returns
  lower_bound <- stats::quantile(returns, winsorize_level)
  upper_bound <- stats::quantile(returns, 1 - winsorize_level)
  
  # Apply winsorization
  returns[returns < lower_bound] <- lower_bound
  returns[returns > upper_bound] <- upper_bound
  
  return(returns)
}

#' Plot VaR backtesting results
#'
#' @param returns A numeric vector, matrix, data frame, or xts object of returns
#' @param var_values A numeric vector of VaR values
#' @param p Confidence level used for VaR calculation (default: 0.95)
#' @param title Plot title (default: "VaR Backtesting")
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate random returns
#' returns <- rnorm(1000, mean = 0.0005, sd = 0.01)
#'
#' # Calculate VaR for each day
#' var_values <- numeric(length(returns))
#' for (i in 252:length(returns)) {
#'   var_values[i] <- calculate_var(returns[(i-251):(i-1)], method = "historical")$var_pct
#' }
#'
#' # Plot backtesting results
#' plot_var_backtest(returns, var_values)
#' }
plot_var_backtest <- function(returns, var_values, p = 0.95, title = "VaR Backtesting") {
  # Check if returns is a valid object
  if (!is.numeric(returns) && !is.matrix(returns) && !is.data.frame(returns) && !xts::is.xts(returns)) {
    stop("Returns must be a numeric vector, matrix, data frame, or xts object")
  }
  
  # Convert to xts if not already
  if (!xts::is.xts(returns)) {
    if (is.matrix(returns) || is.data.frame(returns)) {
      returns <- xts::as.xts(returns)
    } else {
      returns <- xts::as.xts(matrix(returns))
    }
  }
  
  # Check if var_values has the same length as returns
  if (length(var_values) != nrow(returns)) {
    stop("VaR values must have the same length as returns")
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    date = zoo::index(returns),
    returns = as.numeric(returns),
    var = as.numeric(var_values),
    violation = as.numeric(returns) < -as.numeric(var_values)
  )
  
  # Count violations
  n_violations <- sum(plot_data$violation, na.rm = TRUE)
  n_obs <- sum(!is.na(plot_data$violation))
  expected_violations <- (1 - p) * n_obs
  violation_ratio <- n_violations / expected_violations
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = returns), color = "darkgrey") +
    ggplot2::geom_line(ggplot2::aes(y = -var), color = "red", linetype = "dashed") +
    ggplot2::geom_point(data = plot_data[plot_data$violation, ], 
                       ggplot2::aes(y = returns), color = "red", size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Violations: ", n_violations, " (", round(n_violations / n_obs * 100, 2), "%), ",
                      "Expected: ", round(expected_violations), " (", round((1 - p) * 100, 2), "%), ",
                      "Ratio: ", round(violation_ratio, 2)),
      x = "Date",
      y = "Returns",
      caption = paste0("Confidence Level: ", p * 100, "%")
    )
  
  return(p)
}

