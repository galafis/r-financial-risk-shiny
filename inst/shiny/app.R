#' Financial Risk Analysis Shiny App
#'
#' @description A Shiny application for financial risk analysis and visualization.
#'
#' @import shiny
#' @import shinydashboard
#' @import shinythemes
#' @import shinyjs
#' @import shinyWidgets
#' @import DT
#' @import plotly
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import xts
#' @import quantmod
#' @import PerformanceAnalytics
#' @import PortfolioAnalytics
#'
#' @author Gabriel Demetrios Lafis
#'
#' @examples
#' \dontrun{
#' # Run the Shiny app
#' shiny::runApp(system.file("shiny", package = "rfinancialriskshiny"))
#' }

# Load required packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(rugarch)
library(rmgarch)
library(viridis)
library(RColorBrewer)

# UI definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = "Financial Risk Analysis",
    titleWidth = 300
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Value at Risk (VaR)", tabName = "var", icon = icon("chart-line")),
      menuItem("Portfolio Optimization", tabName = "portfolio", icon = icon("balance-scale")),
      menuItem("Stress Testing", tabName = "stress", icon = icon("exclamation-triangle")),
      menuItem("Risk Factor Analysis", tabName = "factors", icon = icon("search")),
      menuItem("Monte Carlo Simulation", tabName = "monte_carlo", icon = icon("random")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          border-radius: 3px;
        }
        .box-header {
          color: #444;
          background-color: #f8f9fa;
        }
        .box-title {
          font-weight: 600;
        }
        .small-box {
          border-radius: 3px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
      "))
    ),
    
    # Tab content
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Portfolio Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotlyOutput("portfolio_overview_plot", height = "300px")
          )
        ),
        fluidRow(
          valueBoxOutput("var_box", width = 3),
          valueBoxOutput("es_box", width = 3),
          valueBoxOutput("sharpe_box", width = 3),
          valueBoxOutput("volatility_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Risk Metrics",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotlyOutput("risk_metrics_plot", height = "300px")
          ),
          box(
            title = "Asset Allocation",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            plotlyOutput("asset_allocation_plot", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Historical Performance",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotlyOutput("historical_performance_plot", height = "300px")
          )
        )
      ),
      
      # Value at Risk (VaR) tab
      tabItem(
        tabName = "var",
        fluidRow(
          box(
            title = "VaR Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            selectInput("var_method", "VaR Method:",
                       choices = c("Historical" = "historical",
                                  "Gaussian" = "gaussian",
                                  "Modified" = "modified",
                                  "Cornish-Fisher" = "cornish_fisher",
                                  "GARCH" = "garch",
                                  "Monte Carlo" = "monte_carlo"),
                       selected = "historical"),
            sliderInput("var_confidence", "Confidence Level:",
                       min = 0.9, max = 0.99, value = 0.95, step = 0.01),
            numericInput("var_portfolio_value", "Portfolio Value:",
                        value = 1000000, min = 1000, step = 1000),
            conditionalPanel(
              condition = "input.var_method == 'monte_carlo'",
              numericInput("var_n_sim", "Number of Simulations:",
                          value = 10000, min = 1000, step = 1000)
            ),
            conditionalPanel(
              condition = "input.var_method == 'garch'",
              selectInput("var_garch_model", "GARCH Model:",
                         choices = c("sGARCH", "eGARCH", "gjrGARCH"),
                         selected = "sGARCH"),
              selectInput("var_garch_dist", "Distribution:",
                         choices = c("Normal" = "norm", "Student-t" = "std"),
                         selected = "norm")
            ),
            actionButton("calculate_var_btn", "Calculate VaR", 
                        class = "btn-primary btn-block")
          ),
          box(
            title = "VaR Results",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Summary",
                      fluidRow(
                        column(6, valueBoxOutput("var_value_box", width = 12)),
                        column(6, valueBoxOutput("es_value_box", width = 12))
                      ),
                      fluidRow(
                        column(12, tableOutput("var_summary_table"))
                      )
              ),
              tabPanel("Component VaR",
                      plotlyOutput("component_var_plot", height = "300px"),
                      DTOutput("component_var_table")
              ),
              tabPanel("Backtesting",
                      plotlyOutput("var_backtest_plot", height = "400px"),
                      tableOutput("var_backtest_summary")
              )
            )
          )
        )
      ),
      
      # Portfolio Optimization tab
      tabItem(
        tabName = "portfolio",
        fluidRow(
          box(
            title = "Optimization Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            selectInput("opt_objective", "Optimization Objective:",
                       choices = c("Maximize Return" = "return",
                                  "Minimize Risk" = "risk",
                                  "Maximize Sharpe Ratio" = "sharpe"),
                       selected = "sharpe"),
            sliderInput("opt_risk_aversion", "Risk Aversion:",
                       min = 0.1, max = 5, value = 1, step = 0.1),
            checkboxInput("opt_constraints", "Add Constraints", value = TRUE),
            conditionalPanel(
              condition = "input.opt_constraints == true",
              sliderInput("opt_max_weight", "Maximum Weight per Asset:",
                         min = 0.1, max = 1, value = 0.4, step = 0.05),
              checkboxInput("opt_allow_short", "Allow Short Selling", value = FALSE)
            ),
            actionButton("run_optimization_btn", "Run Optimization", 
                        class = "btn-primary btn-block")
          ),
          box(
            title = "Optimization Results",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Efficient Frontier",
                      plotlyOutput("efficient_frontier_plot", height = "400px")
              ),
              tabPanel("Optimal Portfolio",
                      fluidRow(
                        column(6, plotlyOutput("optimal_weights_plot", height = "300px")),
                        column(6, tableOutput("optimal_portfolio_summary"))
                      ),
                      DTOutput("optimal_weights_table")
              ),
              tabPanel("Performance",
                      plotlyOutput("optimal_performance_plot", height = "400px"),
                      tableOutput("optimal_performance_summary")
              )
            )
          )
        )
      ),
      
      # Stress Testing tab
      tabItem(
        tabName = "stress",
        fluidRow(
          box(
            title = "Stress Test Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            selectInput("stress_scenario", "Predefined Scenarios:",
                       choices = c("Financial Crisis 2008" = "crisis_2008",
                                  "Tech Bubble 2000" = "tech_2000",
                                  "COVID-19 Crash" = "covid_2020",
                                  "Custom Scenario" = "custom"),
                       selected = "crisis_2008"),
            conditionalPanel(
              condition = "input.stress_scenario == 'custom'",
              numericInput("stress_equity", "Equity Shock (%):", value = -20),
              numericInput("stress_bonds", "Bond Shock (%):", value = -5),
              numericInput("stress_commodities", "Commodities Shock (%):", value = -15),
              numericInput("stress_real_estate", "Real Estate Shock (%):", value = -10),
              numericInput("stress_currencies", "Currency Shock (%):", value = -8)
            ),
            actionButton("run_stress_test_btn", "Run Stress Test", 
                        class = "btn-primary btn-block")
          ),
          box(
            title = "Stress Test Results",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Impact Summary",
                      plotlyOutput("stress_impact_plot", height = "300px"),
                      tableOutput("stress_summary_table")
              ),
              tabPanel("Asset Breakdown",
                      plotlyOutput("stress_breakdown_plot", height = "300px"),
                      DTOutput("stress_breakdown_table")
              ),
              tabPanel("Historical Comparison",
                      plotlyOutput("stress_historical_plot", height = "400px")
              )
            )
          )
        )
      ),
      
      # Risk Factor Analysis tab
      tabItem(
        tabName = "factors",
        fluidRow(
          box(
            title = "Risk Factor Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            selectInput("factor_model", "Factor Model:",
                       choices = c("Market" = "market",
                                  "Fama-French 3-Factor" = "ff3",
                                  "Fama-French 5-Factor" = "ff5",
                                  "Custom Factors" = "custom"),
                       selected = "ff3"),
            dateRangeInput("factor_date_range", "Date Range:",
                          start = Sys.Date() - 365*3, end = Sys.Date()),
            actionButton("run_factor_analysis_btn", "Run Factor Analysis", 
                        class = "btn-primary btn-block")
          ),
          box(
            title = "Risk Factor Results",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Factor Exposures",
                      plotlyOutput("factor_exposures_plot", height = "300px"),
                      DTOutput("factor_exposures_table")
              ),
              tabPanel("Factor Contribution",
                      plotlyOutput("factor_contribution_plot", height = "300px"),
                      DTOutput("factor_contribution_table")
              ),
              tabPanel("Factor Returns",
                      plotlyOutput("factor_returns_plot", height = "400px")
              )
            )
          )
        )
      ),
      
      # Monte Carlo Simulation tab
      tabItem(
        tabName = "monte_carlo",
        fluidRow(
          box(
            title = "Monte Carlo Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            collapsible = TRUE,
            numericInput("mc_n_sim", "Number of Simulations:",
                        value = 10000, min = 1000, step = 1000),
            numericInput("mc_horizon", "Forecast Horizon (Days):",
                        value = 252, min = 1, step = 1),
            selectInput("mc_method", "Simulation Method:",
                       choices = c("Normal" = "normal",
                                  "Student-t" = "t",
                                  "GARCH" = "garch",
                                  "Copula" = "copula"),
                       selected = "normal"),
            conditionalPanel(
              condition = "input.mc_method == 't'",
              sliderInput("mc_df", "Degrees of Freedom:",
                         min = 3, max = 30, value = 5, step = 1)
            ),
            actionButton("run_monte_carlo_btn", "Run Simulation", 
                        class = "btn-primary btn-block")
          ),
          box(
            title = "Monte Carlo Results",
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Simulation Paths",
                      plotlyOutput("mc_paths_plot", height = "400px")
              ),
              tabPanel("Terminal Distribution",
                      plotlyOutput("mc_distribution_plot", height = "300px"),
                      tableOutput("mc_statistics_table")
              ),
              tabPanel("Risk Metrics",
                      plotlyOutput("mc_risk_plot", height = "300px"),
                      tableOutput("mc_risk_table")
              )
            )
          )
        )
      ),
      
      # Settings tab
      tabItem(
        tabName = "settings",
        fluidRow(
          box(
            title = "Portfolio Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            fileInput("portfolio_file", "Upload Portfolio Data (CSV):",
                     accept = c("text/csv", "text/comma-separated-values", ".csv")),
            hr(),
            h4("Manual Portfolio Entry"),
            DTOutput("portfolio_input_table"),
            actionButton("add_asset_btn", "Add Asset", class = "btn-default"),
            actionButton("save_portfolio_btn", "Save Portfolio", class = "btn-primary")
          ),
          box(
            title = "Data Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            dateRangeInput("data_date_range", "Historical Data Range:",
                          start = Sys.Date() - 365*5, end = Sys.Date()),
            selectInput("data_source", "Data Source:",
                       choices = c("Yahoo Finance" = "yahoo",
                                  "Alpha Vantage" = "av",
                                  "Local CSV" = "csv"),
                       selected = "yahoo"),
            conditionalPanel(
              condition = "input.data_source == 'av'",
              textInput("api_key", "Alpha Vantage API Key:")
            ),
            conditionalPanel(
              condition = "input.data_source == 'csv'",
              fileInput("price_data_file", "Upload Price Data (CSV):",
                       accept = c("text/csv", "text/comma-separated-values", ".csv"))
            ),
            actionButton("fetch_data_btn", "Fetch Data", class = "btn-primary")
          )
        ),
        fluidRow(
          box(
            title = "Application Settings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            tabsetPanel(
              tabPanel("Display",
                      fluidRow(
                        column(6,
                              selectInput("theme", "Application Theme:",
                                         choices = c("Blue" = "blue",
                                                    "Black" = "black",
                                                    "Purple" = "purple",
                                                    "Green" = "green",
                                                    "Red" = "red",
                                                    "Yellow" = "yellow"),
                                         selected = "blue")
                        ),
                        column(6,
                              selectInput("plot_theme", "Plot Theme:",
                                         choices = c("Default" = "default",
                                                    "Minimal" = "minimal",
                                                    "Classic" = "classic",
                                                    "Dark" = "dark"),
                                         selected = "default")
                        )
                      ),
                      fluidRow(
                        column(6,
                              selectInput("color_palette", "Color Palette:",
                                         choices = c("Viridis" = "viridis",
                                                    "Magma" = "magma",
                                                    "Plasma" = "plasma",
                                                    "Inferno" = "inferno",
                                                    "Blues" = "Blues",
                                                    "Reds" = "Reds",
                                                    "Greens" = "Greens",
                                                    "Spectral" = "Spectral"),
                                         selected = "viridis")
                        ),
                        column(6,
                              numericInput("decimal_places", "Decimal Places:",
                                          value = 4, min = 1, max = 6, step = 1)
                        )
                      )
              ),
              tabPanel("Calculation",
                      fluidRow(
                        column(6,
                              selectInput("return_calc", "Return Calculation:",
                                         choices = c("Simple" = "simple",
                                                    "Log" = "log"),
                                         selected = "simple")
                        ),
                        column(6,
                              selectInput("risk_measure", "Default Risk Measure:",
                                         choices = c("Standard Deviation" = "sd",
                                                    "Value at Risk" = "var",
                                                    "Expected Shortfall" = "es",
                                                    "Downside Deviation" = "dd"),
                                         selected = "sd")
                        )
                      ),
                      fluidRow(
                        column(6,
                              numericInput("annualization", "Annualization Factor:",
                                          value = 252, min = 1, step = 1)
                        ),
                        column(6,
                              numericInput("risk_free_rate", "Risk-Free Rate (%):",
                                          value = 2, min = 0, max = 10, step = 0.1)
                        )
                      )
              ),
              tabPanel("Export",
                      fluidRow(
                        column(6,
                              selectInput("export_format", "Default Export Format:",
                                         choices = c("CSV" = "csv",
                                                    "Excel" = "xlsx",
                                                    "PDF" = "pdf"),
                                         selected = "csv")
                        ),
                        column(6,
                              textInput("report_title", "Default Report Title:",
                                       value = "Financial Risk Analysis Report")
                        )
                      ),
                      fluidRow(
                        column(12,
                              textAreaInput("report_header", "Report Header Text:",
                                          value = "This report was generated using the Financial Risk Analysis Shiny App.",
                                          height = "100px")
                        )
                      )
              )
            ),
            actionButton("save_settings_btn", "Save Settings", class = "btn-primary"),
            actionButton("reset_settings_btn", "Reset to Defaults", class = "btn-default")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Sample data for demonstration
  sample_returns <- reactive({
    set.seed(123)
    dates <- seq.Date(from = Sys.Date() - 365, to = Sys.Date(), by = "day")
    dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
    
    n <- length(dates)
    assets <- c("AAPL", "MSFT", "AMZN", "GOOGL", "FB")
    
    # Generate correlated returns
    sigma <- matrix(c(
      0.0004, 0.0002, 0.0001, 0.0002, 0.0001,
      0.0002, 0.0003, 0.0001, 0.0001, 0.0001,
      0.0001, 0.0001, 0.0005, 0.0001, 0.0001,
      0.0002, 0.0001, 0.0001, 0.0003, 0.0001,
      0.0001, 0.0001, 0.0001, 0.0001, 0.0004
    ), nrow = 5)
    
    mu <- c(0.0008, 0.0007, 0.0009, 0.0007, 0.0008)
    
    returns <- MASS::mvrnorm(n = n, mu = mu, Sigma = sigma)
    colnames(returns) <- assets
    
    # Convert to xts
    returns_xts <- xts::as.xts(returns, order.by = dates)
    
    return(returns_xts)
  })
  
  # Sample portfolio weights
  sample_weights <- reactive({
    c(0.2, 0.2, 0.2, 0.2, 0.2)
  })
  
  # Calculate portfolio returns
  portfolio_returns <- reactive({
    returns <- sample_returns()
    weights <- sample_weights()
    
    # Calculate portfolio returns
    port_returns <- returns %*% weights
    colnames(port_returns) <- "Portfolio"
    
    return(port_returns)
  })
  
  # Dashboard outputs
  output$portfolio_overview_plot <- renderPlotly({
    returns <- sample_returns()
    
    # Calculate cumulative returns
    cum_returns <- apply(1 + returns, 2, cumprod)
    cum_returns <- cum_returns - 1
    
    # Convert to data frame for plotting
    df <- data.frame(
      Date = index(returns),
      cum_returns
    )
    
    # Reshape for plotting
    df_long <- tidyr::pivot_longer(df, -Date, names_to = "Asset", values_to = "Return")
    
    # Create plot
    p <- ggplot(df_long, aes(x = Date, y = Return, color = Asset)) +
      geom_line() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Cumulative Returns", x = "Date", y = "Cumulative Return") +
      theme_minimal() +
      scale_color_viridis_d()
    
    ggplotly(p)
  })
  
  # Value boxes
  output$var_box <- renderValueBox({
    port_returns <- portfolio_returns()
    var_value <- calculate_var(port_returns, p = 0.95, method = "historical")$var_pct
    
    valueBox(
      paste0(round(var_value * 100, 2), "%"),
      "Daily VaR (95%)",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  output$es_box <- renderValueBox({
    port_returns <- portfolio_returns()
    es_value <- calculate_expected_shortfall(port_returns, p = 0.95, method = "historical")$es_pct
    
    valueBox(
      paste0(round(es_value * 100, 2), "%"),
      "Expected Shortfall (95%)",
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  
  output$sharpe_box <- renderValueBox({
    port_returns <- portfolio_returns()
    sharpe <- mean(port_returns) / sd(port_returns) * sqrt(252)
    
    valueBox(
      round(sharpe, 2),
      "Annualized Sharpe Ratio",
      icon = icon("balance-scale"),
      color = "green"
    )
  })
  
  output$volatility_box <- renderValueBox({
    port_returns <- portfolio_returns()
    vol <- sd(port_returns) * sqrt(252)
    
    valueBox(
      paste0(round(vol * 100, 2), "%"),
      "Annualized Volatility",
      icon = icon("percent"),
      color = "blue"
    )
  })
  
  # Risk metrics plot
  output$risk_metrics_plot <- renderPlotly({
    returns <- sample_returns()
    
    # Calculate rolling volatility
    roll_vol <- sapply(returns, function(x) {
      rollapply(x, width = 21, FUN = sd, align = "right") * sqrt(252)
    })
    roll_vol <- na.omit(roll_vol)
    
    # Convert to data frame for plotting
    df <- data.frame(
      Date = index(roll_vol),
      roll_vol
    )
    
    # Reshape for plotting
    df_long <- tidyr::pivot_longer(df, -Date, names_to = "Asset", values_to = "Volatility")
    
    # Create plot
    p <- ggplot(df_long, aes(x = Date, y = Volatility, color = Asset)) +
      geom_line() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Rolling 21-Day Annualized Volatility", x = "Date", y = "Volatility") +
      theme_minimal() +
      scale_color_viridis_d()
    
    ggplotly(p)
  })
  
  # Asset allocation plot
  output$asset_allocation_plot <- renderPlotly({
    weights <- sample_weights()
    assets <- colnames(sample_returns())
    
    # Create data frame for plotting
    df <- data.frame(
      Asset = assets,
      Weight = weights
    )
    
    # Create plot
    p <- plot_ly(df, labels = ~Asset, values = ~Weight, type = "pie",
                marker = list(colors = viridis::viridis(length(assets)))) %>%
      layout(title = "Portfolio Allocation",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    p
  })
  
  # Historical performance plot
  output$historical_performance_plot <- renderPlotly({
    returns <- sample_returns()
    port_returns <- portfolio_returns()
    
    # Calculate cumulative returns
    cum_returns <- cumprod(1 + port_returns) - 1
    
    # Convert to data frame for plotting
    df <- data.frame(
      Date = index(cum_returns),
      Return = cum_returns
    )
    
    # Create plot
    p <- ggplot(df, aes(x = Date, y = Return)) +
      geom_line(color = "steelblue", size = 1) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Portfolio Cumulative Return", x = "Date", y = "Cumulative Return") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # VaR calculation
  observeEvent(input$calculate_var_btn, {
    port_returns <- portfolio_returns()
    
    if (input$var_method == "garch") {
      var_result <- calculate_garch_var(
        port_returns,
        p = input$var_confidence,
        garch_model = input$var_garch_model,
        distribution = input$var_garch_dist,
        portfolio_value = input$var_portfolio_value
      )
    } else if (input$var_method == "monte_carlo") {
      returns <- sample_returns()
      weights <- sample_weights()
      
      var_result <- calculate_monte_carlo_var(
        returns,
        weights,
        p = input$var_confidence,
        n_sim = input$var_n_sim,
        portfolio_value = input$var_portfolio_value
      )
    } else {
      var_result <- calculate_var(
        port_returns,
        p = input$var_confidence,
        method = input$var_method,
        portfolio_value = input$var_portfolio_value
      )
    }
    
    # Calculate ES
    es_result <- calculate_expected_shortfall(
      port_returns,
      p = input$var_confidence,
      method = ifelse(input$var_method == "cornish_fisher", "modified", input$var_method),
      portfolio_value = input$var_portfolio_value
    )
    
    # Update value boxes
    output$var_value_box <- renderValueBox({
      valueBox(
        paste0("$", format(round(var_result$var_monetary), big.mark = ",")),
        paste0("VaR (", input$var_confidence * 100, "%)"),
        icon = icon("chart-line"),
        color = "red"
      )
    })
    
    output$es_value_box <- renderValueBox({
      valueBox(
        paste0("$", format(round(es_result$es_monetary), big.mark = ",")),
        paste0("Expected Shortfall (", input$var_confidence * 100, "%)"),
        icon = icon("exclamation-triangle"),
        color = "orange"
      )
    })
    
    # Update summary table
    output$var_summary_table <- renderTable({
      data.frame(
        Metric = c("VaR (%)", "VaR ($)", "ES (%)", "ES ($)", "Method", "Confidence Level"),
        Value = c(
          paste0(round(var_result$var_pct * 100, 4), "%"),
          paste0("$", format(round(var_result$var_monetary), big.mark = ",")),
          paste0(round(es_result$es_pct * 100, 4), "%"),
          paste0("$", format(round(es_result$es_monetary), big.mark = ",")),
          toupper(input$var_method),
          paste0(input$var_confidence * 100, "%")
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Update component VaR plot and table
    if (input$var_method != "garch" && input$var_method != "monte_carlo") {
      returns <- sample_returns()
      weights <- sample_weights()
      
      component_var <- calculate_component_var(
        returns,
        weights,
        p = input$var_confidence,
        portfolio_value = input$var_portfolio_value
      )
      
      output$component_var_plot <- renderPlotly({
        # Create data frame for plotting
        df <- component_var
        
        # Create plot
        p <- ggplot(df, aes(x = asset, y = pct_contrib, fill = asset)) +
          geom_bar(stat = "identity") +
          scale_y_continuous(labels = scales::percent_format(scale = 1)) +
          labs(title = "Component VaR Contribution", x = "Asset", y = "Contribution (%)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_viridis_d()
        
        ggplotly(p)
      })
      
      output$component_var_table <- renderDT({
        df <- component_var
        df$weight <- scales::percent(df$weight)
        df$marginal_var <- scales::percent(df$marginal_var, accuracy = 0.01)
        df$component_var <- scales::percent(df$component_var, accuracy = 0.01)
        df$pct_contrib <- scales::percent(df$pct_contrib / 100, accuracy = 0.01)
        df$component_var_monetary <- paste0("$", format(round(df$component_var_monetary), big.mark = ","))
        
        datatable(df, options = list(pageLength = 5))
      })
    }
    
    # Update backtesting plot
    output$var_backtest_plot <- renderPlotly({
      # Calculate rolling VaR
      window_size <- 252
      n <- length(port_returns)
      var_values <- numeric(n)
      
      for (i in (window_size + 1):n) {
        var_values[i] <- calculate_var(
          port_returns[(i - window_size):(i - 1)],
          p = input$var_confidence,
          method = ifelse(input$var_method == "monte_carlo" || input$var_method == "garch", "historical", input$var_method)
        )$var_pct
      }
      
      # Create backtesting plot
      df <- data.frame(
        Date = index(port_returns),
        Returns = as.numeric(port_returns),
        VaR = var_values
      )
      
      df$Violation <- df$Returns < -df$VaR
      
      # Count violations
      violations <- sum(df$Violation, na.rm = TRUE)
      n_obs <- sum(!is.na(df$Violation))
      expected_violations <- (1 - input$var_confidence) * n_obs
      violation_ratio <- violations / expected_violations
      
      # Create plot
      p <- ggplot(df, aes(x = Date)) +
        geom_line(aes(y = Returns), color = "darkgrey") +
        geom_line(aes(y = -VaR), color = "red", linetype = "dashed") +
        geom_point(data = df[df$Violation, ], aes(y = Returns), color = "red", size = 2) +
        theme_minimal() +
        labs(
          title = "VaR Backtesting",
          subtitle = paste0("Violations: ", violations, " (", round(violations / n_obs * 100, 2), "%), ",
                          "Expected: ", round(expected_violations), " (", round((1 - input$var_confidence) * 100, 2), "%), ",
                          "Ratio: ", round(violation_ratio, 2)),
          x = "Date",
          y = "Returns"
        )
      
      ggplotly(p)
    })
    
    # Update backtesting summary
    output$var_backtest_summary <- renderTable({
      # Calculate rolling VaR
      window_size <- 252
      n <- length(port_returns)
      var_values <- numeric(n)
      
      for (i in (window_size + 1):n) {
        var_values[i] <- calculate_var(
          port_returns[(i - window_size):(i - 1)],
          p = input$var_confidence,
          method = ifelse(input$var_method == "monte_carlo" || input$var_method == "garch", "historical", input$var_method)
        )$var_pct
      }
      
      # Create backtesting data
      df <- data.frame(
        Date = index(port_returns),
        Returns = as.numeric(port_returns),
        VaR = var_values
      )
      
      df$Violation <- df$Returns < -df$VaR
      
      # Count violations
      violations <- sum(df$Violation, na.rm = TRUE)
      n_obs <- sum(!is.na(df$Violation))
      expected_violations <- (1 - input$var_confidence) * n_obs
      violation_ratio <- violations / expected_violations
      
      # Perform Kupiec test
      p_hat <- violations / n_obs
      p_0 <- 1 - input$var_confidence
      
      if (violations > 0 && violations < n_obs) {
        lr_stat <- -2 * log(((p_0^violations) * (1 - p_0)^(n_obs - violations)) / 
                           ((p_hat^violations) * (1 - p_hat)^(n_obs - violations)))
        p_value <- 1 - pchisq(lr_stat, df = 1)
      } else {
        lr_stat <- NA
        p_value <- NA
      }
      
      # Create summary table
      data.frame(
        Metric = c("Observations", "Violations", "Expected Violations", "Violation Ratio", 
                  "Kupiec Test Statistic", "Kupiec Test p-value", "Model Rejected"),
        Value = c(
          n_obs,
          violations,
          round(expected_violations, 2),
          round(violation_ratio, 2),
          ifelse(is.na(lr_stat), "N/A", round(lr_stat, 4)),
          ifelse(is.na(p_value), "N/A", round(p_value, 4)),
          ifelse(is.na(p_value), "N/A", ifelse(p_value < 0.05, "Yes", "No"))
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  })
  
  # Run the application
  shinyApp(ui, server)
}

# Run the application
shinyApp(ui = ui, server = server)

