# --- Fixed Financial Parameters ---
S0 = 50       # Initial Price (EUR)
K = 55        # Strike Price (EUR)
T = 1         # Time to Expiration (years)
r = 0.05      # Risk-Free Rate
sigma = 0.2   # Volatility

# --- Varying Simulation Parameters ---
n_values <- c(300, 600)       # Number of averaging steps
m_values <- c(100, 10000)     # Number of Monte Carlo Simulations

# Function to price an Asian Call Option using Monte Carlo Simulation
asian_option_price <- function(
    S0, K, T, r, sigma, n, m, average_type
) {
  dt <- T / n
  
  # Initialize matrix for asset prices (m simulations x (n+1) steps)
  S_path <- matrix(NA, nrow = m, ncol = n + 1)
  S_path[, 1] <- S0
  
  # Monte Carlo Simulation loop
  for (j in 1:m) {
    Z <- rnorm(n) 
    for (i in 1:n) {
      # Geometric Brownian Motion (Risk-Neutral)
      S_path[j, i + 1] <- S_path[j, i] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[i])
    }
  }
  
  # --- Calculate Average Mean ---
  S_prices_to_average <- S_path[, 2:(n + 1)]
  
  if (average_type == "arithmetic") {
    S_bar <- rowMeans(S_prices_to_average)
  } else if (average_type == "geometric") {
    S_bar <- exp(rowMeans(log(S_prices_to_average)))
  }
  
  # --- Calculate Payoff and Discount ---
  payoff <- pmax(S_bar - K, 0)
  option_price <- exp(-r * T) * mean(payoff)
  
  return(option_price)
}

# --- Main Execution and Output ---

# Initialize a data frame to store all results
results_df <- data.frame(
  n = integer(),
  m = integer(),
  Arithmetic_Price = numeric(),
  Geometric_Price = numeric()
)

cat("--- Asian Option Prices (Monte Carlo Simulation) ---\n")

for (n_val in n_values) {
  for (m_val in m_values) {
    
    # Calculate Arithmetic Price
    price_arithmetic <- asian_option_price(
      S0, K, T, r, sigma, n_val, m_val, average_type = "arithmetic"
    )
    
    # Calculate Geometric Price
    price_geometric <- asian_option_price(
      S0, K, T, r, sigma, n_val, m_val, average_type = "geometric"
    )
    
    # Print intermediate results for visibility
    cat(sprintf("n=%-4d | m=%-5d | Arithmetic Price: %-8.4f EUR | Geometric Price: %.4f EUR\n", 
                n_val, m_val, price_arithmetic, price_geometric))
    
    # Store results
    results_df <- rbind(results_df, data.frame(
      n = n_val,
      m = m_val,
      Arithmetic_Price = round(price_arithmetic, 4),
      Geometric_Price = round(price_geometric, 4)
    ))
  }
}

cat("\n--- Final Results Table ---\n")
print(results_df)
