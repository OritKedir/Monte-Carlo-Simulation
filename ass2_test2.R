# --- 1. Define Parameters ---

T_exp <- 5 / 12       # Time to expiration (5 months in years)
sigma <- 0.2          # Volatility
r <- 0.06             # Annual interest rate
N <- 200              # Number of time steps
K <- 50               # Strike price (EUR)
S0_values <- c(40, 45, 50) # Initial stock prices

# Derived parameters
h <- T_exp / N        # Time step length
discount_factor <- (1 + r * h)^(-N) # Discrete compounding discount factor

# --- 2. Monte Carlo Simulation Function ---

# Function to calculate the Monte Carlo Estimate (MCE)
# S0: initial stock price
# m: number of trajectories (simulations)
monte_carlo_option_price <- function(S0, m) {
  
  # Pre-allocate matrix to store final stock prices S(T)
  # Each row is a trajectory, column 1 holds S(T)
  ST_matrix <- matrix(NA, nrow = m, ncol = 1)
  
  # Perform m trajectories
  for (j in 1:m) {
    
    # Start the trajectory at S0
    S_t <- S0
    
    # Generate N standard normal random variables (one for each step)
    z <- rnorm(N)
    
    # Evolve the stock price over N steps
    for (i in 1:N) {
      # Discretized Geometric Brownian Motion using provided formula
      S_t <- S_t + r * S_t * h + sigma * S_t * z[i] * sqrt(h)
    }
    
    # Store the final price S(T)
    ST_matrix[j, 1] <- S_t
  }
  
  # Calculate the payoff for each trajectory: max(S(T) - K, 0)
  payoffs <- pmax(ST_matrix - K, 0)
  
  # Calculate the average payoff
  average_payoff <- mean(payoffs)
  
  # Calculate the Monte Carlo estimate: Present Value of Average Payoff
  MCE <- discount_factor * average_payoff
  
  return(MCE)
}

# --- 3. Execute Simulation and Tabulate Results ---

# Initialize a data frame for the results
results_table <- data.frame(
  S0 = paste0("V_hat(", S0_values, ")"),
  MCE_100 = numeric(length(S0_values)),
  MCE_1000 = numeric(length(S0_values))
)

# Run the simulations
for (i in 1:length(S0_values)) {
  S0 <- S0_values[i]
  
  # Calculate MCE(100)
  mce100 <- monte_carlo_option_price(S0, m = 100)
  results_table[i, "MCE_100"] <- mce100
  
  # Calculate MCE(1000)
  mce1000 <- monte_carlo_option_price(S0, m = 1000)
  results_table[i, "MCE_1000"] <- mce1000
}

# --- 4. Display Results in the Requested Format ---

# Rename columns for the final display
colnames(results_table) <- c("", "MCE(100)", "MCE(1000)")

# Print the final table (rounded to 4 decimal places for presentation)
cat("Monte Carlo Estimate for a 5-month European Call Option:\n\n")
print(format(results_table, digits = 4, nsmall = 4), row.names = FALSE)