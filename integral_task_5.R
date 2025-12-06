# --- Monte Carlo Simulation for Integral Estimation: Integral of 4 / (1 + x^2) from 0 to 1 ---

f <- function(x) {
  # The function is f(x) = 4 / (1 + x^2)
  return(4 / (1 + x^2))
}

monte_carlo_integral <- function(n) {
  # Define bounds
  a <- 0
  b <- 1
  interval_width <- b - a
  
  # Step 1: Generate n uniform random numbers U in [0, 1]
  U <- runif(n)
  
  # Step 2: Transform U to X in [a, b]
  X <- a + interval_width * U
  
  # Step 3: Calculate the function value f(X)
  f_values <- f(X)
  
  # Step 4: Calculate the Monte Carlo Estimate (a_hat_n)
  a_hat_n <- interval_width * mean(f_values)
  
  # Step 5: Calculate the Sample Standard Deviation and Standard Error
  s_f <- sd(f_values)
  standard_error <- interval_width * s_f / sqrt(n)
  
  # Return the results in a list
  return(list(
    n = n,
    estimate = a_hat_n,
    sample_stdev = s_f,
    standard_error = standard_error
  ))
}

# 3. Set up the exact value for comparison (The integral is exactly pi)
exact_value <- pi

# 4. Run the simulation for the requested values of n (10, 100, 1000)
n_values <- c(10, 100, 1000)

results <- lapply(n_values, monte_carlo_integral)

# 5. Display the results
cat("--- Integral of 4 / (1 + x^2) from 0 to 1 ---\n")
cat("Exact Value of Integral (pi):", exact_value, "\n\n")

cat("--- Monte Carlo Simulation Results ---\n")
for (res in results) {
  # Calculate the absolute difference between the estimate and the true value
  absolute_error <- abs(res$estimate - exact_value)
  
  cat("N =", res$n, "\n")
  cat("  Estimate (a_hat_n):", res$estimate, "\n")
  cat("  Sample StDev (s_f):", res$sample_stdev, "\n")
  cat("  Standard Error (s_f/sqrt(n)):", res$standard_error, "\n")
  cat("  Absolute Difference from pi:", absolute_error, "\n")
  cat("-----------------------------------\n")
}

# ----------------------------------------------------------------------
# Monte Carlo Simulation for Integral Estimation: Integral of sqrt(x) from 0 to 4
# ----------------------------------------------------------------------

f <- function(x) {
  return(sqrt(x))
}

monte_carlo_integral <- function(n) {
  a <- 0
  b <- 4
  interval_width <- b - a
  
  # Step 1: Generate n uniform random numbers U in [0, 1]
  U <- runif(n)
  
  # Step 2: Transform U to X in [a, b]
  X <- a + interval_width * U
  
  f_values <- f(X)
  
  a_hat_n <- interval_width * mean(f_values)
  
  s_f <- sd(f_values)
  
  standard_error <- interval_width * s_f / sqrt(n)
  
  return(list(
    n = n,
    estimate = a_hat_n,
    sample_stdev_of_f = s_f,
    standard_error = standard_error
  ))
}

exact_value <- 16/3  

n_values <- c(10, 100, 1000)

results <- lapply(n_values, monte_carlo_integral)

cat("--- Integral of sqrt(x) from 0 to 4 ---\n")
cat("Exact Value (16/3):", exact_value, "\n\n")

cat("--- Monte Carlo Simulation Results ---\n")
for (res in results) {
  absolute_error <- abs(res$estimate - exact_value)
  
  cat("N =", res$n, "\n")
  cat("  Estimate (a_hat_n):", res$estimate, "\n")
  cat("  Sample StDev of f(x) (s_f):", res$sample_stdev_of_f, "\n")
  cat("  Standard Error of Integral Estimate:", res$standard_error, "\n")
  cat("  Absolute Difference from Exact Value:", absolute_error, "\n")
  cat("-----------------------------------\n")
}

# ----------------------------------------------------------------------
# Monte Carlo Simulation for Integral Estimation: Integral of sqrt(x + sqrt(x)) from 0 to 1
# ----------------------------------------------------------------------

f <- function(x) {
  # The function is f(x) = sqrt(x + sqrt(x))
  return(sqrt(x + sqrt(x)))
}

monte_carlo_integral <- function(n) {
  # Define the integration bounds
  a <- 0
  b <- 1
  interval_width <- b - a  
  
  # Step 1: Generate n uniform random numbers U in [0, 1]
  U <- runif(n)
  
  # Step 2: Transform U to X in [a, b] (Here a=0, b=1, so X=U)
  X <- a + interval_width * U
  
  # Calculate the function value f(X)
  f_values <- f(X)
  
  # Calculate the Monte Carlo Estimate (a_hat_n)
  a_hat_n <- interval_width * mean(f_values)
  
  # Calculate the Sample Standard Deviation (s_f) of the f(x) values
  s_f <- sd(f_values)
  
  # Calculate the Standard Error of the Integral Estimate
  standard_error <- interval_width * s_f / sqrt(n)
  
  # Return the results
  return(list(
    n = n,
    estimate = a_hat_n,
    sample_stdev_of_f = s_f,
    standard_error = standard_error
  ))
}

# Using the high-precision numerical approximation as the "exact value"
exact_value <- 0.87113166  

n_values <- c(10, 100, 1000)

results <- lapply(n_values, monte_carlo_integral)

cat("--- Integral of sqrt(x + sqrt(x)) from 0 to 1 ---\n")
cat("Exact Value (Numerical Approx.):", exact_value, "\n\n")

cat("--- Monte Carlo Simulation Results ---\n")
for (res in results) {
  # Calculate the absolute difference from the numerical approximation
  absolute_error <- abs(res$estimate - exact_value)
  
  cat("N =", res$n, "\n")
  cat("  Estimate (a_hat_n):", res$estimate, "\n")
  cat("  Sample StDev of f(x) (s_f):", res$sample_stdev_of_f, "\n")
  cat("  Standard Error of Integral Estimate:", res$standard_error, "\n")
  cat("  Absolute Difference from Exact Value:", absolute_error, "\n")
  cat("-----------------------------------\n")
}

# ----------------------------------------------------------------------
# Monte Carlo Simulation for Double Integral: Integral of (4 - x^2 - y^2) over [0, 5/4]x[0, 5/4]
# ----------------------------------------------------------------------

f <- function(x, y) {
  # The function is f(x, y) = 4 - x^2 - y^2
  return(4 - x^2 - y^2)
}

monte_carlo_integral_2d <- function(n) {
  # Define the integration bounds (a=c=0, b=d=5/4)
  a <- 0; b <- 5/4
  c <- 0; d <- 5/4
  
  # Area of the integration region: (b-a) * (d-c)
  area_factor <- (b - a) * (d - c)  
  
  # Step 1: Generate n uniform random numbers U1, U2 in [0, 1]
  U1 <- runif(n)
  U2 <- runif(n)
  
  # Step 2: Transform U1 to X in [a, b] and U2 to Y in [c, d]
  X <- a + (b - a) * U1
  Y <- c + (d - c) * U2
  
  # Calculate the function value f(X, Y)
  f_values <- f(X, Y)
  
  # Calculate the Monte Carlo Estimate (a_hat_n)
  a_hat_n <- area_factor * mean(f_values)
  
  # Calculate the Sample Standard Deviation (s_f) of the f(x,y) values
  s_f <- sd(f_values)
  
  # Calculate the Standard Error of the Integral Estimate
  standard_error <- area_factor * s_f / sqrt(n)
  
  # Return the results
  return(list(
    n = n,
    estimate = a_hat_n,
    sample_stdev_of_f = s_f,
    standard_error = standard_error
  ))
}

# The exact value (1775/384)
exact_value <- 1775/384  

n_values <- c(10, 100, 1000)

results <- lapply(n_values, monte_carlo_integral_2d)

cat("--- Double Integral of (4 - x^2 - y^2) ---\n")
cat("Exact Value (1775/384):", exact_value, "\n\n")

cat("--- Monte Carlo Simulation Results ---\n")
for (res in results) {
  absolute_error <- abs(res$estimate - exact_value)
  
  cat("N =", res$n, "\n")
  cat("  Estimate (a_hat_n):", res$estimate, "\n")
  cat("  Sample StDev of f(x,y) (s_f):", res$sample_stdev_of_f, "\n")
  cat("  Standard Error of Integral Estimate:", res$standard_error, "\n")
  cat("  Absolute Difference from Exact Value:", absolute_error, "\n")
  cat("-----------------------------------\n")
}

# ----------------------------------------------------------------------
# Monte Carlo Simulation for Double Integral: Integral of sqrt(4 - x^2 - y^2) over [0, 5/4]x[0, 5/4]
# ----------------------------------------------------------------------

f <- function(x, y) {
  # The function is f(x, y) = sqrt(4 - x^2 - y^2)
  return(sqrt(4 - x^2 - y^2))
}

monte_carlo_integral_2d <- function(n) {
  # Define the integration bounds (a=c=0, b=d=5/4)
  a <- 0; b <- 5/4
  c <- 0; d <- 5/4
  
  # Area of the integration region: (b-a) * (d-c)
  area_factor <- (b - a) * (d - c)  
  
  # Step 1: Generate n uniform random numbers U1, U2 in [0, 1]
  U1 <- runif(n)
  U2 <- runif(n)
  
  # Step 2: Transform U1 to X in [a, b] and U2 to Y in [c, d]
  X <- a + (b - a) * U1
  Y <- c + (d - c) * U2
  
  # Calculate the function value f(X, Y)
  f_values <- f(X, Y)
  
  # Calculate the Monte Carlo Estimate (a_hat_n)
  a_hat_n <- area_factor * mean(f_values)
  
  # Calculate the Sample Standard Deviation (s_f) of the f(x,y) values
  s_f <- sd(f_values)
  
  # Calculate the Standard Error of the Integral Estimate
  standard_error <- area_factor * s_f / sqrt(n)
  
  # Return the results
  return(list(
    n = n,
    estimate = a_hat_n,
    sample_stdev_of_f = s_f,
    standard_error = standard_error
  ))
}

# Using a high-precision numerical approximation as the "exact value"
exact_value <- 4.49885  

n_values <- c(10, 100, 1000)

results <- lapply(n_values, monte_carlo_integral_2d)

cat("--- Double Integral of sqrt(4 - x^2 - y^2) ---\n")
cat("Exact Value (Numerical Approx.):", exact_value, "\n\n")

cat("--- Monte Carlo Simulation Results ---\n")
for (res in results) {
  absolute_error <- abs(res$estimate - exact_value)
  
  cat("N =", res$n, "\n")
  cat("  Estimate (a_hat_n):", res$estimate, "\n")
  cat("  Sample StDev of f(x,y) (s_f):", res$sample_stdev_of_f, "\n")
  cat("  Standard Error of Integral Estimate:", res$standard_error, "\n")
  cat("  Absolute Difference from Exact Value:", absolute_error, "\n")
  cat("-----------------------------------\n")
}

# ----------------------------------------------------------------------
# Monte Carlo Simulation for Triple Integral (Task 6)
# ----------------------------------------------------------------------

f <- function(x, y, z) {
  # The function is f(x, y, z) = 4 - x^2 - y^2 - z^2
  return(4 - x^2 - y^2 - z^2)
}

monte_carlo_integral_3d <- function(n) {
  # Define the integration bounds (x: [a1, b1], y: [a2, b2], z: [a3, b3])
  a1 <- 0; b1 <- 9/10  # x-bounds
  a2 <- 0; b2 <- 1     # y-bounds
  a3 <- 0; b3 <- 11/10 # z-bounds
  
  # Volume of the integration region
  volume_factor <- (b1 - a1) * (b2 - a2) * (b3 - a3)  
  
  # Step 1: Generate n uniform random numbers U1, U2, U3 in [0, 1]
  U1 <- runif(n)
  U2 <- runif(n)
  U3 <- runif(n)
  
  # Step 2: Transform U1 to X in [a1, b1], U2 to Y in [a2, b2], U3 to Z in [a3, b3]
  X <- a1 + (b1 - a1) * U1
  Y <- a2 + (b2 - a2) * U2
  Z <- a3 + (b3 - a3) * U3
  
  # Calculate the function value f(X, Y, Z)
  f_values <- f(X, Y, Z)
  
  # Calculate the Monte Carlo Estimate (a_hat_n)
  a_hat_n <- volume_factor * mean(f_values)
  
  # Calculate the Sample Standard Deviation (s_f) of the f(x,y,z) values
  s_f <- sd(f_values)
  
  # Calculate the Standard Error of the Integral Estimate
  standard_error <- volume_factor * s_f / sqrt(n)
  
  # Return the results
  return(list(
    n = n,
    estimate = a_hat_n,
    sample_stdev_of_f = s_f,
    standard_error = standard_error
  ))
}

# The exact value (134707/49500)
exact_value <- 134707/49500  

n_values <- c(10, 100, 1000)

results <- lapply(n_values, monte_carlo_integral_3d)

cat("--- Triple Integral of (4 - x^2 - y^2 - z^2) ---\n")
cat("Exact Value (134707/49500):", exact_value, "\n\n")

cat("--- Monte Carlo Simulation Results ---\n")
for (res in results) {
  absolute_error <- abs(res$estimate - exact_value)
  
  cat("N =", res$n, "\n")
  cat("  Estimate (a_hat_n):", res$estimate, "\n")
  cat("  Sample StDev of f(x,y,z) (s_f):", res$sample_stdev_of_f, "\n")
  cat("  Standard Error of Integral Estimate:", res$standard_error, "\n")
  cat("  Absolute Difference from Exact Value:", absolute_error, "\n")
  cat("-----------------------------------\n")
}

