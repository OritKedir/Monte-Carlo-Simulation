#Assignment 1


# A common LCG setup with a large period for demonstration.
m <- 2^31        # Modulus
a <- 1103515245  # Multiplier
c <- 12345       # Increment

# LCG function: generates uniform [0, 1] numbers
LCG_generate <- function(n, x0, a, c, m) {
  x <- numeric(n)
  x_prev <- x0
  
  for (i in 1:n) {
    # LCG formula: X_i = (a * X_{i-1} + c) mod m
    x_current <- (a * x_prev + c) %% m
    x[i] <- x_current
    x_prev <- x_current
  }
  
  # Convert integers to uniform [0, 1] variables
  uniform_values <- x / m
  return(uniform_values)
}

# --- Generate U and V (length 1000) ---
N <- 1000 
seed_U <- 123456 # Different seeds ensure independence
seed_V <- 789012 

# Generate U = {U_1, ..., U_1000}
U <- LCG_generate(N, seed_U, a, c, m)

# Generate V = {V_1, ..., V_1000}
V <- LCG_generate(N, seed_V, a, c, m)

# --- Plot Histograms ---

# Set up two plots side-by-side
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1) + 0.1)

# Histogram for U
hist(U, 
     main = expression(paste("Histogram of ", U)), 
     xlab = "U (LCG Output)", 
     col = "lightblue", 
     breaks = 20)

# Histogram for V
hist(V, 
     main = expression(paste("Histogram of ", V)), 
     xlab = "V (LCG Output)", 
     col = "lightgreen", 
     breaks = 20)

# Reset plot layout
par(mfrow = c(1, 1))

# --- Verification ---
cat("First 5 values of U:\n")
print(head(U, 5))
cat("\nFirst 5 values of V:\n")
print(head(V, 5))


#Assignmnet 3
# --- LCG Helper Function ---
# Generates a sequence of length n with period determined by m.
LCG_generate_sequence <- function(n, a, c, m, x0) {
  x <- numeric(n)
  x_prev <- x0
  
  for (i in 1:n) {
    # LCG: X_i = (a * X_{i-1} + c) mod m
    x_current <- (a * x_prev + c) %% m
    x[i] <- x_current
    x_prev <- x_current
  }
  
  # Convert integers to uniform [0, 1] numbers
  uniform_values <- x / m
  return(uniform_values)
}

# --- A. Sequence Repeats from the 6th value (Period p = 5) ---

# Choose m=5 for period 5. Parameters a=6, c=1 guarantee full period.
m_a <- 5     
a_a <- 6     
c_a <- 1     
x0_a <- 3    # Seed

# Generate the sequence of 30 numbers
U_a <- LCG_generate_sequence(n = 30, a = a_a, c = c_a, m = m_a, x0 = x0_a)

cat("A. Sequence (repeats from 6th value, Period = 5):\n")
cat("Parameters: m=5, a=6, c=1, x0=3\n")
print(matrix(U_a, ncol = 5, byrow = TRUE))

cat("\n---")
# --- B. Sequence Repeats from the 11th value (Period p = 10) ---

# Choose m=10 for period 10. Parameters a=11, c=3 guarantee full period.
m_b <- 10    
a_b <- 11    
c_b <- 3     
x0_b <- 7    # Seed

# Generate the sequence of 30 numbers
U_b <- LCG_generate_sequence(n = 30, a = a_b, c = c_b, m = m_b, x0 = x0_b)

cat("\nB. Sequence (repeats from 11th value, Period = 10):\n")
cat("Parameters: m=10, a=11, c=3, x0=7\n")
print(matrix(U_b, ncol = 10, byrow = TRUE))

# ASSIGNMENT 4
# ---  Inverse CDF for Arcsine Distribution ---
# Based on the identity: X = 1/2 - 1/2 * cos(U*pi)
inverse_arcsine_cdf <- function(u) {
  # u is a vector of uniform random numbers
  return(0.5 - 0.5 * cos(u * pi))
}

# --- Generation of Random Variable Y ---
N <- 1000  # Sample size

# 1. Generate N uniform random numbers U ~ Uniform(0, 1)
U <- runif(N)

# 2. Apply the Simplified Inverse Transform: Y_i = F^{-1}(U_i)
Y <- inverse_arcsine_cdf(U)

# --- Draw a Histogram ---

# Set plot parameters
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Plot the histogram (U-shape expected)
hist(Y, 
     breaks = 50, # Use many breaks to show the U-shape
     freq = FALSE, # Plot density
     col = "darkseagreen", 
     border = "white",
     main = "Histogram of Arcsine Distribution (Simplified Transform)",
     xlab = expression(Y[i]),
     ylab = "Density")

# Optional: Overlay theoretical PDF for comparison
y_pdf <- seq(0.001, 0.999, length.out = 100)
f_arcsine <- 1 / (pi * sqrt(y_pdf * (1 - y_pdf)))
lines(y_pdf, f_arcsine, col = "darkblue", lwd = 2)

# Add legend
legend("top", 
       legend = c("Generated Data (Simplified)", "Theoretical PDF"),
       col = c("darkseagreen", "darkblue"),
       lwd = c(NA, 2),
       fill = c("darkseagreen", NA),
       bty = "n")

# --- Verification ---
cat("First 5 generated Arcsine random variables Y (using simplified transform):\n")
print(head(Y, 5))

#ASSIGNMNET 5

# --- Parameters ---
N <- 1000     # Total number of random variables to generate
mu <- 1       # Mean of the target distribution N(1, 4)
sigma <- 2    # Standard deviation: sqrt(4) = 2
N_pairs <- N / 2 # Number of uniform pairs needed

# --- 1. Generate Uniform Variables ---
# We need N/2 pairs of uniform numbers
U1 <- runif(N_pairs)
U2 <- runif(N_pairs)

# --- 2. Apply Box-Muller Transformation (Standard Normal N(0, 1)) ---

# Z1 = sqrt(-2*ln(U1)) * cos(2*pi*U2)
Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)

# Z2 = sqrt(-2*ln(U1)) * sin(2*pi*U2)
Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)

# Combine the two independent sequences (Z1 and Z2) into one vector
Z <- c(Z1, Z2)

# --- 3. Scale to Target Distribution N(1, 4) ---
# X = mu + sigma * Z
X <- mu + sigma * Z

# --- Draw a Histogram ---

# Set plot parameters
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Plot the histogram (bell-shape expected, centered at mu=1)
hist(X, 
     breaks = 30, # Sufficient breaks to visualize the shape
     freq = FALSE, # Plot density
     col = "darkorange", 
     border = "white",
     main = expression(paste("Histogram of N(1, 4) using Box-Muller (N=", 1000, ")")),
     xlab = expression(X[i]),
     ylab = "Density")


# Add legend
legend("topright", 
       legend = c("Generated Data", "Theoretical PDF"),
       col = c("darkorange", "blue"),
       lwd = c(NA, 2),
       fill = c("darkorange", NA),
       bty = "n")

# --- Verification ---
cat("First 5 generated N(1, 4) random variables X:\n")
print(head(X, 5))
cat("\nSample Mean:", mean(X))
cat("\nSample SD:", sd(X))


#ASSIGNMENT 6
# --- Parameters ---
a <- 1          # Lower bound
b <- 2          # Upper bound
n <- 1000       # Number of samples
exact_value <- -1 # Exact value calculated analytically

set.seed(123)
# --- 1. Define the Function ---
# f(x) = 5 - 16x^-3
f <- function(x) {
  return(5 - 16 * x^(-3))
}

# 2a. Generate n uniform random numbers in the unit interval [0, 1]
Z <- runif(n)

# 2b. Transform Z to uniform random numbers X in [a, b]
# X = a + (b - a) * Z, but b-a is 1
X <- a +  Z

# 2b. Evaluate the function at these points
f_X <- f(X)

# 2c. Calculate the Monte Carlo estimate (I_hat),but b-a=1
# I_hat = (1 * mean(f(X))
I_hat <-  mean(f_X)

# --- 3. Error Estimation ---

# 3a. Calculate the sample standard deviation of f(X)
s_f <- sd(f_X)

# 3b. Calculate the Standard Error (SE) of the Monte Carlo estimate
# SE(I_hat) = (b-a) / sqrt(n) * s_f and b-a is 1
SE_I_hat <- 1 / sqrt(n) * s_f

# --- 4. Results and Comparison ---

cat("--- Monte Carlo Integration of integral(1 to 2) (5 - 16x^-3) dx ---\n")
cat(paste("Sample size (n):", n, "\n"))
cat(paste("Integration interval (b - a):", (b - a), "\n"))
cat("------------------------------------------------------------------\n")

# Monte Carlo Estimate
cat(paste("Monte Carlo Estimate (I_hat):", round(I_hat, 4), "\n"))

# Error Estimate
cat(paste("Estimated Standard Error (SE):", round(SE_I_hat, 5), "\n"))
cat(paste("95% Confidence Interval: [", round(I_hat - 1.96*SE_I_hat, 4), ", ", round(I_hat + 1.96*SE_I_hat, 4), "]\n", sep=""))
cat(paste("Absolute Error: |I_hat - I|:", round(abs(I_hat - exact_value), 4), "\n"))


