#generate a smaple from a two dimensional standard normal distribution with 
#500 elements using the Box - Muller methodù; and transfer it into NORMAL distribution with specific y(15,12) 

# --- Box-Muller Transformation (2D Standard Normal, n=500) ---
n_samples <- 500

# 1. Generate Uniform Variables U1 and U2
# Using a small min to avoid log(0) and ensure statistical validity
U1 <- runif(n_samples) 
U2 <- runif(n_samples) 

# 2. Apply Box-Muller Transformation 

Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)

Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)

# 3. Combine Sample
Z <- cbind(Z1, Z2)

# --- Plotting the Standard Normal Distribution ---
par(mfrow = c(1, 3)) # Set up a 1x3 plotting layout

# Plot 1: Histogram of Z1
hist(Z1, 
     main = "Histogram of Z1 (Standard Normal)", 
     xlab = "Z1 Values", 
     col = "skyblue", 
     border = "black")

# Plot 2: Histogram of Z2
hist(Z2, 
     main = "Histogram of Z2 (Standard Normal)", 
     xlab = "Z2 Values", 
     col = "lightcoral", 
     border = "black")

# Plot 2: Histogram of Z2
hist(Z, 
     main = "Histogram of Z (Standard Normal)", 
     xlab = "Z2 Values", 
     col = "palegreen", 
     border = "black")

# Plot 3: Scatter Plot of the Combined 2D Sample
plot(Z, 
     main = "Scatter Plot of 2D Standard Normal", 
     xlab = "Z1 Values", 
     ylab = "Z2 Values", 
     pch = 20, 
     col = "#005a8d80") 

par(mfrow = c(1, 1)) # Reset plotting layout