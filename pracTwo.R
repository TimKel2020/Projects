#### Generating Simulated Data ####

set.seed(1)

x <- 1:100 # Or x <- seq(1, 100, by = 1)  
e <- rnorm(length(x), mean = 0, sd = 0.2) # Generate noise from N(0, 0.04)
y <- sin(x/10) + e # Compute y as a noisy sine wave

# Print first few values to check
head(data.frame(x, y))

#### Implementing the Lowess Algo ####

customLowess <- function(x, y, f) {
  
  n = nrow(as.matrix(x))
  
  k = min(n, ceiling(f*n)) + 1 # + 1 to not count xi
  
  y_smooth <- NULL
  
  # Matrices X and Y
  X <- cbind(1, as.matrix(x))  # Add intercept column
  Y <- as.matrix(y)
  
  for (i in 1:n) {
    
    # Compute distances and find closest neighbors
    distances <- abs(x - x[i])
    neighbour_indices <- order(distances)[1:k]
    
    # finding max distance
    dmax <- max(distances[neighbour_indices])
    
    # finding weights
    weights <- (1 - (distances/dmax)^3)^3
    
    # Apply the condition: If |x_j - x_i| >= dmax, set weight to 0
    weights[distances >= dmax] <- 0
    
    # Weighted regression matrix W
    W = diag(weights)
    
    # Optimal Beta 
    Beta = solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y
    
    # smoothed value
    y <- Beta[1] + Beta[2]*x[i]
    
    #y_smooth <- c(y_smooth, y) 
    y_smooth[i] <- Beta[1] + Beta[2] * x[i]
    
  }
  
  return(y_smooth)
  
}

y_smooth <- customLowess(x, y, 0.05)

# Create the plot first
plot(x, y, col = "blue", pch = 16, main = "LOWESS Smoothing")  # Scatter plot of original data

# Add smoothed curve
lines(x, y_smooth, col = "red", lwd = 2)  # Add smoothed curve in red

#### Comparing with R's built-in lowess() ####

# Create the plot first
plot(x, y, col = "blue", pch = 16, main = "LOWESS Smoothing")  # Scatter plot of original data

lines(lowess_result <- lowess(x, y, f = 0.05, iter = 0))

# Plot results
plot(x, y, pch = 16, col = "gray", main = "LOWESS Smoothing Comparison", xlab = "x", ylab = "y")
lines(x, y_smooth, col = "blue", lwd = 2, lty = 2)
lines(lowess_result, col = "red", lwd = 2, lty = 3)
legend("topright", legend = c("Custom LOWESS", "Built-in LOWESS"), col = c("blue", "red"), lty = c(2, 3), lwd = 2, cex = 0.8)

