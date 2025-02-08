#### Generating Simulated Data ####

set.seed(1)

x <- 1:100 # Or x <- seq(1, 100, by = 1)  
e <- rnorm(length(x), mean = 0, sd = 0.2) # Generate noise from N(0, 0.04)
y <- sin(x/10) + e # Compute y as a noisy sine wave

# Print first few values to check
head(data.frame(x, y))

#### Implementing the Lowess Algo ####

customLowess <- f(x, y, f){
  
  n = nrow(x)
  
  k = min(n, ceiling(f*n))
  
  y_smooth <- NULL
  
  for (i in n) {
    
    # Compute distances and find closest neighbors
    distances <- abs(x - x[i])
    neighbor_indices <- order(distances)[1:k]
    
    # finding max distance
    dmax <- neighbor_indices[k]
    
    # Replace elements not in neighbour_indices with 0
    filtered_distances <- rep(0, length(distances))  # Initialize with 0s
    filtered_distances[neighbour_indices] <- distances[neighbour_indices]  # Keep values at neighbour indices
    
    # finding weights
    weights <- (1 - (filtered_distances/dmax)^3)^3
    
    # Weighted regression matrix W
    W = diag(weights)
    
    # Matrices X and Y
    X <- cbind(1, as.matrix(x))  # Add intercept column
    Y <- as.matrix(y)
    
    # Optimal Beta 
    Beta = solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y
    
    # smoothed value
    y <- Beta[0] + Beta[1]*x
    
    y_smooth <- c(y_smooth, y) 
    
  }
  
  return(y_smooth)
  
}

customLowess(x, y, 0.5)

#### Comparing with R's built-in lowess() ####

lowess(x, y, f = 0.5, iter = 0)
