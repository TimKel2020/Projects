set.seed(1)

x <- 1:100 # Or x <- seq(1, 100, by = 1)  
e <- rnorm(length(x), mean = 0, sd = sqrt(0.04)) # Generate noise from N(0, 0.04)
y <- sin(x/10) + e # Compute y as a noisy sine wave

# Print first few values to check
head(data.frame(x, y))