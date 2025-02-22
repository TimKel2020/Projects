# Question 0: Bergit's added questions
## i - Simulate a, b and c with probabilities 0.2, 0.5 and 0.3
x = runif(1000, min = 0, max = 1)

simulated_data <- ifelse(x <= 0.2, "a",
                         ifelse(x <= 0.7, "b",
                         ifelse(x <= 1, "c")))


simulated_data <- factor(simulated_data, levels = c("a", "b", "c"))

table(simulated_data)
table(simulated_data)/1000

barplot(table(simulated_data)/100)

## ii


## iii


# Question 1: Likelihood
x = c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28, 1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, 0.52, 2.50)

n = length(x)

theta = seq(-pi,pi,length=100)

log_likelihood_function <- function(x, n, theta){
  
  log_likelihood = rep(NA, n=100)
  
  for(i in 1:100){
    log_likelihood[i] <- -n*log(2*pi) + sum(log(1 - cos(x-theta[i])))
  }
  
  return(log_likelihood)
  
}

log_likelihood <- log_likelihood_function(x, n, theta)

plot(x = theta, y = log_likelihood)

df <- data.frame(x = theta, y = log_likelihood)
ggplot(df, aes(x = theta, y = log_likelihood)) +
  geom_line()

theta = seq(-pi,pi,length=100)
L-values <- sapply(theta, log_likelihood_function, x = x)

# Question 2: Mixture Distribution

x = runif(5000, min = 0, max = 1)
mix <- NA
  
for(i in 1:5000){
  mix[i] <- ifelse(x[i] <= 0.75, rnorm(n = 1, mean = 0, sd = 1),
                   ifelse(x[i] <= 1, rnorm(n = 1, mean = 3, sd = 1)))
}

# Improved histogram with density overlay
hist(X, breaks = 30, probability = TRUE, col = "gray", border = "black",
     main = "Random Sample from Mixture Distribution", xlab = "x", ylab = "Density")

# Overlay kernel density estimate in purple
lines(density(X), col = "purple", lwd = 2)

# Overlay theoretical density in blue
curve(0.75 * dnorm(x, 0, 1) + (1 - 0.75) * dnorm(x, 3, 1), 
      col = "blue", lwd = 2, add = TRUE, lty = 2)

# Question 3: Uniform Distribution on the Sphere

d = 2
n = 200
X <- matrix(rnorm(d * n, mean = 0, sd = 1), nrow = n, ncol = d)

# Compute row-wise norms
row_norms <- sqrt(rowSums(X^2)) # X%*%X

# Normalize each row
X_unit <- X / row_norms  #X_unit[i, j] = X[i, j] / row_norms[i]

# plotting
plot(X_unit, xlab = "Dimension 1", ylab = "Dimension 2", 
     main = "Scatter Plot of Normalized Rows", col = "blue", pch = 16)
abline(h = 0, v = 0, col = "gray", lty = 2)  # Add reference lines


# Avoid division by zero
# row_norms[row_norms == 0] <- 1

for(i in 1:d*n){
  X <- matrix(rnorm(d * n, mean = 0, sd = 1), nrow = d, ncol = n)
  
  # Compute row-wise norms
  row_norms <- sqrt(rowSums(X^2))
  
  # Avoid division by zero
  row_norms[row_norms == 0] <- 1
  
  # Normalize each row
  X_unit <- X / row_norms
  U = rep(NA, n)
  
  deno = sqrt(as.numeric(t(X)%*%X))
  U[i] = c(X[i],X[i+1])/deno
}

# Question 4: Quantiles

set.seed(42)  # For reproducibility

# Step 1: Generate 500 standard normal variables using Box-Muller transform
n <- 100 * 5  # 100 chi-square samples, each from sum of 5 N(0,1)^2
U <- runif(n/2)  # Generate U ~ U(0,1)
V <- runif(n/2)  # Generate V ~ U(0,1)

Z1 <- sqrt(-2 * log(U)) * cos(2 * pi * V)  # Standard normal
Z2 <- sqrt(-2 * log(V)) * sin(2 * pi * U)  # Standard normal

Z <- c(Z1, Z2)  # Combine Z1 and Z2

# Step 2: Generate chi-square (df=5) by summing squares of 5 independent normals
X_chisq5 <- rowSums(matrix(Z[1:(100*5)], nrow=100, ncol=5)^2)

# Step 3: Compare sample quantiles with theoretical quantiles
qqplot(qchisq(ppoints(100), df=5), X_chisq5, main="QQ-Plot: Sample vs Theoretical Quantiles",
       xlab="Theoretical Quantiles (Chi-Square df=5)", ylab="Sample Quantiles")
abline(0, 1, col="red")  # Reference line



# Question 5: Simulate from Linear Model

x = runif(200, min = -1, max = 1)
e = rnorm(200, mean = 0, sd = 2.5)
y = 2-3*x+e
X = cbind(1,x)

B = solve(t(X)%*%X)%*%t(X)%*%y
B

data <- data.frame(x,y)
lm(y~x)

plot(y~x)
# Question 6: Simulate from model



# Question 7: Least Squares Seals



# Question 8: Randomization Test



# Question 9: QQ-plot


