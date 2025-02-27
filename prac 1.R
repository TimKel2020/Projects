#### PRACTICAL ONE ####

# 1. Find all rows in “airquality” that have missing values.

missing_rows <- airquality[!complete.cases(airquality), ]
print(missing_rows)

# 2. Find mean, sd, min, max for each of temperature and ozone level.

summary_stats <- data.frame(
  Variable = c("Temperature", "Ozone"),
  Mean = c(mean(airquality$Temp, na.rm = TRUE), 
           mean(airquality$Ozone, na.rm = TRUE)),
  SD = c(sd(airquality$Temp, na.rm = TRUE), 
         sd(airquality$Ozone, na.rm = TRUE)),
  Min = c(min(airquality$Temp, na.rm = TRUE), 
          min(airquality$Ozone, na.rm = TRUE)),
  Max = c(max(airquality$Temp, na.rm = TRUE), 
          max(airquality$Ozone, na.rm = TRUE))
)

print(summary_stats)

# 3. The cars data (an R data set, also always available in R) contains two variables: speed and distance to stop. Fit a simple linear regression model to these data, i.e. find the  Beta estimates, using the Beta equation, and matrix calcuations in R.

X <- cbind(1, as.matrix(cars[, 1]))  # Add intercept column
Y <- as.matrix(cars[, 2])

f <- function(Y, X) {
  
  # Beta coefficients
  Beta = solve(t(X)%*%X)%*%t(X)%*%Y
  
  # Residual sum of squares 
  RSS = t(Y-X%*%Beta)%*%(Y-X%*%Beta)
  
  # Number of obs (n) and number of independent variables/predictors (k)
  n = nrow(X)
  k = ncol(X)
  
  # Mean square error 
  MSE = as.numeric(RSS)/(n-k)
  
  # Standard Errors of Beta coefficients
  SE = sqrt(diag(MSE * solve(t(X) %*% X)))
  
  # t-values
  t_values = Beta / SE
  
  # p-values (two-tailed test)
  p_values = 2 * pt(-abs(t_values), df = n - k)
  
  # Return results as a list
  return(list(
    Beta = Beta,
    RSS = RSS,
    MSE = MSE,
    SE = SE,
    t_values = t_values,
    p_values = p_values
  ))
  
}

f(Y, X)

# 4. Check that you get the same Beta estimates as when fitting the linear regression model using lm() in R.

model <- lm(dist~speed, data = cars)
summary(model)

