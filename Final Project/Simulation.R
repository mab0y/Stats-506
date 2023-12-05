library(AER)  # for ivreg function

n <- 1000  # sample size
num_simulations <- 1000  # number of simulations

# Function to run a single simulation for univariate case
run_simulation_uni <- function(cor_coef, sd_error) {
  avg_coef <- mean(replicate(num_simulations, {
    Z <- rnorm(n)
    error <- cor_coef * Z + sqrt(sd_error^2 - cor_coef^2) * rnorm(n)
    X <- 0.5*Z + rnorm(n) + 0.1*error
    Y <- 1 + 2 * X + error
    model <- ivreg(Y ~ X | Z) 
    return(coef(model)[2] - 2)
  }))
  return(avg_coef)
}

# Range of correlation coefficients and error term standard deviations
cor_coefs <- seq(-0.9, 0.9, by = 0.2)
sd_errors <- seq(1, 5, by = 1)

# Initialize the results matrix
results_uni <- matrix(NA, nrow = length(cor_coefs), ncol = length(sd_errors))
rownames(results_uni) <- paste("Correlation:", cor_coefs)
colnames(results_uni) <- paste("SD Error:", sd_errors)

# Run simulations
for (i in 1:length(cor_coefs)) {
  for (j in 1:length(sd_errors)) {
    results_uni[i, j] <- run_simulation_uni(cor_coefs[i], sd_errors[j])
  }
}


# Function to run a single simulation for multivariate case
run_simulation_multi <- function(cor_coef, sd_error) {
  avg_coefs <- rowMeans(replicate(num_simulations, {
    Z <- rnorm(n)
    error <- cor_coef * Z + sqrt(sd_error^2 - cor_coef^2) * rnorm(n)
    X1 <- 0.5*Z + rnorm(n) + 0.1*error
    X2 <- rnorm(n)
    Y <- 1 + 2 * X1 + 3 * X2 + error
    model <- ivreg(Y ~ X1 + X2 | Z + X2)
    return(c(coef(model)[2] - 2, coef(model)[3]) - 3)
  }))
  return(avg_coefs)
}

# Initialize results matrices for endogenous and exogenous variables
results_endogenous <- matrix(NA, nrow = length(cor_coefs), ncol = length(sd_errors))
results_exogenous <- matrix(NA, nrow = length(cor_coefs), ncol = length(sd_errors))
rownames(results_endogenous) <- paste("Correlation:", cor_coefs)
rownames(results_exogenous) <- paste("Correlation:", cor_coefs)
colnames(results_endogenous) <- paste("SD Error:", sd_errors)
colnames(results_exogenous) <- paste("SD Error:", sd_errors)

# Run simulations
for (i in 1:length(cor_coefs)) {
  for (j in 1:length(sd_errors)) {
    coefs <- run_simulation_multi(cor_coefs[i], sd_errors[j])
    results_endogenous[i, j] <- coefs[1]
    results_exogenous[i, j] <- coefs[2]
  }
}