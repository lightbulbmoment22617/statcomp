#### Numerical maximum likelihood estimation ####

# Simulation study:
# - S: Simulate; known true model parameters theta_true, simulate data
# - E: Estimate; pretend theta_true are not known, estimate them using the data
# - A: Assess; compare the theta estimates with theta_true

# Simulate data

# True model is Gamma(2, 1)
y <- rgamma(100, shape = 2, scale = 1)
plot(y) # Simple data plot
plot.ecdf(y) # Empirical cumulative distribution function

# Optimisation target

f <- function(theta, y) {
  shape <- exp(theta[1]);
  scale <- exp(theta[2]);
  -sum(dgamma(y, shape = shape, scale = scale, log = TRUE))
}
# Estimate/optimise!

opt <- optim(c(0,0), f, y = y)

# Inspect the result

exp(opt$par)

# How close should we expect the estimate to be to the true parameter values?
# We need methods for assessing the difference between estimates and truth.
