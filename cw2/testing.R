# Import code given
source('~/StatComp/cw2/CWB2019code.R')

# Q1.1
negloglike <- function(param, Y) {
  if (param[1] < max(Y)) {
    return(+Inf)
  } else {
    return(sum(
      lgamma(Y+1),
      lgamma(param[1]-Y+1),
      -2*lgamma(param[1]+1),
      2*param[1]*log(1+exp(param[2])),
      -param[2]*sum(Y)
    ))
  }
}

# Q1.2
opt <- optim(par = c(257, logit(0.5)), fn = negloglike, Y = c(256, 237))
N_hat <- opt$par[1]
theta_hat <- opt$par[2]
# Obtain estimate of phi
phi_hat <- ilogit(theta_hat)
# TODO: Plot negloglik as we optimise

# Q1.3
hess <- optimHess(opt$par, fn = negloglike, Y = c(256, 237))
covar <- solve(hess)

SE_N <- sqrt(covar[1,1])
error <- qnorm(0.975)*SE_N
lwr <- N_hat-error
upr <- N_hat+error
N_CI <- c(lwr, upr)

# Q2.2
myhessian <- function(param, Y) {
  # Extract parameters
  N <- param[1]
  theta <- param[2]
  
  # Compute second order partial derivatives
  thetatwo <- 2 * N * exp(theta) / (1 + exp(theta))^2
  theta_n <- 2 * exp(theta) / (1 + exp(theta))
  ntwo <- psigamma(N - Y[1] + 1, 1) + psigamma(N - Y[2] + 1, 1) - 2 * psigamma(N + 1, 1)
  
  # Return Hessian
  return(matrix(c(ntwo, theta_n, theta_n, thetatwo), nrow = 2, ncol = 2))
}

myhess <- myhessian(opt$par, Y = c(256, 237))
# Q2.3
Y <- c(256, 237)
L0 <- negloglike(opt$par, Y)
L1 <- digamma(N_hat-Y[1]+1) / gamma(N_hat-Y[1]+1) 
       + digamma(N_hat-Y[2]+1) / gamma(N_hat-Y[2]+1)
       - 2*digamma(N_hat+1) / gamma(N_hat+1)
       + 2*log(1+exp(theta_hat))
L4 <- abs(sum(psigamma(N_hat-Y+1,3)) - 2*psigamma(N_hat+1,3))

e <- .Machine$double.eps
h <- 0.0001
bound <- e*(4*L0+2*abs(theta_hat)*L1) / h^2
         + (L4*h^2) / 12

# Q2.4
# TODO: Run multiple times for stability
bm <- microbenchmark::microbenchmark(myhessian(opt$par, Y = c(256, 237)),
                                     optimHess(opt$par, fn = negloglike, Y = c(256, 237)))

# Q3.1
arch_boot <- function(param, J) {
  boot_params <- matrix(0, J,2)
  N <- floor(param[1])
  phi <- ilogit(param[2])
  for (j in 1:J) {
    Y_j <- rbinom(2, N, phi)
    boot_params[j, ] <- optim(
      par = c(2*max(Y_j), 0),
      fn = negloglike,
      Y = Y_j)$par
  }
  return(boot_params)
}

# Q3.2
estimates <- arch_boot(opt$par, 10000)
errors <- sweep(estimates, 2, opt$par)
bias <- colMeans(errors)
std_dev_error <- apply(errors, 2, sd)

# Q3.3
temp <- estimates
temp[,1] <- log(temp[,1])
log_N_CI <- log(N_hat) - quantile(temp[,1] - log(N_hat),
                                  probs = c(0.975, 0.025))
theta_CI <- theta_hat - quantile(temp[,2] - theta_hat,
                                 probs = c(0.975, 0.025))
N_CI_2 <- exp(log_N_CI)
phi_CI <- ilogit(theta_CI)

# Q4.1
# In k-fold cross-validation we randomly split our data into k subsets
# of size ~N/k. In Step 1 we generate indices to define this random splitting
# of our training data.
#
# Here we perform k-fold cross-validation on the training data by choosing each
# of our k subsets as defined in Step 1, training a model using the provided
# formular on the remaining data and evaluating its performance on this subset.
# Once we have done this with each of our k subsets we return the k sets of
# scores we have obtained. These will be the SE, DS and Brier scores.
#
# Here we use the same formula we had used before: we train our model on the 
# entire training dataset and evaluate its performance on the testing set again
# by taking the SE, DS and Brier score.
#
# Here we return the combined corss-validation scores for each of the three
# scoring rules we have used. We also return the combined standard errors for
# each of the three scoring rules and finally the scores we obtained when building
# our model using the regular holdout method in Step 3.

# Q4.2
TMINdata <- read_TMIN_data("~/StatComp/cw2/")
J <- 20
S_boot_train <- data.frame(SE = numeric(J),
                           DS = numeric(J),
                           Brier = numeric(J))
S_boot_test <- data.frame(SE = numeric(J),
                          DS = numeric(J),
                          Brier = numeric(J))

for (j in 1:J) {
  boot_data <- data_list_resample(TMINdata)
  boot_scores <- cwb4_scores(boot_data, 10)
  S_boot_train[j,] <- boot_scores$cvk_mean
  S_boot_test[j,] <- boot_scores$test
}

# Q4.3
S_boot_train_mean <- mean_score(S_boot_train)
S_boot_test_mean <- mean_score(S_boot_test)

S_boot_train_sd <- lapply(S_boot_train, sd)
S_boot_test_sd <- lapply(S_boot_test, sd)

# Q4.4
#S_boot_train_matrix <- data.matrix(S_boot_train)
#boot_errors <- sweep(S_boot_train_matrix, 3, S_boot_test_mean)
boot_errors <- data.frame(SE = numeric(J),
                          DS = numeric(J),
                          Brier = numeric(J))
for (j in 1:J) {
  boot_errors[j, ] <- S_boot_train[j,] - S_boot_test_mean
}
boot_bias <- mean_score(boot_errors)
boot_sd <- apply(boot_errors, 2, sd)
print(boot_bias)
print(boot-sd)

df <- data.frame()
df[1] <- S_boot_train_mean[c(1,2,3)]
df[2] <- S_boot_train_sd[c(1,2,3)]

df <- data.frame(SE = numeric(2),
                          DS = numeric(2),
                          Brier = numeric(2))

df[1, ] <- S_boot_train_mean
df[2, ] <- S_boot_train_sd


