# Import code given
source('cw2/CWB2019code.R')

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
  N <- param[1]
  theta <- param[2]
  thetatwo <- 2*N*exp(theta)/(1+exp(theta))^2
  theta_n <- 2*exp(theta)/(1+exp(theta))
  ntwo <- (psigamma(N-Y[1]+1, 1)*gamma(N-Y[1]+1)-(digamma(N-Y[1]+1))^2) / (gamma(N-Y[1]+1))^2
          +
          (psigamma(N-Y[2]+1, 1)*gamma(N-Y[2]+1)-(digamma(N-Y[2]+1))^2) / (gamma(N-Y[2]+1))^2
          -
          2*(psigamma(N+1, 1)*gamma(N+1)-(digamma(N+1))^2) / (gamma(N+1))^2
  return(matrix(c(ntwo, theta_n, theta_n, thetatwo),nrow=2, ncol=2))
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