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
# Obtain estimate of phi
phi_hat <- ilogit(opt$par[2])
# TODO: Plot negloglik as we optimise

# Q1.3
hess <- optimHess(opt$par, fn = negloglike, Y = c(256, 237))
covar <- solve(hess)

SE_N <- sqrt(covar[1,1])
error <- qnorm(0.975)*SE_N
lwr <- opt$par[1]-error
upr <- opt$par[1]+error

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
N <- opt$par[1]
theta <- opt$par[2]
Y <- c(256, 237)
L0 <- negloglike(opt$par, Y)
L1 <- digamma(N-Y[1]+1) / gamma(N-Y[1]+1) 
       + digamma(N-Y[2]+1) / gamma(N-Y[2]+1)
       - 2*digamma(N+1) / gamma(N+1)
       + 2*log(1+exp(theta))
L4 <- abs(sum(psigamma(N-Y+1,3)) - 2*psigamma(N+1,3))

e <- .Machine$double.eps
h <- 0.0001
bound <- e*(4*L0+2*abs(theta)*L1) / h^2
         + (L4*h^2) / 12

# Q2.4
bm <- microbenchmark::microbenchmark(myhessian(opt$par, Y = c(256, 237)),
                                     optimHess(opt$par, fn = negloglike, Y = c(256, 237)))

