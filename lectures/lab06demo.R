set.seed(12345L)

source("labs/lab06code.R")
TMINallobs <- read.csv(
  file = "data/TMINallobs.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
TMINalltest <- read.csv(
  file = "data/TMINalltest.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)


cvk_define_split <- function(N, K) {
  Nk <- ceiling(1:K * N / K) - ceiling(0:(K - 1) * N / K)
  sample(rep(1:K, times = Nk), N, replace = FALSE)
}

cvk_define_split(10, 4)

cvk_split <- function(data, splits, k) {
  list(
    train = data[splits != k, , drop = FALSE],
    valid = data[splits == k, , drop = FALSE]
  )
}

mysplits <- cvk_define_split(nrow(TMINallobs), 10)
data1 <- cvk_split(TMINallobs, mysplits, 1)

cvk_do_all <- function(data, splits, formula) {
  K <- max(splits) # or length(unique(splits))
  S_hat <- data.frame(
    SE = numeric(K),
    DS = numeric(K),
    Brier = numeric(K)
  )
  for (k in 1:K) {
    data_k <- cvk_split(data, splits, k)
    scores_k <-
      train_and_validate(
        data_k$train,
        data_k$valid,
        formula
      )
    S_hat[k, ] <-
      mean_score(scores_k, by.ID = FALSE)[
        c("SE", "DS", "Brier")]
  }
  S_hat
}

(cvk_do_all(TMINallobs,
           rep(1:2, nrow(TMINallobs)/2),
           Value ~ Elevation))
