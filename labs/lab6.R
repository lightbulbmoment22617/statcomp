set.seed(10)
source("labs/lab06code.R")

TMINallobs <- read.csv(
  file = "labs/TMINallobs.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)
TMINalltest <- read.csv(
  file = "labs/TMINalltest.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

cvk_define_splits <- function(N, K) {
  if (K > N) {
    stop("K-fold cross validation cannot have K > N")
  }
  sub_size <- ceiling((1:K) * N / K) - ceiling((0:(K - 1)) * N / K)
  sample(rep(1:K, times = sub_size), size = N, replace = FALSE)
}

cvk_split <- function(data, splits, k) {
  list(
    train = data[splits != k, , drop = FALSE],
    valid = data[splits == k, , drop = FALSE]
  )
}

mysplits <- cvk_define_splits(nrow(TMINallobs), 10)
data1 <- cvk_split(TMINallobs, mysplits, 1)

scores <-
  train_and_validate(
    TMINallobs,
    TMINalltest,
    Value ~ Longitude + Latitude +
      Elevation + I(DecYear - Year)
  )
mean_score(scores, by.ID = FALSE)

cvk_do_all <- function(data, splits, formula) {
  K <- max(splits)
  S_hat <- data.frame(
    SE = numeric(K),
    DS = numeric(K),
    Brier = numeric(K)
  )
  for (k in 1:K) {
    data_k <- cvk_split(data, splits, k)
    scores_k <- train_and_validate(
      train = data_k$train,
      valid = data_k$valid,
      formula = formula
    )
    S_hat[k, ] <- mean_score(scores_k, by.ID = FALSE)[c("SE", "DS", "Brier")]
  }
  S_hat
}

colMeans(cvk_do_all(TMINallobs,
                    rep(1:3, nrow(TMINallobs)/3),
                    Value ~ Elevation))
