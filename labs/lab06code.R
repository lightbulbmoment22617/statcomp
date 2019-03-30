# score_se: Compute the Squared Error score
# Input:
#   pred : data.frame with (at least) a column "mu"
#   y : data vector
# Output:
#   a vector of scores

score_se <- function(pred, y) {
  (y - pred$mu)^2
}

# score_ds: Compute the Dawid-Sebastiani score
# Input:
#   pred : data.frame with (at least) columns "mu" and "sigma"
#   y : data vector
# Output:
#   a vector of scores

score_ds <- function(pred, y) {
  ((y - pred$mu) / pred$sigma)^2 + 2 * log(pred$sigma)
}

# score_brier: Compute the Brier score
#   Note: this implementation uses the prediction probabilities
#         and event indicator values directly, unlike the CWA version!
# Input:
#   prob : vector with event prediction probabilities
#   event : data vector of event indicators
# Output:
#   a vector of scores

score_brier <- function(prob, event) {
  (event - prob)^2
}


# mean_score: Compute average scores
# Input:
#   data: A data frame containing columns with computed scores
#         Scores in columns named SE, SD, and Brier are handled.
#   by.ID: If TRUE, the mean scores are computed within unique IDs
# Output:
#   a data.frame with averaged scores
# Usage example:
#   testscores <- data.frame(ID = rep(c("A", "B"), c(3, 2)),
#                            SE = 1:5,
#                            DS = rnorm(5))
#   mean_score(testscores, by.ID = FALSE)
#   mean_score(testscores, by.ID = TRUE)

suppressPackageStartupMessages(library(tidyverse)) # Required library
mean_score <- function(data, by.ID = FALSE) {
  if (by.ID) {
    out <- data %>%
      group_by(ID) %>%
      summarise(SE = ifelse(is.null(data$SE), NA, mean(SE)),
                DS = ifelse(is.null(data$DS), NA, mean(DS)),
                Brier = ifelse(is.null(data$Brier), NA, mean(Brier))) %>%
      ungroup() %>%
      select(ID, SE, DS, Brier)
  } else {
    out <- data %>%
      summarise(SE = ifelse(is.null(data$SE), NA, mean(SE)),
                DS = ifelse(is.null(data$DS), NA, mean(DS)),
                Brier = ifelse(is.null(data$Brier), NA, mean(Brier))) %>%
      select(SE, DS, Brier)
  }
  as.data.frame(out)
}



# Implementation of an answer from Coursework A:
prediction <- function(newdata, fit) {
  pred <- predict(fit, newdata = newdata, se.fit = TRUE)
  # Compute an estimate of the prediction standard deviation:
  pred_sigma <- sqrt(pred$se.fit^2 + sigma(fit)^2)
  data.frame(mu = pred$fit,
             sigma = pred_sigma,
             prob = pnorm(0, mean = pred$fit, sd = pred_sigma))
}

# Extra helper function to avoid duplicating code in Coursework A.
# Input:
#   newdata: A data.frame with data to be used for model assessment
#   pred: output from the prediction() function
#   response_name: A ""-quoted string with the name of the response
#                  variable in the data objects
# Output:
#   A data.frame with computed scores
collect_scores <- function(newdata, pred, response_name) {
  # Deviation for the coursework: score_brier is defined differently, see above.
  score <- data.frame(
    SE = score_se(pred, newdata[, response_name]),
    DS = score_ds(pred, newdata[, response_name]),
    Brier = score_brier(pred$prob, (newdata[, response_name] <= "0")))
  if (is.null(newdata$ID)) {
    score
  } else {
    cbind(data.frame(ID = newdata$ID), score)
  }
}


# Estimate a model on training data and return validation scores
# Input:
#   train: A data.frame with data to be used for model estimation
#   valid: A data.frame with data to be used for model assessment
#   formula: The formula defining the model estimable with lm()
# Output:
#   The prediction scores (SE and DS for y predictions,
#   and Brier for y <= 0 events) for the validation data.
train_and_validate <- function(train, valid, formula) {
  fit <- lm(formula, data = train)
  pred <- prediction(newdata = valid, fit = fit)
  # This seems to be the most reliable approach to extract the response name:
  response_name <- all.vars(update(formula, . ~ 1))
  collect_scores(valid, pred, response_name)
}



# See lab06 Q7 for information about this function
make_formula <- function(max_order) {
  form <-
    paste0("Value ~ Longitude + Latitude",
           " + Elevation + I(DecYear-Year)",
           paste0(" + I(cos(2*pi*DecYear*", seq_len(max_order), "))",
                  collapse = ""),
           paste0(" + I(sin(2*pi*DecYear*", seq_len(max_order), "))",
                  collapse = ""))
  as.formula(form)
}





