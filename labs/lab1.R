# Generate synthetic data
z <- rep(1:10, times = 10)
data <- data.frame(z = z, y = 2 * z + rnorm(length(z), sd = 4))

# Plot the data
plot(data$z, data$y)

# Estimate a linear model
mod <- lm(y ~ z, data = data)

# Now to plot the original data and prediction
newdata <- data.frame(z = -10:20)

plot(newdata$z, predict(mod, newdata), "l")
points(data$z, data$y, pch = 20)
abline(0, 2, col = 2)

# Now we want to plot with prediction intervals
pred <- predict(mod, newdata, interval = "prediction")
plot(newdata$z, pred[, "fit"], "l", ylim = range(pred))
lines(newdata$z, pred[, "lwr"], lty = 2)
lines(newdata$z, pred[, "upr"], lty = 2)
points(data$z, data$y, pch = 20)
abline(0, 2, col = 2)

# Function to redo this analysis with new models
plot_predictions <- function(x, newdata, xname = "x", ylim = NULL, xlab = NULL, ...) {
  pred <- predict(x, newdata, interval = "prediction")
  if (is.null(ylim)) {
    ylim <- range(pred)
  }
  if (is.null(xlab)) {
    xlab <- xname
  }
  plot(newdata[[xname]], pred[, "fit"], type = "l", ylim = ylim, xlab = xlab, ...)
  lines(newdata[[xname]], pred[, "lwr"], lty = 2)
  lines(newdata[[xname]], pred[, "upr"], lty = 2)

  pred <- predict(x, newdata, interval = "confidence")
  lines(newdata[[xname]], pred[, "lwr"], lty = 3)
  lines(newdata[[xname]], pred[, "upr"], lty = 3)
}

# This should regenerate the previous figure
plot_predictions(mod, newdata, xname = "z", xlab = "z", ylab = "y")
points(data$z, data$y, pch = 20)
abline(0, 2, col = 2)

# Testing the new function on a quadratic model
newdata <- data.frame(z = -10:20)
mod <- lm(y ~ 1 + z + I(z^2), data)
plot_predictions(mod, newdata, xname = "z", xlab = "z", ylab = "y")
points(data$z, data$y, pch = 20)
abline(0, 2, col = 2)

# Plot a list of models
formulas <- c(
  y ~ 1,
  y ~ z,
  y ~ z + I(z^2),
  y ~ z + I(z^2) + I(z^3)
)

mods <- lapply(formulas[], function(mod) lm(mod, data))

par(mfrow = c(2, 2))
for (k in seq_along(formulas)) {
  plot_predictions(mods[[k]], newdata,
    xname = "z", ylab = "y",
    main = as.character(formulas[k])
  )
  points(data$z, data$y, pch = 20)
  abline(0, 2, col = 2)
}
par(mfrow = c(1, 1))
