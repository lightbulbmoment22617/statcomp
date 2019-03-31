set.seed(10)
source("labs/lab6.R")

boot_resample <- function(data) {
  data[sample(nrow(data), size = nrow(data), replace = TRUE), , drop = FALSE]
}

head(TMINallobs)
data_boot <- boot_resample(TMINallobs)
head(data_boot)

balmoral <- TMINallobs %>% filter(ID == "UKE00105875")
theta_hat <- mean(balmoral$Value < 0)
theta_hat

theta_m_hat <- numeric(12)
for (m in 1:12) {
  theta_m_hat[m] <- mean((balmoral %>% filter(Month == m))$Value < 0)
}
theta_m_hat

#plot(theta_m_hat)

J <- 1000
theta_hat_boot <- numeric(J)
for (j in 1:J) {
  theta_hat_boot[j] <- mean((boot_resample(
    TMINallobs %>% filter(ID == "UKE00105875")))$Value < 0)
}
summary(theta_hat_boot)

theta_CI <- theta_hat - quantile(theta_hat_boot - theta_hat, probs = c(0.975, 0.025))
theta_CI

J <- 250
theta_m_hat_boot <- data.frame()
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
for (m in 1:12) {
  for (j in 1:J) {
    theta_m_hat_boot[j,months[m]] <-
      mean(boot_resample(balmoral %>%
                           filter(Month == m))$Value < 0)
  }
}