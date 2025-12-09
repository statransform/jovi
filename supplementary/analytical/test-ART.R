library(tidyverse)

## ----------------------------------------------------------
## Functions
## ----------------------------------------------------------

# simulate one 2×2 dataset and estimate spurious B effect
simulate_once <- function(alpha, n, sigma = 1, mu = 0) {
  # A = 1,2 ; B = 1,2
  alpha_vals <- c(0, alpha)                # effect only on A2
  mu_mat <- outer(alpha_vals, c(0,0), "+") # no B-effect
  
  # sample log-normal data
  Y <- matrix(rlnorm(4*n, meanlog = mu_mat, sdlog = sigma),
              nrow = 4)
  
  # cell means
  mA1B1 <- mean(Y[1,])
  mA1B2 <- mean(Y[2,])
  mA2B1 <- mean(Y[3,])
  mA2B2 <- mean(Y[4,])
  
  # estimated marginal B effect:
  #   B2 - B1
  est_B <- ((mA1B2 + mA2B2)/2) - ((mA1B1 + mA2B1)/2)
  return(est_B)
}

# analytic approximation
analytic_B <- function(alpha, n, sigma = 1, mu = 0) {
  S <- exp(mu)
  m <- exp(sigma^2/2)
  SD2 <- exp(sigma^2) - 1
  
  # approx: E|ΔB| ≈ S m * sqrt( SD2*(1 + e^(2α)) / (π n) )
  S*m * sqrt( SD2*(1 + exp(2*alpha)) / (pi*n) )
}

## ----------------------------------------------------------
## Run simulation across alpha and n
## ----------------------------------------------------------

alphas <- seq(0, 2, by = 0.25)
ns <- c(10, 20, 100)
R <- 2000

results <- expand.grid(alpha = alphas, n = ns, rep = 1:R) %>%
  group_by(alpha, n) %>%
  do({
    ests <- replicate(R, simulate_once(.$alpha[1], .$n[1]))
    tibble(
      est_B_abs = abs(ests),
      analytic  = analytic_B(.$alpha[1], .$n[1])
    )
  }) %>%
  ungroup() %>%
  group_by(alpha, n) %>%
  summarise(
    mean_abs_B = mean(est_B_abs),
    analytic   = first(analytic)
  )

## ----------------------------------------------------------
## Plot
## ----------------------------------------------------------

pp <- ggplot(results, aes(alpha, mean_abs_B, color = factor(n))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_line(aes(y = analytic), size = 1.5) +
  labs(
    x = "True A-effect α",
    y = "Spurious B-effect magnitude",
    color = "n per cell"
  ) +
  theme_minimal(base_size = 14)
