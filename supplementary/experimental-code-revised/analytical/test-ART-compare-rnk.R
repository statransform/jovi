library(dplyr)

#############################################
### PARAMETERS
#############################################
mu     <- 0
sigma  <- 1            # lognormal sd
alphas <- seq(0, 4, by=0.5)
#ns     <- c(10, 30, 50, 200, 1000)
ns <- c(10, 20, 50, 100)
R      <- 100         # Monte Carlo replicates
set.seed(123)


#############################################
### FUNCTION: compute ART B-error (raw + logged)
#############################################
compute_art_error <- function(alpha, n){

  # Levels
  a1 <- 0
  a2 <- alpha

  # Create 2x2 structure
  dat <- expand.grid(
    i = c(1,2),
    j = c(1,2),
    k = 1:n
  )
  dat$alpha <- ifelse(dat$i==1, a1, a2)

  # ----- Generate data -----
  dat$Y_raw <- exp(mu + dat$alpha + rnorm(nrow(dat), sd=sigma))
  dat$Y_log <- log(dat$Y_raw)


  # ----- ART ALIGNMENT FOR RAW -----
  grand.raw  <- mean(dat$Y_raw)
  alpha.raw  <- tapply(dat$Y_raw, dat$i, mean) - grand.raw
  dat$YB_raw <- dat$Y_raw - alpha.raw[dat$i] + grand.raw

  e_raw <- abs(mean(dat$YB_raw[dat$j==2]) - mean(dat$YB_raw[dat$j==1]))

  # ----- ART ALIGNMENT FOR LOGGED -----
  grand.log  <- mean(dat$Y_log)
  alpha.log  <- tapply(dat$Y_log, dat$i, mean) - grand.log
  dat$YB_log <- dat$Y_log - alpha.log[dat$i] + grand.log

  e_log <- abs(mean(dat$YB_log[dat$j==2]) -
               mean(dat$YB_log[dat$j==1]))

  c(error_raw = e_raw, error_log = e_log)
}

# analytic approximation
analytic_B <- function(alpha, n, sigma = 1, mu = 0) {
  S <- exp(mu)
  m <- exp(sigma^2/2)
  SD2 <- exp(sigma^2) - 1
  
  # approx: E|ΔB| ≈ S m * sqrt( SD2*(1 + e^(2α)) / (π n) )
  S*m * sqrt( SD2*(1 + exp(2*alpha)) / (pi*n) )
}

#############################################
### MONTE CARLO LOOP
#############################################
results <- expand.grid(alpha=alphas, n=ns)
results$err_raw <- NA
results$err_log <- NA
results$pred <- NA

for(row in 1:nrow(results)){
  alpha <- results$alpha[row]
  n     <- results$n[row]

  # Monte Carlo
  tmp <- replicate(R, compute_art_error(alpha, n))
  results$err_raw[row] <- mean(tmp["error_raw", ])
  results$err_log[row] <- mean(tmp["error_log", ])

  # analytic raw prediction
  results$pred[row] <- analytic_B(alpha, n)
}



#############################################
### PLOT
#############################################
library(ggplot2)

p <- ggplot(results, aes(x=alpha, color=factor(n))) +
  geom_point(aes(y=err_raw)) +
  geom_line(aes(y=err_raw)) +
  geom_line(aes(y=err_log), linewidth=1.1) +
  geom_line(aes(y=pred), linewidth=1.1) +

  labs(
    title="Spurious ART B-effect in a 2×2 lognormal model",
    subtitle=paste(
      "Solid points/lines = ART on raw data (spurious bias)\n",
      "Open circles = ART on log-transformed data (baseline: should be near zero)\n",
      "Black line = analytic bias prediction"
    ),
    x="True A-effect α",
    y="Spurious B-effect magnitude",
    color="n per cell"
  ) +
  theme_minimal(base_size=14)
