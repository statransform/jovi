library(dplyr)

#############################################
### PARAMETERS
#############################################
mu     <- 0
sigma  <- 1            # lognormal sd
alphas <- seq(0, 4, by=0.5)
#ns     <- c(10, 30, 50, 200, 1000)
ns <- c(10, 50, 100)
R      <- 80         # Monte Carlo replicates
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
#  grand.raw  <- mean(dat$Y_raw)
#  alpha.raw  <- tapply(dat$Y_raw, dat$i, mean) - grand.raw
#  dat$YB_raw <- dat$Y_raw - alpha.raw[dat$i] + grand.raw

#  e_raw <- abs(mean(dat$YB_raw[dat$j==2]) - mean(dat$YB_raw[dat$j==1]))

  ## ------------- Alternative alignment
  dat$S <- factor(dat$k)
  dat$A <- factor(dat$i)
  dat$B <- factor(dat$j)
  dat <- dat %>% 
    group_by(A) %>% mutate(Ai = mean(Y_raw)) %>% # mean for A = Ai
    group_by(B) %>% mutate(Bj = mean(Y_raw)) %>% # mean for B = Bj
    # mean for A=Ai, B=Bj and cell mean
    group_by(A,B) %>% mutate(AiBj = mean(Y_raw)) %>% mutate(Cell = mean(Y_raw)) 

  m <- mean(dat$Y_raw) # the grand mean

  # Aligned observations
#  dat$alignedA <- rank(dat$Y_raw - dat$Cell + dat$Ai - m)
  dat$alignedB <- dat$Y_raw - dat$Cell + dat$Bj - m
  dat$alignedB_rank <- rank(dat$alignedB)
#  dat$alignedAB <- rank(dat$Y_raw - dat$Cell + dat$AiBj - dat$Ai - dat$Bj + m) 

  e_art_raw <- abs(mean(dat$alignedB[dat$j==2]) - mean(dat$alignedB[dat$j==1]))
  e_art <- abs(mean(dat$alignedB_rank[dat$j==2]) - mean(dat$alignedB_rank[dat$j==1]))

  # ----- LOGGED, ranked, row -----
  dat$log<- log(dat$Y_raw)
  e_log <- abs(mean(dat$log[dat$j==2]) -
               mean(dat$log[dat$j==1]))

  dat$rnk <- rank(dat$Y_raw)
  e_rnk <- abs(mean(dat$rnk[dat$j==2]) -
               mean(dat$rnk[dat$j==1]))

  e_raw <- abs(mean(dat$Y_raw[dat$j==2]) -
               mean(dat$Y_raw[dat$j==1]))

  c(error_art_raw = e_art_raw, error_art = e_art, error_baseline_log = e_log, error_baseline_rnk = e_rnk, error_raw = e_raw)
}

# analytic approximation
analytic_B <- function(alpha, n, sigma = 1, mu = 0) {
  S <- exp(mu)
  m <- exp(sigma^2/2)
  SD2 <- exp(sigma^2) - 1
  
  # approx: E|ΔB| ≈ S m * sqrt( SD2*(1 + e^(2α)) / (π n) )
  S*m * sqrt( SD2*(1 + exp(2*alpha)) / (pi*n) )
}


# Expected ART B-bias after ranking
analytic_B_ranks <- function(alpha, n,
                                sigma = 1,
                                mu = 0) {
  # constants
  S <- exp(mu)
  m <- exp(sigma^2/2)
  sigma_eps <- S*m*sqrt(exp(sigma^2) - 1)
  
  # C(alpha)
  C <- (S*m)^2 * (exp(alpha) - 1) * (exp(sigma^2) - 1) / 2
  
  # mean shift in rank difference
  mu_D <- (2 / (sqrt(pi) * sigma_eps)) * C
  
  # variance of rank difference under null
  tau_n <- sqrt((2/3) * n)
  
  # expected absolute shift including noise
  E_abs <- tau_n * sqrt(2/pi) * exp(-mu_D^2 / (2*tau_n^2)) +
           mu_D * (1 - 2 * pnorm(- mu_D / tau_n))
  
  # subtract null baseline
  b_n <- (2 / sqrt(3*pi)) * sqrt(n)
  
  distortion <- E_abs - b_n

  #return(distortion)
  return(E_abs)
}


#############################################
### MONTE CARLO LOOP
#############################################
results <- expand.grid(alpha=alphas, n=ns)
#results$err_raw <- NA
#results$err_log <- NA
#results$pred <- NA

for(row in 1:nrow(results)){
  alpha <- results$alpha[row]
  n     <- results$n[row]

  # Monte Carlo
  tmp <- replicate(R, compute_art_error(alpha, n))
  results$err_art_raw[row] <- mean(tmp["error_art_raw", ])
  results$err_art[row] <- mean(tmp["error_art", ])
  results$err_baseline_log[row] <- mean(tmp["error_baseline_log", ])
  results$err_baseline_rnk[row] <- mean(tmp["error_baseline_rnk", ])
  results$err_raw[row] <- mean(tmp["error_raw", ])

  # analytic raw prediction
  results$pred[row] <- analytic_B(alpha, n)

  # analytic prediction for ranks
  results$pred_ranks[row] <- analytic_B_ranks(alpha, n)
}

#############################################
### PLOT
#############################################
library(ggplot2)

p_raw <- ggplot(results, aes(x=alpha, color=factor(n))) +
  geom_point(aes(y=err_art_raw)) +
  geom_line(aes(y=err_art_raw), linewidth=0.8) +
  geom_line(aes(y=err_baseline_log), linewidth=1.1) +
  geom_line(aes(y=pred), linewidth=1.1) +

  labs(
    title="Spurious ART B-effect in a 2×2 lognormal model",
    x="True A-effect α",
    y="Spurious B-effect magnitude",
    color="n per cell"
  ) +
  theme_minimal(base_size=14)

p_rank <- ggplot(results, aes(x=alpha, color=factor(n))) +
  geom_point(aes(y=err_art)) +
  geom_line(aes(y=err_art), linewidth=0.8) +
  geom_line(aes(y=err_baseline_rnk), linewidth=1.1) +
  geom_line(aes(y=pred_ranks), linewidth=1.1) +

  labs(
    title="Spurious ART B-effect in a 2×2 lognormal model",
    x="True A-effect α",
    y="Spurious B-effect magnitude",
    color="n per cell"
  ) +
  theme_minimal(base_size=14)


p_raw2 <- ggplot(results, aes(x=alpha, color=factor(n))) +
  geom_point(aes(y=err_art_raw)) +
  geom_line(aes(y=err_art_raw), linewidth=0.8) +
  geom_line(aes(y=err_raw), linewidth=1.1) +
#  geom_line(aes(y=pred), linewidth=1.1) +

  labs(
    title="Spurious ART B-effect in a 2×2 lognormal model",
    x="True A-effect α",
    y="Spurious B-effect magnitude",
    color="n per cell"
  ) +
  theme_minimal(base_size=14)
