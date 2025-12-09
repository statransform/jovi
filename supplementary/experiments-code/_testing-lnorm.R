rm(list=ls())

source("utils-data-generation.R")
source("utils-analysis.R")

# Distribution parameters used in simulation
#params_norm   <- list(sigma_s = 0.4, sigma_e = 1)
params_lnorm_1  <- list(sigma_s = 0.5, sigma_e = 0.2, mean_target = 1)
params_lnorm_2  <- list(sigma_s = 0.5, sigma_e = 1, mean_target = 1)

nlevels = c(2)
within = c(1)
n = 100000
coeffs <- c("X1"=2, "X2"=0, "X1:X2"=0) # a1, a2, a12

df1 <- simulate_response(
  nlevels, within, n, coeffs,
  family = "lnorm",
  params = params_lnorm_1,
  normalize = TRUE
)

df2 <- simulate_response(
  nlevels, within, n, coeffs,
  family = "lnorm",
  params = params_lnorm_2,
  normalize = TRUE
)

printParams <- function(df, sigma){
  cat("####### sigma = ", sigma, " ########\n")
  y1 = log(df[df$X1=="A1", ]$Y)
  y2 = log(df[df$X1=="A2", ]$Y)
  diff = abs(mean(y1) - mean(y2))

  cat("means = ", mean(y1), ", ", mean(y2), " diff = ", diff, "\n")
  cat("sds = ", mean(sd(y1), sd(y2)), "\n")
  cat("effect size = ", diff/mean(sd(y1), sd(y2)), "\n")
}

printParams(df1, params_lnorm_1$sigma_e)
printParams(df2, params_lnorm_2$sigma_e)
