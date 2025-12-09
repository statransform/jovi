rm(list=ls())
library(lmerTest)
library(ARTool)

library(microbenchmark)

library(tidyr)

source("utils-data-generation.R")
source("utils-analysis.R")

################################
# Distribution parameters used in simulation
params_norm   <- list(sigma_s = 0.3, sigma_e = 1)
params_lnorm  <- list(sigma_s = 0.3, sigma_e = 1, mean_target = 1)
params_cauchy <- list(sigma_s = 0.3, gamma = 1)
params_exp    <- list(sigma_s = 0.3, mean_target = 0.5)
params_poisson <- list(sigma_s = 0.3, mean_target = 3)
params_binom  <- list(sigma_s = 0.3, size = 10, p_target = 0.1)

use_parameters <- function(family){
  params <- switch(family, 
    norm    = params_norm, 
    lnorm   = params_lnorm, 
    cauchy  = params_cauchy, 
    exp     = params_exp, 
    poisson = params_poisson, 
    binom   = params_binom
  )

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- runif(1, min = 0.1, max = 0.5)

  params
}

# B. 4 x 3 repeated measures
design = c(4,3)
within = c(1,1) # Both factors are within-subjects
formula1 = Y ~ X1*X2 + Error(factor(subject)) 
formula2 = Y ~ X1*X2 + (1|subject) 
vars = c("X1", "X2", "X1:X2") 
n = 256

data = simulate_response(
  nlevels = c(4,3),
  within = c(1,1), 
  n = n,
  coeffs = c("X1"=0, "X2"=0, "X1:X2"=0),
  family = "lnorm",
  use_parameters("lnorm")
)

bench <- microbenchmark(
  aov = compare_p_values(data, formula1, vars),
  lmer = compare_p_values(data, formula2, vars),
  times = 1
)



