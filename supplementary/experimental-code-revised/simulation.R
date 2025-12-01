rm(list=ls())
#library(lmerTest)
#library(ARTool)

source("simulation-utils.R")

nlevels = c(4, 3)
nsubjects = 10000
reps = 1
coeffs <- c(2, 0, 0) # a1, a2, a12

df_normal <- simulate_response(
  nlevels, nsubjects, reps,
  coeffs,
  sigma_s = 0.4,
  family = "normal",
  sigma_e = 1
)

df_lognormal <- simulate_response(
  nlevels, nsubjects, reps,
  coeffs,
  sigma_s = 0.4,
  family = "lognormal",
  mean_target = 1,
  sigma_e = 1
)

df_cauchy <- simulate_response(
  nlevels, nsubjects, reps,
  coeffs,
  sigma_s = 0.4,
  family = "cauchy",
  gamma = 1
)

df_exp <- simulate_response(
  nlevels, nsubjects, reps,
  coeffs,
  sigma_s = 0.3,
  family = "exponential",
  mean_target = 0.5
)

df_poisson <- simulate_response(
  nlevels, nsubjects, reps,
  coeffs,
  sigma_s = 0.3,
  family = "poisson",
  mean_target = 3
)

df_binom <- simulate_response(
  nlevels, nsubjects, reps,
  coeffs,
  sigma_s = 0.3,
  family = "binomial",
  p_target = 0.1,
  size = 10
)




