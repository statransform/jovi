rm(list=ls())
#library(lmerTest)
#library(ARTool)

library(microbenchmark)

source("simulation-utils.R")

nlevels = c(4, 3)
nsubjects = 10000
reps = 1
a1 = 2
a2 = 0
a12 = 0


bench <- microbenchmark(
  poisson = simulate_response(
    nlevels, nsubjects, reps,
    a1, a2, a12,
    sigma_s = 0.3,
    family = "poisson",
    mean_target = 3
  ),

  binom = simulate_response(
    nlevels, nsubjects, reps,
    a1, a2, a12,
    sigma_s = 0.3,
    family = "binomial",
    p_target = 0.1,
    size = 10
  ),
#  accurate = mu_binomial_accurate(compute_effects(make_design(m1=2,m2=2),a1=4,a2=0,a12=0), sigma_s = 0.3, p_target = 0.1),
#  gh = mu_binomial(compute_effects(make_design(m1=2,m2=2),a1=4,a2=0,a12=0), sigma_s = 0.3, p_target = 0.1, size=10, n_nodes = 32),
  times = 100
)

