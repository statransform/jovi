rm(list=ls())

source("utils-data-generation.R")
source("utils-analysis.R")


# Distribution parameters used in simulation
params_normal <- list(sigma_s = 0.4, sigma_e = 1)
params_ordinal <- list(sigma_s = 0.4, sigma_e = 1, levels = 5, equidistant = FALSE)
params_lognormal <- list(sigma_s = 0.4, sigma_e = 1, mean_target = 1)
params_cauchy <- list(sigma_s = 0.4, gamma = 1)
params_exponential <- list(sigma_s = 0.3, mean_target = 0.5)
params_poisson <- list(sigma_s = 0.3, mean_target = 3)
params_binomial <- list(sigma_s = 0.3, size = 10, p_target = 0.1)

nlevels = c(2, 2, 2)
within = c(1, 1, 1)
n = 20
coeffs <- c(X1=2, X2=0, X3 = 0, X1X2=0) # a1, a2, a12

df_normal <- simulate_response(
  nlevels, within, n, coeffs,
  family = "normal",
  params = params_normal
)

df_ordinal <- simulate_response(
  nlevels, within, n, coeffs,
  family = "ordinal",
  params = params_ordinal
)

df_lognormal <- simulate_response(
  nlevels, within, n, coeffs,
  family = "lognormal",
  params = params_lognormal
)

df_cauchy <- simulate_response(
  nlevels, within, n, coeffs,
  family = "cauchy",
  params = params_cauchy
)

df_exp <- simulate_response(
  nlevels, within, n, coeffs,
  family = "exponential",
  params = params_exponential
)

df_poisson <- simulate_response(
  nlevels, within, n, coeffs,
  family = "poisson",
  params = params_poisson
)

df_binom <- simulate_response(
  nlevels, within, n, coeffs,
  family = "binomial",
  params = params_binomial
)

printResults <- function(family, res){
  cat("Results for ", family, " #################\n")
  cat("PAR: ", res[grep("PAR", names(res))], "\n")
  cat("ART: ", res[grep("ART", names(res))], "\n")
  cat("RNK: ", res[grep("RNK", names(res))], "\n")
  cat("INT: ", res[grep("INT", names(res))], "\n\n")
}

vars = c("X1", "X2", "X3", "X1:X2")
formula = Y ~ X1*X2*X3 + (1|subject)
cat("Use of lmer function", "\n")
printResults("Normal: ", compare_p_values(df_normal, formula, vars))
printResults("Ordinal: ", compare_p_values(df_ordinal, formula, vars))
printResults("Lognormal: ", compare_p_values(df_lognormal, formula, vars))
printResults("Exp: ", compare_p_values(df_exp, formula, vars))
printResults("Poisson: ", compare_p_values(df_poisson, formula, vars))
printResults("Binom: ", compare_p_values(df_poisson, formula, vars))

formula = Y ~ X1*X2*X3 + Error(factor(subject))
cat("\nUse of aov function", "\n")
printResults("Normal: ", compare_p_values(df_normal, formula, vars))
printResults("Ordinal: ", compare_p_values(df_ordinal, formula, vars))
printResults("Lognormal: ", compare_p_values(df_lognormal, formula, vars))
printResults("Exp: ", compare_p_values(df_exp, formula, vars))
printResults("Poisson: ", compare_p_values(df_poisson, formula, vars))
printResults("Binom: ", compare_p_values(df_poisson, formula, vars))

##library(microbenchmark)
##bench <- microbenchmark(
## lmer = analysis_lmer(df_normal),
##  aov = analysis_aov(df_normal),
##  art = analysis_art(df_normal),
##  art_aov = analysis_art_aov(df_normal),
##  times = 1
##)
##print(bench)

