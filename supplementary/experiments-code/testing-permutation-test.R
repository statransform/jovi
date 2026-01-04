# This is just to test INT coupled with permutation tests 

rm(list=ls())

source("utils-data-generation.R")
source("utils-permutation.R")

INT = function(x){
  qnorm((rank(x) - 0.5)/length(x))
}


# Returns a vector with all p-values for all methods
compare_p_values <- function(df, 
  formula = Y ~ X1*X2 + Error(factor(subject))
){  
  df$Z <- INT(df$Y)
  formula2 <- update(formula, Z ~ .)

  model_int <- suppressMessages(do.call(aov, list(formula2, df))) # INT
  
  c(
    INT = extract_p(model_int),
    PER = permutation_test(df, formula2, B = 3000) 
  )
}

# Distribution parameters used in simulation
params_norm   <- list(sigma_s = 0.4, sigma_e = 1)
params_likert <- list(sigma_s = 0.4, sigma_e = 1, levels = 5, flexible = TRUE)
params_lnorm  <- list(sigma_s = 0.4, sigma_e = 1, mean_target = 1)
params_cauchy <- list(sigma_s = 0.4, gamma = 1)
params_exp    <- list(sigma_s = 0.3, mean_target = 0.5)
params_poisson <- list(sigma_s = 0.3, mean_target = 3)
params_binom  <- list(sigma_s = 0.3, size = 10, p_target = 0.1)

nlevels = c(4, 3)
within = c(1, 1)
n = 20
coeffs <- c("X1"=4, "X2"=4, "X1:X2"=0) # a1, a2, a12

df_normal <- simulate_response(
  nlevels, within, n, coeffs,
  family = "norm",
  params = params_norm
)

df_ordinal <- simulate_response(
  nlevels, within, n, coeffs,
  family = "likert",
  params = params_likert
)

df_lognormal <- simulate_response(
  nlevels, within, n, coeffs,
  family = "lnorm",
  params = params_lnorm
)

df_cauchy <- simulate_response(
  nlevels, within, n, coeffs,
  family = "cauchy",
  params = params_cauchy
)

df_exp <- simulate_response(
  nlevels, within, n, coeffs,
  family = "exp",
  params = params_exp
)

df_poisson <- simulate_response(
  nlevels, within, n, coeffs,
  family = "poisson",
  params = params_poisson
)

df_binom <- simulate_response(
  nlevels, within, n, coeffs,
  family = "binom",
  params = params_binom
)

printResults <- function(family, res){
  cat("Results for ", family, " #################\n")
  cat("INT: ", res[grep("INT", names(res))], "\n")
  cat("INT permut: ", res[grep("PER", names(res))], "\n\n")
}

formula = Y ~ X1*X2 + Error(factor(subject))

printResults("Normal: ", compare_p_values(df_normal, formula))
printResults("Ordinal: ", compare_p_values(df_ordinal, formula))
printResults("Lognormal: ", compare_p_values(df_lognormal, formula))
printResults("Exp: ", compare_p_values(df_exp, formula))
printResults("Poisson: ", compare_p_values(df_poisson, formula))
printResults("Binom: ", compare_p_values(df_binom, formula))



