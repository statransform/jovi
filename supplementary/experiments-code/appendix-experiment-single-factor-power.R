# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Comparing single-factor non-parametric methods 
# Testing single-factor designs with 2,3 or 4 levels (both within-subjects and between-subjects) 

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("utils-data-generation.R")
source("utils-analysis.R")
source("utils-extra.R")

################################
# Distribution parameters used in simulation
params_norm   <- list(sigma_e = 1)
params_lnorm  <- list(sigma_e = 1, mean_target = 1)
params_exp    <- list(mean_target = 0.5)
params_poisson <- list(mean_target = 3)
params_binom  <- list(size = 10, p_target = 0.1)
params_likert <- list(sigma_e = 1, levels = 5, flexible=TRUE)

use_parameters <- function(family){
  params <- switch(family, 
    norm    = params_norm, 
    lnorm   = params_lnorm, 
    cauchy  = params_cauchy, 
    exp     = params_exp, 
    poisson = params_poisson, 
    binom   = params_binom,
    likert  = params_likert
  )

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- c(0.1, 0.5) # Specifies the min and max of a uniform range

  params
}

# Testing single-factor designs with 2,3 or 4 levels (both within-subjects and between-subjects) 
designs <- list(2, 3, 4, 2, 3, 4)
within <- list(1, 1, 1, 0, 0, 0)

formula = Y ~ X1 + Error(factor(subject)) 
vars = c("X1") 

distributions <- c("norm", "lnorm", "exp", "binom", "poisson", "likert")

# Various effects
effects <- matrix(c(0, 0.5, 1.0, 1.5), ncol = 1, byrow = TRUE)
colnames(effects) <- vars

# Cell sizes (n in the paper)
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Power_single_factor"

# Set the seed for reproducibility
set.seed(4031)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(desId = 1:length(designs), .combine=rbind) %do% { 
      foreach(family = distributions, .combine=rbind) %do% {
        foreach(n = Ns, .combine=rbind) %do% {
          foreach(effId = 1:length(effects), .combine=rbind) %do% {
            # The test function is parallelized (using multiple cores)
            repeat_test_custom(
              nlevels=designs[[desId]], 
              within=within[[desId]], 
              n=n, 
              coeffs=effects[effId,],
              family=family,
              params=use_parameters(family),
              formula=formula,
              vars=vars,
              iterations = R,
              methods = c("PAR", "RNK", "INT", "NON"),
              compare_function = compare_p_values_single
            )
          }
        }
      }
    }
})


# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(results, file = csvfile, row.names=FALSE, quote=F)


