# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT for a range of experimental designs:
# 2x3 between-subjects, 2x4 mixed-subjects, 2x2x2 within-subjects, and 3x3x3 within-subjects
# We fix n=20

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("utils-data-generation.R")
source("utils-analysis.R")

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

# (1) 2 x 3 between-subjects, (2) 2 x 4 mixed design (one between-subjects and one repeated-measures factor), (3) 2 x 2 x 2 repeated measures, and (4) 3 x 3 x 3 repeated measures
designs <- list(c(2,3), c(2,4), c(2,2,2), c(3,3,3))
within <- list(c(0,0), c(0,1), c(1,1,1), c(1,1,1))

formula1 = Y ~ X1*X2 + Error(factor(subject)) 
formula2 = Y ~ X1*X2*X3 + Error(factor(subject)) 
formulas <- c(formula1, formula1, formula2, formula2)

vars = c("X1", "X2", "X1:X2", "X3", "X1:X3", "X2:X3", "X1:X2:X3") 

# Continuous distributions (equal variance, full) and discrete distribution: binom (size = 10, prob = 0.1) and Poisson
distributions <- c("norm", "lnorm", "exp", "binom", "poisson", "likert")

# Various combinations of effects
effects <- matrix(c(0, 0, 0, 0,
            0.5, 0.5, 0, 0,
            1, 1, 0, 0,
            2, 2, 0, 0, 
            4, 4, 0, 0,
            8, 8, 0, 0,
            0.5, 0, 0, 0,
            1, 0, 0, 0,
            2, 0, 0, 0,
            4, 0, 0, 0,
            8, 0, 0, 0), 
           ncol = 4, byrow = TRUE)

colnames(effects) <- vars[1:ncol(effects)]

# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Type_I_designs"

# Set the seed for reproducibility
set.seed(3377)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(desId = 1:length(designs), .combine=rbind) %do% { 
      foreach(family = distributions, .combine=rbind) %do% {
        foreach(n = Ns, .combine=rbind) %do% {
          foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
            # The test function is parallelized (using multiple cores)
            repeat_test(
              nlevels=designs[[desId]], 
              within=within[[desId]], 
              n=n, 
              coeffs=effects[effId,],
              family=family,
              params=use_parameters(family),
              formula=formulas[[desId]],
              vars=vars,
              iterations = R
            )
          }
        }
      }
    }
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effect, names_sep = "_") %>% 
  mutate(rate = lapply(rate, function(x) setNames(as.numeric(x), vars))) %>%
  unnest_wider(rate, names_sep = "_")  %>%
  rename_with(~ gsub("[:_]", "", .x)) # Just rename the columns by erasing the "_" and ":""

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)


