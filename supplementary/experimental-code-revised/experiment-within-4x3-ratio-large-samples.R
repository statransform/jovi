# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on ratio scales (distributions below) for a 4x3 within-subjects design
# Evaluation for growing sample sizes (up to n=512) and a fixed effect on the first factor


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
params_cauchy <- list(gamma = 1)
params_exp    <- list(mean_target = 0.5)
params_poisson <- list(mean_target = 3)
params_binom  <- list(size = 10, p_target = 0.1)

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
  params$sigma_s <- c(0.1, 0.5) # Specifies the min and max of a uniform range

  params
}

# Design configuration
design = c(4,3)
within = c(1,1) # Both factors are within-subjects
formula = Y ~ X1*X2 + Error(factor(subject))
formula_lmer = Y ~ X1*X2 + (1|subject) # For large samples, aov becomes particularly slow, we use LMER
vars = c("X1", "X2", "X1:X2") 

# Continuous distributions (equal variance, full) and discrete distribution: binom (size = 10, prob = 0.1) and Poisson
distributions <- c("norm", "lnorm", "exp", "cauchy", "binom", "poisson")

# Applying only effect on X1
effects <- matrix(c(
            1, 0, 0), 
            ncol = 3, byrow = TRUE)

colnames(effects) <- vars

# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
Ns <- c(32, 64, 128, 256, 512)
# For larger samples, we use lmer as aov becomes quite slow. However, results are nearly identical
formulas <- c(formula, formula, formula, formula_lmer, formula_lmer) 

# 5000 iterations
R <- 5000

filename = "Type_I_4x3_ratio_large_samples"

# Set the seed for reproducibility
set.seed(2234)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(family = distributions, .combine=rbind) %do% {
      foreach(ni = 1:length(Ns), .combine=rbind) %do% {
        foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
          # The test function is parallelized (using multiple cores)
          repeat_test(
            nlevels=design, 
            within=within, 
            n=Ns[ni], 
            coeffs=effects[effId,],
            family=family,
            params=use_parameters(family),
            formula=formulas[[ni]],
            vars=vars,
            iterations = R
          )
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

