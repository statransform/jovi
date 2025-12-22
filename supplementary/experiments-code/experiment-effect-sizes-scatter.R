# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the precision of effect size estimates

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("utils-data-generation.R")
source("utils-effect-size.R")

################################
# Distribution parameters used in simulation
params_norm   <- list(sigma_e = 1)
params_lnorm  <- list(sigma_e = 1, mean_target = 1)
params_likert  <- list(sigma_e = 1, levels = 5, flexible=TRUE)

use_parameters <- function(family){
  params <- switch(family, 
    norm    = params_norm, 
    lnorm   = params_lnorm, 
    likert  = params_likert
  )

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- c(0.5) # Specifies the min and max of a uniform range

  params
}

# (1) 4 x 3 within-subjects, (2) 2 x 3 between-subjects, (3) 2 x 4 mixed design
designs <- list(c(4,3), c(2,3), c(2,4))
within <- list(c(1,1), c(0,0), c(0,1))

formula = Y ~ X1*X2 + Error(factor(subject)) 
vars = c("X1", "X2", "X1:X2") 

# Continuous distributions (equal variance, full) and discrete distribution: binom (size = 10, prob = 0.1) and Poisson
distributions <- c("norm", "lnorm", "likert")

max_effects = c(8,8,0)
names(max_effects) <- vars

# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
Ns <- c(20) 

# 100 iterations
R <- 100

filename = "cohens_f_8"

# Set the seed for reproducibility
set.seed(4567)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(family = distributions, .combine=rbind) %do% {
      foreach(n = Ns, .combine=rbind) %do% {
        foreach(designId = 1:length(designs), .combine=rbind) %do% {
          # The test function is parallelized (using multiple cores)
          repeat_test_effect_size(
            nlevels=designs[[designId]], 
            within=within[[designId]], 
            n=n, 
            coeffs=max_effects,
            family=family,
            params=use_parameters(family),
            formula=formula,
            vars=vars,
            measure="cohens",
            iterations = R,
            is_max = TRUE
          )
        }
      }
    }
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>%
  rename_with(~ gsub("[(.|:)_]", "", .x)) # Just rename the columns by erasing the "_" and ":""

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)


