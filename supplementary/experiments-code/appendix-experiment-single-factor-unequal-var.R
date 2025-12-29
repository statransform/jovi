# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Comparing single-factor non-parametric methods 
# Testing single-factor designs with 2,3 or 4 levels (both within-subjects and between-subjects) 
# The ratio_sd is the maximum ratio of standard deviations of levels of the factor

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
params_likert_5       <- list(sigma_e = 1, levels = 5, flexible=FALSE)
params_likert_5_flex  <- list(sigma_e = 1, levels = 5, flexible=TRUE)
params_likert_7       <- list(sigma_e = 1, levels = 7, flexible=FALSE)
params_likert_7_flex  <- list(sigma_e = 1, levels = 7, flexible=TRUE)
params_likert_11      <- list(sigma_e = 1, levels = 11, flexible=FALSE)
params_likert_11_flex <- list(sigma_e = 1, levels = 11, flexible=TRUE)

use_parameters <- function(family, ratio){
  params <- switch(family, 
    norm          = params_norm, 
    likert_5      = params_likert_5, 
    likert_5_flex = params_likert_5_flex, 
    likert_7      = params_likert_7, 
    likert_7_flex = params_likert_7_flex,
    likert_11     = params_likert_11, 
    likert_11_flex = params_likert_11_flex, 
  )

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- c(0.1, 0.5) # Specifies the min and max of a uniform range
  params$ratio_sd = ratio

  params
}

# Testing single-factor designs with 2,3 or 4 levels (both within-subjects and between-subjects) 
designs <- list(2, 3, 4, 2, 3, 4)
within <- list(1, 1, 1, 0, 0, 0)

formula = Y ~ X1 + Error(factor(subject)) 
vars = c("X1") 

# Normal distribution + some ordinal scales
distributions <- c("norm", "likert_5", "likert_5_flex", "likert_7", "likert_7_flex")

ratios_sd <- c(1, 1.5, 2, 2.5, 3)  # max SD ratios (unequal variances)

effects <- c(0)
names(effects) <- vars

# Cell sizes (n in the paper)
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Type_I_single_factor"

# Set the seed for reproducibility
set.seed(6134)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(desId = 1:length(designs), .combine=rbind) %do% { 
      foreach(family = distributions, .combine=rbind) %do% {
        foreach(n = Ns, .combine=rbind) %do% {
          foreach(effId = 1:length(effects), .combine=rbind) %do% {
            foreach(ratio = ratios_sd, .combine=rbind) %do% {
              # The test function is parallelized (using multiple cores)
              repeat_test_custom(
                nlevels=designs[[desId]], 
                within=within[[desId]], 
                n=n, 
                coeffs=effects[effId],
                family=family,
                params=use_parameters(family, ratio),
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
    }
})

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(results, file = csvfile, row.names=FALSE, quote=F)


