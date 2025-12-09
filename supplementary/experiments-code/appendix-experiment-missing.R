# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT under missing data (when variances are either equal or not)
# We only consider that missing data are random occurences (10%, 20%, and 30% of the total data values)
# 4x3 within-subjects and 2x4 mixed-subjects
# Normal distributions

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

use_parameters <- function(family, ratio_sd, ratio_missing){
  params <- switch(family, 
    norm          = params_norm, 
  )

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- c(0.1, 0.5) #Specifies the min and max of a uniform range
  
  params$ratio_missing = ratio_missing
  params$ratio_sd = ratio_sd

  params
}

# (1) 4 x 3 within-subjects, and (2) 2 x 4 mixed design
designs <- list(c(4,3), c(2,4))
within <- list(c(1,1), c(0,1))

formula = Y ~ X1*X2 + (1|subject) # We are going to use LMER and Type III errors
vars = c("X1", "X2", "X1:X2") 

# Normal distribution
distributions <- c("norm")

ratios_sd <- c(1, 2)  # max SD ratios (unequal variances)
ratios_missing <- c(0.1, 0.2, 0.3) # missing data ratios 

effects <- matrix(c(0, 0, 0,
            0.5, 0, 0,
            1, 0, 0,
            2, 0, 0,
            4, 0, 0,
            8, 0, 0), 
           ncol = 3, byrow = TRUE)

colnames(effects) <- vars

# Cell sizes (n in the paper)
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Type_I_missing"

# Set the seed for reproducibility
set.seed(2092)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(desId = 1:length(designs), .combine=rbind) %do% { 
      foreach(family = distributions, .combine=rbind) %do% {
        foreach(n = Ns, .combine=rbind) %do% {
          foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
            foreach(ratio_sd = ratios_sd, .combine=rbind) %do% {
              foreach(ratio_mis = ratios_missing, .combine=rbind) %do% {
               # The test function is parallelized (using multiple cores)
                repeat_test(
                  nlevels=designs[[desId]], 
                  within=within[[desId]], 
                  n=n, 
                  coeffs=effects[effId,],
                  family=family,
                  params=use_parameters(family, ratio_sd, ratio_mis),
                  formula=formula,
                  vars=vars,
                  iterations = R
                )
              }
            }
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


