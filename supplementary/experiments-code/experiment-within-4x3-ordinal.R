# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on ordinal scales (with 5, 7, and 11 levels) for a 4x3 within-subjects design
# We evaluate both flexible and equidistant thresholds

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("utils-data-generation.R")
source("utils-analysis.R")

################################
# Distribution parameters used in simulation
params_likert_5       <- list(sigma_e = 1, levels = 5, flexible=FALSE)
params_likert_5_flex  <- list(sigma_e = 1, levels = 5, flexible=TRUE)
params_likert_7       <- list(sigma_e = 1, levels = 7, flexible=FALSE)
params_likert_7_flex  <- list(sigma_e = 1, levels = 7, flexible=TRUE)
params_likert_11      <- list(sigma_e = 1, levels = 11, flexible=FALSE)
params_likert_11_flex <- list(sigma_e = 1, levels = 11, flexible=TRUE)

use_parameters <- function(family){
  params <- switch(family, 
    likert_5      = params_likert_5, 
    likert_5_flex = params_likert_5_flex, 
    likert_7      = params_likert_7, 
    likert_7_flex = params_likert_7_flex,
    likert_11     = params_likert_11, 
    likert_11_flex = params_likert_11_flex, 
  )

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- c(0.1, 0.5) # Specifies the min and max of a uniform range

  params
}

# 4 x 3 repeated measures
design = c(4,3)
within = c(1,1) # Both factors are within-subjects
formula = Y ~ X1*X2 + Error(factor(subject)) 
vars = c("X1", "X2", "X1:X2") 

# Ordinal scales of 5, 7, and 11 levels, with equidistant or flexible thresholds
distributions <- c("likert_5", "likert_5_flex", "likert_7", "likert_7_flex", "likert_11", "likert_11_flex")

# Various combinations of effects
effects <- matrix(c(0, 0, 0,
            0.5, 0, 0,
            1, 0, 0,
            2, 0, 0,
            4, 0, 0,
            8, 0, 0,
            0.5, 0.5, 0,
            1, 1, 0,
            2, 2, 0,
            4, 4, 0,
            8, 8, 0), 
           ncol = 3, byrow = TRUE)

colnames(effects) <- vars

# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
Ns <- c(10, 20, 30) 

# 5000 iterations
R <- 5000

filename = "Type_I_ordinal"

# Set the seed for reproducibility
set.seed(5224)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(family = distributions, .combine=rbind) %do% {
      foreach(n = Ns, .combine=rbind) %do% {
        foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
          # The test function is parallelized (using multiple cores)
          repeat_test(
            nlevels=design, 
            within=within, 
            n=n, 
            coeffs=effects[effId,],
            family=family,
            params=use_parameters(family),
            formula=formula,
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


