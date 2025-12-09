# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on various binomial distributions (varying p and size) 
# for a 4x3 within-subjects design

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("utils-data-generation.R")
source("utils-analysis.R")

################################
# Distribution parameters used in simulation
use_parameters <- function(family){
  # Add the normalization parameter to normalize the effects at the latent space
  params  <- list(sigma_e = 1, mean_target = 1) 

  # infer the size and probability from the family name
  subfix = sub("binom_", "", family)
  size = as.numeric(sub("_.*", "", subfix))
  p = as.numeric(paste("0.", sub(".*_", "", subfix), sep=""))

  list(size = size, p_target = p, sigma_s = c(0.1, 0.5))
}

# (1) 4 x 3 within-subjects, (2) 2 x 3 between-subjects, (3) 2 x 4 mixed design
designs <- list(c(4,3), c(2,3), c(2,4))
within <- list(c(1,1), c(0,0), c(0,1))

formula = Y ~ X1*X2 + Error(factor(subject)) 
vars = c("X1", "X2", "X1:X2") 

# Bionomial distributions with different k (5 and 10) and p (5%, 10, and 20%) parameters
distributions <- c("binom_5_05", "binom_5_10", "binom_5_20", "binom_10_05", "binom_10_10", "binom_10_20")

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

# Cell sizes (n in the paper)
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Type_I_binomial"

# Set the seed for reproducibility
set.seed(8206)

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
              formula=formula,
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


