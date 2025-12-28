# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Comparison of Type I error rates of RNK and INT methods against the multifactorial generalizations of the van der Waerden test 
# and the Kruskal-Wallis and Friedman tests: http://www.uni-koeln.de/~luepsen/R/
# 4x3 within-subjects, 2x3 within-subjects, and 2x4 mixed-subjects
# We fix n=20

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("np.anova.R") # From http://www.uni-koeln.de/~luepsen/R/

source("utils-data-generation.R")
source("utils-analysis.R")
source("utils-extra.R")
library("POSSA") # to get access to the get_p function

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

# (1) 4 x 3 within-subjects, (2) 2x3 within-subjects, and (3) 2 x 4 mixed design (one between-subjects and one repeated-measures factor)
designs <- list(c(4,3), c(2,3), c(2,4))
within <- list(c(1,1), c(0,0), c(0,1))

# Notice that I include slope random effects in my models, which is required by the np.anova functions  
formula1 = Y ~ X1*X2 + Error(subject/(X1*X2))
formula2 = Y ~ X1*X2 
formula3 = Y ~ X1*X2 + Error(subject/X2) 
formulas <- c(formula1, formula2, formula3)

vars = c("X1", "X2", "X1:X2") 

# Continuous distributions (equal variance, full) and discrete distribution: binom (size = 10, prob = 0.1) and Poisson
distributions <- c("norm", "lnorm", "exp", "binom", "poisson", "likert")

# Various combinations of effects
effects <- matrix(c(0, 0, 0,
            0.5, 0.5, 0,
            1, 1, 0,
            2, 2, 0,
            4, 4, 0,
            8, 8, 0,
            0.5, 0, 0,
            1, 0, 0,
            2, 0, 0,
            4, 0, 0,
            8, 0, 0,
            0, 0.5, 0,
            0, 1, 0,
            0, 2, 0,
            0, 4, 0,
            0, 8, 0), 
           ncol = 3, byrow = TRUE)

colnames(effects) <- vars

# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Type_I_vdWaerden"

# Set the seed for reproducibility
set.seed(3207)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

options (contrasts=c("contr.sum","contr.poly")) # See http://www.uni-koeln.de/~luepsen/R/manual.pdf

time <- system.time({ 
    results <- foreach(desId = 1:length(designs), .combine=rbind) %do% { 
      foreach(family = distributions, .combine=rbind) %do% {
        foreach(n = Ns, .combine=rbind) %do% {
          foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
            # The test function is parallelized (using multiple cores)
            repeat_test_custom(
              nlevels=designs[[desId]], 
              within=within[[desId]], 
              n=n, 
              coeffs=effects[effId,],
              family=family,
              params=use_parameters(family),
              formula=formulas[[desId]],
              vars=vars,
              iterations = R,
              methods = c("RNK", "INT", "VDW", "KWF"),
              compare_function = compare_p_values_gen
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


