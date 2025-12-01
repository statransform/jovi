rm(list=ls())
library(lmerTest)
library(ARTool)

library(tidyr)
library(dplyr)
library(broom)

# Parallel computation
library(foreach)
library(doParallel)

source("simulation-utils.R")

# https://yingji15.github.io/2019-08-17-inverse-normal-transformation/
# https://www.biostars.org/p/80597/
# use rankit version where c = 0.5, based on https://www.researchgate.net/publication/286889581_Impact_of_Rank-Based_Normalizing_Transformations_on_the_Accuracy_of_Test_Scores
INT = function(x){
  qnorm((rank(x) - 0.5)/length(x))
}

# Extract the p-values from the result objects returned by summary() or anova()
get_p_values <- function(model, vars, is_lmer = FALSE) {
  if(is_lmer) return(anova(model)[vars, "Pr(>F)"])
  else if(is.null(model$formula)){
    return((tidy(summary(model)[[2]][[1]]) %>% filter(term %in% vars) %>% select(p.value))[[1]])
  } else {
    anov <- anova(model)
    return(anov[anov$Term %in% vars, "Pr(>F)"])
  }
}

compare_p_values_aov <- function(df){ 
  model_par <- suppressMessages(aov(Y ~ X1*X2 + Error(subject), data=df)) # Parametric
  model_art <- suppressMessages(art(Y ~ X1*X2 + Error(factor(subject)), data=df)) # ARTool
  model_rnk <- suppressMessages(aov(rank(Y) ~ X1*X2 + Error(subject), data=df)) # RNK
  model_int <- suppressMessages(aov(INT(Y) ~ X1*X2 + Error(subject), data=df)) # INT

  vars <- c("X1", "X2", "X1:X2")
  c(get_p_values(model_par, vars), 
    get_p_values(model_rnk, vars), 
    get_p_values(model_art, vars), 
    get_p_values(model_int, vars)
  )
}


################################
# Distribution parameters used in simulation
params_normal <- list(sigma_s = 0.4, sigma_e = 1)
params_lognormal <- list(sigma_s = 0.4, sigma_e = 1, mean_target = 1)
params_cauchy <- list(sigma_s = 0.4, gamma = 1)
params_exponential <- list(sigma_s = 0.3, mean_target = 0.5)
params_poisson <- list(sigma_s = 0.3, mean_target = 3)
params_binomial <- list(sigma_s = 0.3, size = 10, p_target = 0.1)

generateDataset <- function(n, design, effects, distr){
  params <- switch(distr, 
    normal = params_normal, 
    lognormal = params_lognormal, 
    cauchy = params_cauchy, 
    exponential = params_exponential, 
    poisson = params_poisson, 
    binomial = params_binomial
  )

  simulate_response(
    nlevels = design, nsubjects = n, reps = 1,
    coeffs = effects, family = distr, params = params
  )
}


##################################
# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, effects, distr) {
  results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
    tryCatch(
      {
        compare_p_values_aov(generateDataset(n, design, effects, distr))
      }, 
      error = function(cond) {
        # do nothing
      }, finally = {
        # do nothing
      }
    )
  }

  # From p-values to positive rates that represent (depending on whether there is a true effect) either Type I error rates (false positives) or power (true positives) 
  res.05 <- round(colMeans(results<.05, na.rm = TRUE), digits = 4) # alpha = .05
  res.01 <- round(colMeans(results<.01, na.rm = TRUE), digits = 4) # alpha = .01

  designStr <- paste(design, collapse="x")

  # Split the results into separate rows 
  return(tribble(~n, ~design, ~distr, ~method, ~alpha, ~effects, ~rates,
      n, designStr, distr, "PAR", 0.05, effects, res.05[1:3],   
      n, designStr, distr, "RNK", 0.05, effects, res.05[4:6],
      n, designStr, distr, "ART", 0.05, effects, res.05[7:9],
      n, designStr, distr, "INT", 0.05, effects, res.05[10:12],

      n, designStr, distr, "PAR", 0.01, effects, res.01[1:3],   
      n, designStr, distr, "RNK", 0.01, effects, res.01[4:6],
      n, designStr, distr, "ART", 0.01, effects, res.01[7:9],
      n, designStr, distr, "INT", 0.01, effects, res.01[10:12]
    )
  )
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT.

# B. 4 x 3 repeated measures
design = c(4,3)

# C. Continuous distributions (equal variance, full) and discrete distribution: binom (size = 10, prob = 0.1) and Poisson
distributions <- c("normal", "lognormal", "exponential", "cauchy", "binomial", "poisson")

distributions <- c("lognormal")

# D. Various combinations of effects
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
            8, 0, 0), 
           ncol = 3, byrow = TRUE)

effects <- matrix(c(
            2, 0, 0), 
            ncol = 3, byrow = TRUE)

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 100

filename = "1_test_4x3_Ratio"

# Set the seed for reproducibility
#set.seed(1234)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
    results <- foreach(distr = distributions, .combine=rbind) %do% {
      foreach(n = Ns, .combine=rbind) %do% {
        foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
          # The test function is parallelized (using multiple cores)
          test(repetitions = R, n = n, design = design, effects = effects[effId,], distr = distr)
        }
      }
    }
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(rates, names_sep = "_")
colnames(res)[6:8]=c("effectX1","effectX2","effectX1X2")
colnames(res)[9:11]=c("rateX1","rateX2","rateX1X2")


# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)
