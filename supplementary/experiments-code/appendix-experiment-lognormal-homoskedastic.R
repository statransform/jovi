# Author: Theophanis Tsandilas, Dec 2025
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on special lognormal distributions with homoskedastic responses

rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)
library(tidyr)

source("utils-data-generation.R")
source("utils-analysis.R")

##################################
# These are some extra functions 

# See https://en.wikipedia.org/wiki/Log-normal_distribution
# We control the standard deviation of the generated data so that it is equal
sdlog <- function(mean, sd) {
  sqrt(log(1 + sd^2/mean^2))  
}

meanlog <- function(mean, sd) {
  log(mean^2/sqrt(sd^2 + mean^2))
}


simulate_response_equalvar <- function(
  nlevels = c(4,3), within = c(1, 1), n = 1,
  coeffs = c("X1"=0, "X2"=0, "X1:X2"=0),
  family = c("lnorm"),
  params
) {
  family <- match.arg(family)
  sigma_s <- runif(1, min = params$sigma_s[1], max = params$sigma_s[length(params$sigma_s)])

  # -------------------------------------------------
  # Create design with correct within/between logic
  # -------------------------------------------------
  design <- make_design(nlevels, within, n)

  # -------------------------------------------------
  # Build effects based on number of factors
  # -------------------------------------------------
  k <- length(nlevels)
  # Currently supporting up to three factors
  effects <- switch(
    k,
    with(design, coeffs["X1"]*x1),
    with(design, coeffs["X1"]*x1 + coeffs["X2"]*x2 + coeffs["X1:X2"]*x1*x2),
    with(design, coeffs["X1"]*x1 + coeffs["X2"]*x2 + coeffs["X3"]*x3 + coeffs["X1:X2"]*x1*x2)
  )

  mu <- mu_lognormal(effects, sigma_s, params$sigma_e, params$mean_target)

  # -------------------------------------------------
  # Random subject effects
  # -------------------------------------------------
  subjects <- unique(design$subject)
  s <- rnorm(length(subjects), 0, sigma_s)
  design$s_eff <- s[ match(design$subject, subjects) ]

  # -------------------------------------------------
  # Linear predictor
  # -------------------------------------------------
  eta <- effects + design$s_eff
  means <- exp(mu + eta)

  # Determine the latent parameters
  meanlogs <- meanlog(means, params$sd_target)
  sdlogs <- sdlog(means, params$sd_target)

  Y <- mapply(function(x){rlnorm(1,meanlogs[x],sdlogs[x])}, 1:length(meanlogs))

  # Ensure that categorical variables and the random effect are coded as factors
  data.frame(design, eta = eta, Y = Y, stringsAsFactors = TRUE)
}

################################
# Distribution parameters used in simulation
use_parameters <- function(family){
  # Add the normalization parameter to normalize the effects at the latent space
  params  <- list(sigma_e = 1, mean_target = 1, sd_target = 1) 

  # Choose a random standard deviation for the random subject effect between 0.1 and 0.5
  params$sigma_s <- c(0.1, 0.5) #Specifies the min and max of a uniform range

  params$sigma_e <- switch(family, 
    lnorm_0.2  = 0.2,
    lnorm_0.4  = 0.4,
    lnorm_0.6  = 0.6,
    lnorm_0.8  = 0.8,
    lnorm_1.0  = 1.0,
    lnorm_1.2  = 1.2
  )

  params
}

# (1) 4 x 3 within-subjects, (2) 2 x 3 between-subjects, (3) 2 x 4 mixed design
designs <- list(c(4,3), c(2,3), c(2,4))
within <- list(c(1,1), c(0,0), c(0,1))

formula = Y ~ X1*X2 + Error(factor(subject)) 
vars = c("X1", "X2", "X1:X2") 

# Log-normal distributions
distributions <- c("lnorm_0.2", "lnorm_0.4", "lnorm_0.6", "lnorm_0.8", "lnorm_1.0", "lnorm_1.2")

# Various combinations of effects
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
R <- 1# 5000

filename = "Type_I_lognormal"

# Set the seed for reproducibility
set.seed(1031)

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


