# Functions dedicated to effect size estimates
# Author: Theophanis Tsandilas, Dec 2025

library(lmerTest)
library(ARTool)
library(effectsize) # Effect size functions

library(broom)
library(dplyr)

# Implementation of the Inverse Normal Transformation
INT = function(x){
  qnorm((rank(x) - 0.5)/length(x))
}

# Partial eta squared measure
eta_values <- function(model, vars) {
  tab <- eta_squared(model)
  tab[tab$Parameter %in% vars,]$Eta2_partial
}

# Cohen's F measure
cohensf_values <- function(model, vars) {
  tab <- cohens_f(model)
  tab[tab$Parameter %in% vars,]$Cohens_f_partial    
}

# Returns a vector with all effect size point estimates for all methods
compare_effect_sizes <- function(df, 
  formula = Y ~ X1*X2 + (1|subject), 
  vars = c("X1", "X2", "X1:X2"), # Specify the terms of interest. P-values will be returned with the specified order
  measure = c("eta", "cohens")
){  
  measure <- match.arg(measure)
  effect_size_function <- {
    if(measure == "eta") eta_values
    else cohensf_values 
  }

  is_aov <- "Error" %in% all.names(formula) # Identifies if it should use lmer or aov

  if(is_aov) { # Use aov
    model_par <- suppressMessages(do.call(aov, list(formula, df))) # Parametric
    model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
    model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT
  } 
  else { # Use lmer
    model_par <- suppressMessages(do.call(lmer, list(formula, data=df))) # Parametric
    model_rnk <- suppressMessages(do.call(lmer, list(update(formula, rank(.) ~ .), data=df))) # RNK
    model_int <- suppressMessages(do.call(lmer, list(update(formula, INT(.) ~ .), data=df))) # INT
  }

  model_art <- suppressMessages(do.call(art, list(formula, df))) # ARTool

  c(PAR = suppressMessages(effect_size_function(model_par, vars)), 
    RNK = suppressMessages(effect_size_function(model_rnk, vars)), 
    ART = suppressMessages(effect_size_function(model_art, c(vars, 1:length(vars)))), # For aov formulas, variables are just enumerated 
    INT = suppressMessages(effect_size_function(model_int, vars))
  )
}

# This is to add the results of groundtruth method-simple anova over the correct data
get_groundtruth_effect_sizes <- function(df,
    formula = Y ~ X1*X2 + (1|subject), 
    vars = c("X1", "X2", "X1:X2"), # Specify the terms of interest. P-values will be returned with the specified order
    measure = c("eta", "cohens")
  ){
    
  measure <- match.arg(measure)
  effect_size_function <- {
    if(measure == "eta") eta_values
    else cohensf_values 
  }

  is_aov <- "Error" %in% all.names(formula) # Identifies if it should use lmer or aov
  if(is_aov) { # Use aov
    model <- suppressMessages(do.call(aov, list(formula, df))) 
  } 
  else { # Use lmer
    model <- suppressMessages(do.call(lmer, list(formula, data=df))) 
  }

  c(GRT = suppressMessages(effect_size_function(model, vars)))
}

################################################################
# Tests over a large number of iterations 
################################################################
# This the most common simulation process
# simulate_function: simulation function
# params_function: function for accessing the various distribution parameters
repeat_test_effect_size <- function(
  nlevels=c(4,3), 
  within = c(1,1),
  n=20, 
  coeffs=c("X1"=0, "X2"=0, "X1:X2"=0),
  family=c("norm", "lnorm", "likert"),
  params,
  formula,
  vars,
  measure = c("eta", "cohens"),
  iterations = 100,
  is_max = FALSE # If true, the coeffs define maximum effects (to be randomly chosen)
) {
  
  family <- match.arg(family)
  measure <- match.arg(measure)

  results <- foreach(rid = 1:iterations, .combine=rbind) %dopar% {
    tryCatch(
      {
        if(is_max) {
          coeffs[1:length(coeffs)] = 
                sapply(1:length(coeffs), function(x){runif(1, min=-coeffs[x], max=coeffs[x])})
        } 
        data <- { # Check if variances are equal
          if(is.null(params$ratio_sd)) simulate_response(nlevels, within, n, coeffs, sub("_.*", "", family), params)
          else simulate_heteroscedastic_response(nlevels, within, n, coeffs, sub("_.*", "", family), params)
        } 
        if(!is.null(params$ratio_missing)) { # Are there missing data?
          data <- removeCells(data, params$ratio_missing)
        }
        
        eff_sizes <- compare_effect_sizes(data, formula, vars, measure)
        if(family=="norm") { # The ground truth is the PAR
          groundtruth <- eff_sizes[startsWith(names(eff_sizes), "PAR")]
          names(groundtruth) <- paste0("GRT", seq_along(vars))  
        }
        else if(family=="lnorm") {
          data$Y <- log(data$Y)
          groundtruth <- get_groundtruth_effect_sizes(data, formula, vars, measure)
        } 
        else if(family=="likert") {
          data$Y <- data$eta
          groundtruth <- get_groundtruth_effect_sizes(data, formula, vars, measure)
        }

        c(coeffs, eff_sizes, groundtruth)
      }, 
      error = function(cond) {
        # do nothing
      }, finally = {
        # do nothing
      }
    )
  }

  designStr <- paste(nlevels, collapse="x")

  colnames(results) <- c( paste0("effect", names(coeffs)), paste0("par", vars), paste0("rnk", vars), paste0("art", vars), paste0("int", vars), paste0("grt", vars)) 
  #coeffs_prefixed <- setNames(coeffs, paste0("effect", names(coeffs)))

  as.data.frame(cbind(n = n, design=designStr, family = family, round(results,digits=4)))
}

