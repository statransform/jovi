# Functions dedicated to effect size estimates
# Author: Theophanis Tsandilas, Dec 2025

library(lmerTest)
library(ARTool)
library(effectsize) # Effect size functions


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
  family="norm",
  params,
  formula,
  vars,
  measure = "eta",
  iterations = 1000
) {
  results <- foreach(rid = 1:iterations, .combine=rbind) %dopar% {
    tryCatch(
      {
        data <- { # Check if variances are equal
          if(is.null(params$ratio_sd)) simulate_response(nlevels, within, n, coeffs, sub("_.*", "", family), params)
          else simulate_heteroscedastic_response(nlevels, within, n, coeffs, sub("_.*", "", family), params)
        } 
        if(!is.null(params$ratio_missing)) { # Are there missing data?
          data <- removeCells(data, params$ratio_missing)
        }
        
        compare_effect_sizes(data, formula, vars, measure)
      }, 
      error = function(cond) {
        # do nothing
      }, finally = {
        # do nothing
      }
    )
  }

  designStr <- paste(nlevels, collapse="x")

  colnames(results) <- c(paste0("par", vars), paste0("rnk", vars), paste0("art", vars), paste0("int", vars)) 
  coeffs_prefixed <- setNames(coeffs, paste0("effect", names(coeffs)))

  cbind(n = n, designStr=designStr, family = family, as.data.frame(as.list(coeffs_prefixed)), round(results,digits=4))
}

