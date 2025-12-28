# Functions for analysis and simulation
# Author: Theophanis Tsandilas, Dec 2025

library(lmerTest)
library(ARTool)
library("emmeans")

library(broom)
library(dplyr)

# Implementation of the Inverse Normal Transformation
# https://yingji15.github.io/2019-08-17-inverse-normal-transformation/
# https://www.biostars.org/p/80597/
# use rankit version where c = 0.5, based on https://www.researchgate.net/publication/286889581_Impact_of_Rank-Based_Normalizing_Transformations_on_the_Accuracy_of_Test_Scores
INT = function(x){
  qnorm((rank(x) - 0.5)/length(x))
}

# Extract the p-values from the models (either lmer or aov)
get_p_values <- function(model, vars, is_aov = TRUE) {
  if(!is_aov) return(anova(model)[vars, "Pr(>F)"])
  else if(is.null(model$formula)){ # Handling AOV objects is more complicated
    summary_object <- summary(model)
    if(length(summary_object) == 1){ # This occurs when there are only between-subjects factors
      res <- tidy(summary_object[[1]][[1]])
    } 
    else { # When there is at list a within-subjects factor
      res <- bind_rows(tidy(summary_object[[1]][[1]]), tidy(summary_object[[2]][[1]])) 
    }

    res <- res  %>% # Get both the between- and within-subjects terms
        filter(term %in% vars) %>% # Filter the relevant rows
        arrange(match(term, vars)) %>% # Order them
        dplyr::select(p.value) # And select the p-value
    return(res[[1]])
  } else {
    anov_object <- anova(model)
    return(anov_object[anov_object$Term %in% vars, "Pr(>F)"])
  }
}

# Returns a vector with all p-values for all methods
compare_p_values <- function(df, 
  formula = Y ~ X1*X2 + (1|subject), 
  vars = c("X1", "X2", "X1:X2") # Specify the terms of interest. P-values will be returned with the specified order
){  
  is_aov <- "Error" %in% all.names(formula) # Identifies if it should use lmer or aov

  if(is_aov) { # Use aov
    model_par <- suppressMessages(do.call(aov, list(formula, df))) # Parametric
    model_art <- suppressMessages(do.call(art, list(formula, df))) # ARTool
    model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
    model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT
  } 
  else { # Use lmer
    model_par <- suppressMessages(do.call(lmer, list(formula, data=df))) # Parametric
    model_art <- suppressMessages(do.call(art, list(formula, data=df))) # ARTool
    model_rnk <- suppressMessages(do.call(lmer, list(update(formula, rank(.) ~ .), data=df))) # RNK
    model_int <- suppressMessages(do.call(lmer, list(update(formula, INT(.) ~ .), data=df))) # INT
  }

  c(PAR = suppressMessages(get_p_values(model_par, vars, is_aov)), 
    ART = suppressMessages(get_p_values(model_art, vars, is_aov)), 
    RNK = suppressMessages(get_p_values(model_rnk, vars, is_aov)), 
    INT = suppressMessages(get_p_values(model_int, vars, is_aov))
  )
}

# Contrasts ###########################################################
# P-values for contrasts, where expr can be ~ X2 if we are interested in contrasts for the second factor 
get_p_values_contrasts <- function(model, expr) {
  if(is.null(names(model)) || is.null(model$formula)) {
      as.data.frame(suppressMessages(contrast(emmeans(model, expr), method="pairwise", interaction=TRUE)))$p.value
  } 
  else { # This is the case of ART
    as.data.frame(suppressMessages(art.con(model, expr, interaction=TRUE)))$p.value
  }
}

# Returns a vector with all p-values for all methods
compare_p_values_contrasts <- function(df, 
  formula = Y ~ X1*X2 + (1|subject), 
  expr = ~X2 # Specify the contrasts of interest
){  
  is_aov <- "Error" %in% all.names(formula) # Identifies if it should use lmer or aov
 
  if(is_aov) { # Use aov
    # See: https://stackoverflow.com/questions/78664364/why-do-i-need-to-hardcode-the-formula-in-emmeans-instead-of-dynamically-extrac
    model_par <- suppressMessages(do.call(aov, list(formula, df))) # Parametric
    model_art <- suppressMessages(do.call(art, list(formula, df))) # ARTool
    model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
    model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT
  } 
  else { # Use lmer
    model_par <- suppressMessages(do.call(lmer, list(formula, data=df))) # Parametric
    model_art <- suppressMessages(do.call(art, list(formula, data=df))) # ARTool
    model_rnk <- suppressMessages(do.call(lmer, list(update(formula, rank(.) ~ .), data=df))) # RNK
    model_int <- suppressMessages(do.call(lmer, list(update(formula, INT(.) ~ .), data=df))) # INT
  }

  c(PAR = suppressMessages(get_p_values_contrasts(model_par, expr)), 
    ART = suppressMessages(get_p_values_contrasts(model_art, expr)), 
    RNK = suppressMessages(get_p_values_contrasts(model_rnk, expr)), 
    INT = suppressMessages(get_p_values_contrasts(model_int, expr))
  )
}


################################################################
# Tests over a large number of iterations 
################################################################
# This the most common simulation process
# simulate_function: simulation function
# params_function: function for accessing the various distribution parameters
repeat_test <- function(
  nlevels=c(4,3), 
  within = c(1,1),
  n=20, 
  coeffs=c("X1"=0, "X2"=0, "X1:X2"=0),
  family="norm",
  params,
  formula,
  vars,
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
        
        compare_p_values(data, formula, vars)
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

  designStr <- paste(nlevels, collapse="x")

  ndummies <- length(vars) - length(res.05)/4
  dummies <- rep(0, ndummies) # Complete with zeros irrelevant rate results. Useful when producing results for multiple designs with different numbers of factors
  # Split the results into separate rows 
  return(tribble(~n, ~design, ~family, ~method, ~alpha, ~effect, ~rate,
      n, designStr, family, "PAR", 0.05, coeffs, c(res.05[grep("PAR", names(res.05))], dummies),
      n, designStr, family, "RNK", 0.05, coeffs, c(res.05[grep("RNK", names(res.05))], dummies),  
      n, designStr, family, "ART", 0.05, coeffs, c(res.05[grep("ART", names(res.05))], dummies),
      n, designStr, family, "INT", 0.05, coeffs, c(res.05[grep("INT", names(res.05))], dummies),

      n, designStr, family, "PAR", 0.01, coeffs, c(res.01[grep("PAR", names(res.01))], dummies),  
      n, designStr, family, "RNK", 0.01, coeffs, c(res.01[grep("RNK", names(res.01))], dummies),  
      n, designStr, family, "ART", 0.01, coeffs, c(res.01[grep("ART", names(res.01))], dummies),
      n, designStr, family, "INT", 0.01, coeffs, c(res.01[grep("INT", names(res.01))], dummies)
    ) %>% {if(!is.null(params$ratio_sd)) mutate(., sd_ratio=params$ratio_sd, .before=4) else .} %>% # Adding column for heterscedastic data (if relevant)
      {if(!is.null(params$ratio_missing)) mutate(., missing_ratio=params$ratio_missing, .before=4) else .} # Adding column for missing data (if relevant)
  )
}


# Variation of the simulation function that conducts contrast tests 
# params_function: This is the function for acessing the various distribution parameters
repeat_test_contrasts <- function(
  nlevels=c(4,3), 
  within = c(1,1),
  n=20, 
  coeffs=c("X1"=0, "X2"=0, "X1:X2"=0),
  family="norm",
  params,
  formula,
  expr=~X2,
  iterations = 1000 
) {
  results <- foreach(rid = 1:iterations, .combine=rbind) %dopar% {
    tryCatch(
      {
        compare_p_values_contrasts(simulate_response(nlevels, within, n, coeffs, 
            # For ordinal data, the family name also includes the levels and threshold type: "ordonal-5-flex", "ordinal-7", ... 
            sub("_.*", "", family), params), 
          formula, expr
        )
      }, 
      error = function(cond) {
        # do nothing
      }, finally = {
        # do nothing
      }
    )
  }

  # From p-values to positive rates that represent (depending on whether there is a true effect) either Type I error rates (false positives) or power (true positives) 
  res.05 <- colMeans(results<.05, na.rm = TRUE) # alpha = .05 
  res.01 <- colMeans(results<.01, na.rm = TRUE) # alpha = .01

  designStr <- paste(nlevels, collapse="x")

  meanround <- function(values){
    round(mean(values), digits = 4) 
  } 

  # Split the results into separate rows 
 # I take the average rates across pairwise contrasts 
  return(tribble(~n, ~design, ~family, ~method, ~alpha, ~effect, ~rate,
      n, designStr, family, "PAR", 0.05, coeffs, meanround(res.05[grep("PAR", names(res.05))]),   
      n, designStr, family, "RNK", 0.05, coeffs, meanround(res.05[grep("RNK", names(res.05))]),   
      n, designStr, family, "ART", 0.05, coeffs, meanround(res.05[grep("ART", names(res.05))]),
      n, designStr, family, "INT", 0.05, coeffs, meanround(res.05[grep("INT", names(res.05))]),

      n, designStr, family, "PAR", 0.01, coeffs, meanround(res.01[grep("PAR", names(res.01))]),   
      n, designStr, family, "RNK", 0.01, coeffs, meanround(res.01[grep("RNK", names(res.01))]),   
      n, designStr, family, "ART", 0.01, coeffs, meanround(res.01[grep("ART", names(res.01))]),
      n, designStr, family, "INT", 0.01, coeffs, meanround(res.01[grep("INT", names(res.01))])
    )
  )
}
