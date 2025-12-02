library(lmerTest)
library(ARTool)
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
        select(p.value) # And select the p-value
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
    model_par <- suppressMessages(aov(formula, data=df)) # Parametric
    model_art <- suppressMessages(art(formula, data=df)) # ARTool
    model_rnk <- suppressMessages(aov(update(formula, rank(.) ~ .), data=df)) # RNK
    model_int <- suppressMessages(aov(update(formula, INT(.) ~ .), data=df)) # INT
  } 
  else { # Use lmer
    model_par <- suppressMessages(lmer(formula, data=df)) # Parametric
    model_art <- suppressMessages(art(formula, data=df)) # ARTool
    model_rnk <- suppressMessages(lmer(update(formula, rank(.) ~ .), data=df)) # RNK
    model_int <- suppressMessages(lmer(update(formula, INT(.) ~ .), data=df)) # INT
  }

  c(PAR = suppressMessages(get_p_values(model_par, vars, is_aov)), 
    ART = suppressMessages(get_p_values(model_art, vars, is_aov)), 
    RNK = suppressMessages(get_p_values(model_rnk, vars, is_aov)), 
    INT = suppressMessages(get_p_values(model_int, vars, is_aov))
  )
}

# This the iterative simulation process
# generationFunction: This is the data generation function that knows how to choose the various distributions parameters
repeat_test <- function(
  nlevels=c(4,3), 
  within = c(1,1),
  n=20, 
  coeffs=c("X1"=0, "X2"=0, "X1:X2"=0),
  family="norm",
  params_function,
  formula,
  vars,
  iterations = 1000 
) {
  results <- foreach(rid = 1:iterations, .combine=rbind) %dopar% {
    tryCatch(
      {
        compare_p_values(simulate_response(nlevels, within, n, coeffs, 
          # For ordinal data, the family name also includes the levels and threshold type: "ordonal-5-flex", "ordinal-7", ... 
          sub("_.*", "", family), 
          params_function(family)), 
          formula, vars)
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

  # Split the results into separate rows 
  return(tribble(~n, ~design, ~family, ~method, ~alpha, ~effect, ~rate,
      n, designStr, family, "PAR", 0.05, coeffs, res.05[grep("PAR", names(res.05))],   
      n, designStr, family, "RNK", 0.05, coeffs, res.05[grep("RNK", names(res.05))],   
      n, designStr, family, "ART", 0.05, coeffs, res.05[grep("ART", names(res.05))],
      n, designStr, family, "INT", 0.05, coeffs, res.05[grep("INT", names(res.05))],

      n, designStr, family, "PAR", 0.01, coeffs, res.01[grep("PAR", names(res.01))],   
      n, designStr, family, "RNK", 0.01, coeffs, res.01[grep("RNK", names(res.01))],   
      n, designStr, family, "ART", 0.01, coeffs, res.01[grep("ART", names(res.01))],
      n, designStr, family, "INT", 0.01, coeffs, res.01[grep("INT", names(res.01))]
    )
  )
}

