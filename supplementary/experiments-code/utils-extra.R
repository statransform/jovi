# Extra functions for analysis and simulation for additional methods: ART with media alignment, etc.
# Author: Theophanis Tsandilas, Dec 2025

# Custom implementation of ART for repeated measures with two factors: Y ~ X1*X2 + Error(subject)
# fun: it can be median or mean
art_custom <- function(df, fun = median) {

  Ai <- aggregate(Y ~ X1, data = df, fun)
  Bj <- aggregate(Y ~ X2, data = df, fun) 
  AiBj <- aggregate(Y ~ X1+X2, data = df, fun) 
  cmean <- aggregate(Y ~ X1+X2, data = df, fun) 

  df <- df %>% group_by(X1) %>% mutate(Ai = fun(Y)) %>% 
    group_by(X2) %>% mutate(Bj = fun(Y)) %>% 
    group_by(X1,X2) %>% mutate(AiBj = fun(Y)) %>% mutate(Cell = fun(Y))

  df$m <- fun(df$Y)

  df$artA <- df$Y - df$Cell + df$Ai - df$m
  df$artB <- df$Y - df$Cell + df$Bj - df$m
  df$artAB <- df$Y - df$Cell + df$AiBj - df$Ai - df$Bj + df$m 

  df$rnkA <- rank(round(df$artA, digits = 7))
  df$rnkB <- rank(round(df$artB, digits = 7))
  df$rnkAB <- rank(round(df$artAB, digits = 7))

  modelA <- suppressMessages(aov(rnkA ~ X1*X2 + Error(factor(subject)), data=df))
  modelB <- suppressMessages(aov(rnkB ~ X1*X2 + Error(factor(subject)), data=df))
  modelAB <- suppressMessages(aov(rnkAB ~ X1*X2 + Error(factor(subject)), data=df))

  c(get_p_values(modelA, vars=c("X1")), get_p_values(modelB, vars=c("X2")), get_p_values(modelAB, vars=c("X1:X2")))
}

# Overwrite the function in utils-analysis.R (with alternative implementation of ART)
compare_p_values_median <- function(df, 
  formula = Y ~ X1*X2 + Error(factor(subject)), 
  vars = c("X1", "X2", "X1:X2") # Specify the terms of interest. P-values will be returned with the specified order
){  
  model_par <- suppressMessages(do.call(aov, list(formula, df))) # Parametric
  model_art <- suppressMessages(do.call(art, list(formula, df))) # ARTool

  #model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
  model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT
  

  c(PAR = suppressMessages(get_p_values(model_par, vars, is_aov = TRUE)), 
    ARTmean = suppressMessages(get_p_values(model_art, vars, is_aov = TRUE)), 
    ARTmedian = suppressMessages(art_custom(df, fun = median)), 
    INT = suppressMessages(get_p_values(model_int, vars, is_aov = TRUE))
  )
}


############################################################################
# Tests over a large number of iterations - adapted for ART median alignment
############################################################################
repeat_test_median <- function(
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
        
        compare_p_values_median(data, formula, vars)
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
      n, designStr, family, "ART", 0.05, coeffs, c(res.05[grep("ARTmean", names(res.05))], dummies),  
      n, designStr, family, "ART-MED", 0.05, coeffs, c(res.05[grep("ARTmedian", names(res.05))], dummies),
      n, designStr, family, "INT", 0.05, coeffs, c(res.05[grep("INT", names(res.05))], dummies),

      n, designStr, family, "PAR", 0.01, coeffs, c(res.01[grep("PAR", names(res.01))], dummies),  
      n, designStr, family, "ART", 0.01, coeffs, c(res.01[grep("ARTmean", names(res.01))], dummies),  
      n, designStr, family, "ART-MED", 0.01, coeffs, c(res.01[grep("ARTmedian", names(res.01))], dummies),
      n, designStr, family, "INT", 0.01, coeffs, c(res.01[grep("INT", names(res.01))], dummies)
    ) %>% {if(!is.null(params$ratio_sd)) mutate(., sd_ratio=params$ratio_sd, .before=4) else .} %>% # Adding column for heterscedastic data (if relevant)
      {if(!is.null(params$ratio_missing)) mutate(., missing_ratio=params$ratio_missing, .before=4) else .} # Adding column for missing data (if relevant)
  )
}
