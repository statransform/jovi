# Extra functions for analysis and simulation for additional methods: ART with media alignment, ATS, etc.
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

############################################
# Code for the ATS statistic
get_ATS_p_values <- function(model) {
  return(unname(model$ANOVA.test[,3]))
}

# Overwrite the function in utils-analysis.R (with implementation of ATS)
compare_p_values_ATS <- function(df, 
  formula = Y ~ X1*X2 + Error(factor(subject)), 
  vars = c("X1", "X2", "X1:X2") # Specify the terms of interest. P-values will be returned with the specified order
){  
  model_par <- suppressMessages(do.call(aov, list(formula, df))) # Parametric
  model_ats <- suppressMessages(nparLD(update(formula, . ~ . - Error(factor(subject))), data=as.data.frame(df), subject="subject", order.warning=F, description=FALSE))
  model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
  model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT

  c(PAR = suppressMessages(get_p_values(model_par, vars, is_aov = TRUE)), 
    ATS = suppressMessages(get_ATS_p_values(model_ats)), 
    RNK = suppressMessages(get_p_values(model_rnk, vars, is_aov = TRUE)), 
    INT = suppressMessages(get_p_values(model_int, vars, is_aov = TRUE))
  )
}


############################################
# Comparison of RNK and INT methods against the multifactorial generalizations of the van der Waerden test 
# and the Kruskal-Wallis and Friedman tests
# See: http://www.uni-koeln.de/~luepsen/R/
# See: http://www.uni-koeln.de/~luepsen/R/manual.pdf

get_p_values_gen <- function(model, vars) {
  pattern <- paste0("^(", paste(vars, collapse = "|"), ")(\\s|$)")
  model[grepl(pattern, rownames(model)), "Pr(>Chi)"]
}

get_p_values_aov <- function(model, vars, has_error = TRUE) {
  if(has_error) return(unname(unlist(get_p(model))))
  else return(anova(model)[vars, "Pr(>F)"])
}

# Overwrite the function in utils-analysis.R
compare_p_values_gen <- function(df, 
  formula = Y ~ X1*X2 + Error(subject), 
  vars = c("X1", "X2", "X1:X2") # Specify the terms of interest. P-values will be returned with the specified order
){  
  df$subject = factor(df$subject)
  model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
  model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT
  model_vdw <- suppressMessages(np.anova(formula, data = as.data.frame(df), method=1)) # van der Waerden test
  model_kwf <- suppressMessages(np.anova(formula, data = as.data.frame(df), method=0)) # Kruskal-Wallis and Friedman tests

  has_error_term <- sum(grepl("Error", attr(terms(formula), "term.labels")))

  c(
    RNK = suppressMessages(get_p_values_aov(model_rnk, vars, has_error = has_error_term)), 
    INT = suppressMessages(get_p_values_aov(model_int, vars, has_error = has_error_term)),
    VDW = suppressMessages(get_p_values_gen(model_vdw, vars)),
    KWF = suppressMessages(get_p_values_gen(model_kwf, vars))
  )
}


# Non parametric tests
rank.test <- function(df) {
  nlevels <- length(levels(df[,"X1"]))
  paired <- nrow(df) > length(unique(df$subject))

  # Do either the wilcoxon or the friedman (more than two levels)
  if(paired) return (ifelse(nlevels==2, wilcox.test(Y~X1, df, paired=TRUE)$p.value, friedman.test(Y~X1|subject, df)$p.value)) 
  else return (kruskal.test(Y~X1, df)$p.value) 
}

compare_p_values_single <- function(df, 
  formula = Y ~ X1 + Error(subject), 
  vars = c("X1") # Specify the terms of interest. P-values will be returned with the specified order
){  
  model_par <- suppressMessages(do.call(aov, list(formula, df))) # Parametric
  model_rnk <- suppressMessages(do.call(aov, list(update(formula, rank(.) ~ .), df))) # RNK
  model_int <- suppressMessages(do.call(aov, list(update(formula, INT(.) ~ .), df))) # INT

  c(PAR = suppressMessages(get_p_values(model_par, vars, is_aov = TRUE)),  
    RNK = suppressMessages(get_p_values(model_rnk, vars, is_aov = TRUE)), 
    INT = suppressMessages(get_p_values(model_int, vars, is_aov = TRUE)),
    NON = suppressMessages(rank.test(df))
  )
}


############################################################################
# Tests over a large number of iterations - for custom methods
############################################################################
repeat_test_custom <- function(
  nlevels=c(4,3), 
  within = c(1,1),
  n=20, 
  coeffs=c("X1"=0, "X2"=0, "X1:X2"=0),
  family="norm",
  params,
  formula,
  vars,
  iterations = 1000,
  methods = c("PAR", "RNK", "ART", "INT"),
  methods_alt = methods,
  compare_function = compare_p_values
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
  
        do.call(compare_function, list(data, formula, vars))
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

  if(length(nlevels) == 1) {
    designStr <- paste(ifelse(within, "within", "between"), nlevels, sep="-")
  }
  else {
    designStr <- paste(nlevels, collapse="x")
  }

  ndummies <- length(vars) - length(res.05)/4
  dummies <- rep(0, ndummies) # Complete with zeros irrelevant rate results. Useful when producing results for multiple designs with different numbers of factors
  # Split the results into separate rows 
  return(tribble(~n, ~design, ~family, ~method, ~alpha, ~effect, ~rate,
      n, designStr, family, methods[1], 0.05, coeffs, c(res.05[grep(methods_alt[1], names(res.05))], dummies),
      n, designStr, family, methods[2], 0.05, coeffs, c(res.05[grep(methods_alt[2], names(res.05))], dummies),  
      n, designStr, family, methods[3], 0.05, coeffs, c(res.05[grep(methods_alt[3], names(res.05))], dummies),
      n, designStr, family, methods[4], 0.05, coeffs, c(res.05[grep(methods_alt[4], names(res.05))], dummies),

      n, designStr, family, methods[1], 0.01, coeffs, c(res.01[grep(methods_alt[1], names(res.01))], dummies),  
      n, designStr, family, methods[2], 0.01, coeffs, c(res.01[grep(methods_alt[2], names(res.01))], dummies),  
      n, designStr, family, methods[3], 0.01, coeffs, c(res.01[grep(methods_alt[3], names(res.01))], dummies),
      n, designStr, family, methods[4], 0.01, coeffs, c(res.01[grep(methods_alt[4], names(res.01))], dummies)
    ) %>% {if(!is.null(params$ratio_sd)) mutate(., sd_ratio=params$ratio_sd, .before=4) else .} %>% # Adding column for heterscedastic data (if relevant)
      {if(!is.null(params$ratio_missing)) mutate(., missing_ratio=params$ratio_missing, .before=4) else .} # Adding column for missing data (if relevant)
  )
}


