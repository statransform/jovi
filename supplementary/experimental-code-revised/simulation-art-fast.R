# Fast implementation of ART - LMER using lmertest and Satterthwaite approximation
# This is of interest for simulation with large number of subjects, where AOV becomes expensive
fast_art_p <- function(m) {
  # Recover ORIGINAL formula (with random-effects preserved)
  original_formula <- eval(m$formula)

  # aligned columns
  aligned <- m$aligned.ranks
  term_names <- colnames(aligned)

  # Build full data frame with predictors INCLUDING 'subject'
  predictors <- get_all_vars(original_formula, data = m$data)

  df_base <- cbind(aligned, predictors)

  # Extract RHS from original formula
  rhs <- original_formula[[3]]

  results <- numeric(length(term_names))
  names(results) <- term_names

  for (term in term_names) {
    df <- df_base
    df$.y <- df[[term]]
    df <- df[,-c(1:length(term_names))] # And remove the columns that could cause confusion 

    f <- reformulate(deparse(rhs), response = ".y")

    model <- lmerTest::lmer(f, data = df)
    tab <- anova(model, type = 3)
    results[term] <- tab[term, "Pr(>F)"]
  }

  as.data.frame(as.list(results))
}
