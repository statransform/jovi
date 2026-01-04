# Permutation tests
# Author: Theophanis Tsandilas, Jan 2026

# Extracts F value from aov object
extract_F <- function(fit){
  s <- summary(fit)
  Fs <- c()
  names_out <- c()

  for(block in s){
    if(is.list(block)){
      tab <- block[[1]]
      
      keep <- !grepl("Residual", rownames(tab), ignore.case = TRUE)

      Fs <- c(Fs, tab[keep, "F value"])
      names_out <- c(names_out, rownames(tab)[keep])
    }
  }

  names(Fs) <- names_out
  Fs
}

# Extracts p-value from aov object
extract_p <- function(fit){
  s <- summary(fit)
  pvalues <- c()
  names_out <- c()

  for(block in s){
    if(is.list(block)){
      tab <- block[[1]]
      
      keep <- !grepl("Residual", rownames(tab), ignore.case = TRUE)

      pvalues <- c(pvalues, tab[keep, "Pr(>F)"])
      names_out <- c(names_out, rownames(tab)[keep])
    }
  }

  names(pvalues) <- names_out
  pvalues
}


# We assume that the formula declares the independent variable of interest (after transformation)  
# It uses the aov function to conduct ANOVAs -- assumes balanced designs
# (for LMER, the code should be adapted)
# Works for within-subject, between-subjects, and mixed designs
permutation_test <- function(df, formula, B = 5000){
  # Step 1 : detect within vs between factors
  terms <- all.vars(formula[[3]])
  factors <- terms[terms != "subject"]

  # Extract (as string) the dependent variable of interest
  y_str <- toString(formula[2])

  # The vector declares which factors are within-subjects
  # e.g., is_within = c("X1" = TRUE, "X2" = TRUE)
  is_within <- sapply(factors, function(f){
    any(tapply(df[[f]], df$subject, function(x) length(unique(x)) > 1))
  })

  # Step 2: construct permutation function
  permutate <- function(){
    d <- df
    if(any(is_within)){ 
      # Permutate within each subject
      for(s in unique(d$subject)){
        idx <- d$subject == s
        d[idx, y_str] <- sample(d[idx, y_str])
      }
    } else {
      # For between-subjects designs, permutate across all observations
      d[,y_str] <- sample(d[,y_str]) 
    }

    fit <- aov(formula, data = d)
    extract_F(fit)
  }

  # Step 3: observed statistics
  fit_obs <- aov(formula, data = df)
  T_obs <- extract_F(fit_obs)

  # Step 4: permutation distribution
  T_perm <- replicate(B, permutate())

  # Step 5: p-values
  pvals <- rowMeans(T_perm >= T_obs)

  pvals
}
