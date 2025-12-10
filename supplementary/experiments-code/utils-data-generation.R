# Code for generating experimental designs (within-, between-subjects, and mixed) and respones for a range of distributions
# Author: Theophanis Tsandilas, Dec 2025

##################################################################################
# Encodes the 1:nlevels to numerical values
# It ensures a fixed distance between the two extreme levels (equal to 1) and random distances in between
# It also ensures that the sum is zero
coding <- function(nlevels) { 
  levels <- sort(c(0, 1, runif(nlevels - 2)))
  return(levels - mean(levels))
}

# Generate the basic design matrix (balanced)
build_structure <- function(nlevels = c(2,2)) {
  k <- length(nlevels)

  # 1. Create factor labels and numeric codings, e.g., for X1: A1, A2, ... and for X2: B1, B2, ...  
  Xlabs <- lapply(seq_len(k), function(j) paste0(LETTERS[j], seq_len(nlevels[j])))
  xcoding <- lapply(nlevels, coding)

  # 2: Create a frame with the data variables
  df <- expand.grid(
    lapply(1:k, function(j) Xlabs[[j]])
  )
  names(df) <- paste0("X", 1:k)

  # 3: Match numeric coding for each level
  for (j in seq_len(k)) {
    df[[paste0("x", j)]] <- xcoding[[j]][ match(df[[paste0("X", j)]], Xlabs[[j]]) ]
  }
  
  df
}

# Create the full design, adding the participants
# n is the cell sample - the number of times a specific combination of factor levels will appear 
# For within-subjects designs, it is also the number of subjects
make_design <- function(nlevels = c(4,3), within = c(1,1), n = 20) {
  # 1. Create the basic full factorial structure
  df <- build_structure(nlevels)

  k <- length(nlevels)
  if (length(within) != k)
    stop("within must have same length as nlevels")

  # 2. Identify between-subject factors
  between_idx <- which(within == 0)

  if (length(between_idx) == 0) { # Fully within-subjects
    df$subject <- 1
  }
  else {
    # 3. Define unique between-subject combinations
    between_vars <- paste0("X", between_idx)
    df_between <- unique(df[between_vars])

    # 4. Assign subject IDs per between-subject cell
    df_between$subject <- seq_len(nrow(df_between))

    # 5. Merge subject IDs back onto the full design
    df <- merge(df, df_between, by = between_vars)

    # 6: (Optional ordering)
    df <- df[order(df$subject), ]
    # Reorder the factors: X1, X2, ...
    names = c(sort(names(df[1:k])), names(df[(k + 1) :ncol(df)])) 

    df <- df[names]
  }

  # And then extend the data frame for additional subjects
  maxsubj <- max(df$subject)
  dd <- as.data.frame(lapply(df, rep, n))
  offsets <- unlist(lapply(1:n, function(x){rep((x-1)*maxsubj, nrow(df))}))
  dd$subject <- paste("s", dd$subject + offsets, sep = "")

  dd
}


##################################################################################
# Estimating the mean of the linear model based on target distribution parameters

# Poisson distribution
mu_poisson <- function(effects, sigma_s, mean_target) {
  Emean <- mean(exp(effects))
  mu <- log(mean_target) - 0.5 * sigma_s^2 - log(Emean)
  return(mu)
}

# Exponential distribution -- Same as for poisson
mu_exponential <- function(effects, sigma_s, mean_target) {
  Emean <- mean(exp(effects))
  mu <- log(mean_target) - 0.5 * sigma_s^2 - log(Emean)
  return(mu)
}

# And lognormal distibution
mu_lognormal <- function(effects, sigma_s, sigma_e, mean_target) {
  Emean <- mean(exp(effects))
  mu <- log(mean_target) - 0.5*(sigma_s^2 + sigma_e^2) - log(Emean)
  return(mu)
}


###################################################################
# Binomial distribution - No analytical solution but we can solve it numerically 
# 1. Marginal probability for each observation (integrating over random subject effects):
marginal_p_obs <- function(eta0, sigma_s) {
  if (sigma_s == 0)
    return(1/(1 + exp(-eta0)))
  f <- function(u) 1/(1 + exp(-(eta0 + u))) * dnorm(u, 0, sigma_s)
  integrate(f, -Inf, Inf)$value
}

# 2. Average over design
avg_p <- function(mu, effects, sigma_s) {
  etas <- mu + effects
  mean(sapply(etas, marginal_p_obs, sigma_s = sigma_s))
}

# 3. Accurate numerical solution
mu_binomial <- function(effects, sigma_s, p_target, size) {

  f <- function(mu) {
    avg_p(mu, effects, sigma_s) - p_target
  }

  # initial guess = logit(p_target)
  mu0 <- qlogis(p_target)

  uniroot(f, interval = c(mu0-10, mu0+10))$root
}
###################################################################

# Ordinal Likert-like data #########################################
# Transforms a set of values to ordinal based on the given thresholds
# See: https://doi.org/10.1016/j.jesp.2018.08.009
makeDiscrete <- function(values, thresholds = c(-2,-1, 1,2)) {
  discretizeValue = function(x){
    for(i in 1:length(thresholds)) {
      if(x <= thresholds[i]) return(i)
    }
    return(length(thresholds) + 1)
  }

  sapply(values, discretizeValue)
}

# Transforms a set of values to ordinal based on the given thresholds
# Here, the thresholds take into account the variance of the data, trying to better cover the full range of values.
toOrdinal <- function(values, levels = 5, flexible = TRUE) {
  lim <- 2*sd(values) # I take 2 standard deviations that should contain the largest portion of the data (around 95% of the data values)
  thresholds <- NA
  if(flexible) {
    thresholds <- sort(runif(levels - 1))*2*lim-lim
  } else {
    thresholds <- seq(from=-lim, to=lim, by=2*lim/levels)[c(-1, -(levels + 1))]    
  }

  makeDiscrete(values, thresholds)
}


##############################################################
# params is a list with various distributional parameters
# GLM (Generalized Linear Models) formulation
# Implementation for one, two, or three factors (it can be easily exended to more factors)
##############################################################
simulate_response <- function(
  nlevels = c(4,3), within = c(1, 1), n = 1,
  coeffs = c("X1"=0, "X2"=0, "X1:X2"=0),
  family = c("norm","binom","poisson","exp","lnorm","cauchy", "likert"),
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

  # This is for lognormal distributions to evaluate lognormal distributions with lower variances
  # while keeping the effect size at the normal space (after logging) constant
  if(!is.null(params$normalize) && params$normalize){ 
    effects <- effects * params$sigma_e
    sigma_s <- sigma_s * params$sigma_e
  }

  # -------------------------------------------------
  # Mean centering (mu) from GLM mu-functions
  # -------------------------------------------------
  mu <- switch(family,
    norm     = 0,
    likert   = 0, 
    cauchy   = 0,
    lnorm    = mu_lognormal(effects, sigma_s, params$sigma_e, params$mean_target),
    exp      = mu_exponential(effects, sigma_s, params$mean_target),
    poisson  = mu_poisson(effects, sigma_s, params$mean_target),
    binom    = mu_binomial(effects, sigma_s, params$p_target, params$size)
  )

  # -------------------------------------------------
  # Random subject effects
  # -------------------------------------------------
  subjects <- unique(design$subject)
  s <- rnorm(length(subjects), 0, sigma_s)
  design$s_eff <- s[ match(design$subject, subjects) ]

  # -------------------------------------------------
  # Linear predictor
  # -------------------------------------------------
  eta <- mu + effects + design$s_eff

  # -------------------------------------------------
  # Generate Y
  # -------------------------------------------------
  Y <- switch(family,
    norm     = eta + rnorm(length(eta), 0, params$sigma_e),
    likert   = { # For ordinal, I need eta to store the latent values (that include the trial error)
      eta = eta + rnorm(length(eta), 0, params$sigma_e)
      toOrdinal(eta, levels = params$levels, flexible = params$flexible)
    },
    poisson  = rpois(length(eta), exp(eta)),
    exp      = rexp(length(eta), rate = 1/exp(eta)),
    lnorm    = rlnorm(length(eta), eta, params$sigma_e),
    binom    = rbinom(length(eta), size=params$size, prob=plogis(eta)),
    cauchy   = rcauchy(length(eta), location = eta, scale = params$gamma)
  )

  # Ensure that categorical variables and the random effect are coded as factors
  data.frame(design, eta = eta, Y = Y, stringsAsFactors = TRUE)
}

##############################################################################
# Specialized reponse generation function for heteroscedastic and missing data 
##############################################################################

# Generate different sds such that the average is sd_mean and the ratio of max/min is max_ratio
generate_sigmas <- function(nlevels = 4, max_ratio = 1.5, sd_mean = 1){
  ratios <- c(1, max_ratio, 1 + runif(nlevels - 2)*(max_ratio - 1))
  s <- sd_mean*nlevels/sum(ratios)

  sample(s*ratios, replace=FALSE)
}

simulate_heteroscedastic_response <- function(
  nlevels = c(4,3), within = c(1, 1), n = 1,
  coeffs = c("X1"=0, "X2"=0, "X1:X2"=0),
  family = c("norm","likert"),
  params
) {
  family <- match.arg(family)
  sigma_s <- runif(1, min = params$sigma_s[1], max = params$sigma_s[length(params$sigma_s)])

  # -------------------------------------------------
  # Create design with correct within/between logic
  # -------------------------------------------------
  design <- make_design(nlevels, within, n)

  # -------------------------------------------------
  # Generate the sigma_e to be different across X1's levels
  # -------------------------------------------------
  ids = as.numeric(gsub("[^0-9]", " ", design[,"X1"])) # Indices for the levels of X1
  sigmas_e = generate_sigmas(nlevels[1], params$ratio_sd, params$sigma_e)[ids]

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

  # -------------------------------------------------
  # Mean centering (mu) from GLM mu-functions
  # -------------------------------------------------
  mu <- 0

  # -------------------------------------------------
  # Random subject effects
  # -------------------------------------------------
  subjects <- unique(design$subject)
  s <- rnorm(length(subjects), 0, sigma_s)
  design$s_eff <- s[ match(design$subject, subjects) ]

  # -------------------------------------------------
  # Linear predictor
  # -------------------------------------------------
  eta <- mu + effects + design$s_eff

  # -------------------------------------------------
  # Generate Y
  # -------------------------------------------------
  eta <- eta + sapply(1:length(eta), function(i){ rnorm(1,0, sigmas_e[i]) })
  if(family == "likert") {
    Y <- toOrdinal(eta, levels = params$levels, flexible = params$flexible)
  } else {
    Y <- eta
  }

  # Ensure that categorical variables and the random effect are coded as factors
  data.frame(design, eta = eta, Y = Y, stringsAsFactors = TRUE)
}

# This is to simulate missing data, where percentage (0 - 1) represents the portion of missing data  
removeCells <- function(data, percentage = 0) {
  if(percentage > 0) data[-sample(1:nrow(data), round(percentage*nrow(data))),]
  else data
}

