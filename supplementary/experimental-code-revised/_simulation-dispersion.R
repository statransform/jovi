set.seed(123)

# OD values
OD_values <- c(1.1, 1.2, 1.5, 2, 5)

# Preallocate
res <- data.frame(OD = OD_values,
                  sigma_normal = NA,
                  sigma_lognormal = NA,
                  sigma_poisson = NA,
                  sigma_exponential = NA,
                  sigma_binomial = NA,
                  sigma_cauchy = NA)

lambda <- 3
sigma_e <- 1
p_target <- 0.1
n_bin <- 20
gamma <- 1

for(i in seq_along(OD_values)) {
  OD <- OD_values[i]
  # Normal
  res$sigma_normal[i] <- sqrt((OD-1)*sigma_e^2)
  # Log-normal
  res$sigma_lognormal[i] <- sqrt(log(OD + 1))
  # Poisson
  res$sigma_poisson[i] <- if(OD <=1) 0 else sqrt(log(1 + (OD-1)/lambda))
  # Exponential
  res$sigma_exponential[i] <- if(OD <=1) 0 else sqrt(log(OD))
  # Binomial (numerical)
  f_bin <- function(s) {
    p <- plogis(qlogis(p_target) + rnorm(1e5,0,s))
    var(p)/(p_target*(1-p_target)) - OD
  }
  # check if achievable
  tryCatch({
    res$sigma_binomial[i] <- uniroot(f_bin, c(0,5))$root
  }, error=function(e) { res$sigma_binomial[i] <- NA })
  # Cauchy (rough)
  res$sigma_cauchy[i] <- sqrt(OD - 1)
}

res
