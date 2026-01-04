# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Several functions used by our eperimental code, mainly for data generation and transformation

# A great library for simulating data for a large range of desings: https://debruine.github.io/faux
library(faux)

library(dplyr)
library(tidyr)


# https://yingji15.github.io/2019-08-17-inverse-normal-transformation/
# https://www.biostars.org/p/80597/
# use rankit version where c = 0.5, based on https://www.researchgate.net/publication/286889581_Impact_of_Rank-Based_Normalizing_Transformations_on_the_Accuracy_of_Test_Scores
inverseNormalTransform = function(x){
	qnorm((rank(x) - 0.5)/length(x))
}

# Transforms a set of values to ordinal based on the given thresholds
# See: https://doi.org/10.1016/j.jesp.2018.08.009
toOrdinal = function(values, thresholds = c(-2,-1, 1,2)) {
	discretizeValue = function(x){
		for(i in 1:length(thresholds)) {
			if(x <= thresholds[i]) return(i)
		}
		return(length(thresholds) + 1)
	}

	sapply(values, discretizeValue)
}


# Transforms a set of values to ordinal based on the given thresholds
# See: https://doi.org/10.1016/j.jesp.2018.08.009
# Here, the thresholds take into account the variance of the data, trying to better cover the full range of values.
toOrdinalDistr = function(values, nlevels = 5, equidistant = TRUE) {
	lim <- 2*sd(values) # I take 2 standard deviations that should contain the largest portion of the data (around 95% of the data values)

	thresholds <- NA
	if(equidistant) {
		thresholds <- seq(from=-lim, to=lim, by=2*lim/nlevels)[c(-1, -(nlevels + 1))]
	} else {
		thresholds <- sort(runif(nlevels - 1))*2*lim-lim
	}

	toOrdinal(values, thresholds)
}


# Additional support for other distributions with not conversion support at faux
# Based on https://github.com/debruine/faux/blob/master/R/distribution_convertors.R

# To Cauchy distribution 
norm2cauchy <- function(x, location = 0, scale = 1, mu = mean(x), sd = stats::sd(x), ...) {
  p <- stats::pnorm(x, mu, sd)
  stats::qcauchy(p, location, scale, ...)
}

norm2lnorm <- function(x, meanlog = 0, sdlog = 1, mu = mean(x), sd = stats::sd(x), ...) {
	 p <- stats::pnorm(x, mu, sd)
 	 stats::qlnorm(p, meanlog, sdlog, ...)
}

toDistribution <- function(data, distr = "norm") {
	data$y <- switch(
		distr,
		"norm" = data$dv, 
		"lnorm" = norm2lnorm(data$dv),
		"exp" = norm2gamma(data$dv, shape = 1, rate = 2), # it's basically an exponential distribution
		"cauchy" = norm2cauchy(data$dv),
		# Those are likert items with equidistant levels
		"likert11" = toOrdinalDistr(data$dv, nlevels = 11),
		"likert7" = toOrdinalDistr(data$dv, nlevels = 7),
		"likert5" = toOrdinalDistr(data$dv, nlevels = 5),
		# Those are likert items with levels at random distances
		"likert11B" = toOrdinalDistr(data$dv, nlevels = 11, equidistant = FALSE),
		"likert7B" = toOrdinalDistr(data$dv, nlevels = 7, equidistant = FALSE),
		"likert5B" = toOrdinalDistr(data$dv, nlevels = 5, equidistant = FALSE),
		# Other distributions
		"binom" = norm2binom(data$dv, size = 10, prob=.1),
		"binom2" = norm2binom(data$dv, size = 10, prob=.4),
		"bernoulli46" = norm2binom(data$dv, size = 1, prob=.46),
		"bernoulli06" = norm2binom(data$dv, size = 1, prob=.06),
		"poisson" = norm2pois(data$dv, lambda = 3),

		# For evaluations in the appendix
		"lnorm-0.2" = norm2lnorm(data$dv, sdlog = 0.2),
		"lnorm-0.4" = norm2lnorm(data$dv, sdlog = 0.4),
		"lnorm-0.6" = norm2lnorm(data$dv, sdlog = 0.6),
		"lnorm-0.8" = norm2lnorm(data$dv, sdlog = 0.8),
		"lnorm-1.0" = norm2lnorm(data$dv),
		"lnorm-1.2" = norm2lnorm(data$dv, sdlog = 1.2),

		"binom-5-05" = norm2binom(data$dv, size = 5, prob=.05),
		"binom-5-10" = norm2binom(data$dv, size = 5, prob=.1),
		"binom-5-20" = norm2binom(data$dv, size = 5, prob=.2),
		"binom-10-05" = norm2binom(data$dv, size = 10, prob=.05),
		"binom-10-10" = norm2binom(data$dv, size = 10, prob=.1),
		"binom-10-20" = norm2binom(data$dv, size = 10, prob=.2)
	)

	data
}

# This is to simulate missing data, where percentage (0 - 1) represents the portion of missing data  
removeCells <- function(data, percentage = 0) {
	data[-sample(1:nrow(data), round(percentage*nrow(data))),]
}

############################################################################################
# Data generation for various experimental designs

# Encodes the 1:nlevels to numerical values
# It ensures a fixed distance between the two extreme levels (equal to 1) and random distances in between
# It also ensures that the sum is zero
categ2ratio <- function(nlevels) { 
	levels <- sort(c(0,1, runif(nlevels-2)))

	return(levels - mean(levels))
}

# Transforms categorical to numverical
toNumber <- function(factorvalues) {
	as.numeric(as.character(factorvalues))
}


# A mixed design with a between-participants and a within-participants factor
sim.mixed.2f <- function(n = 10, nlevels = c(2,3), effects = c(0.3,0,0), sd_0 = 1, sd_error = 1, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_between("s", x1 = paste('A', 1:nlevels[1], sep="")) %>% 
		add_within("s", x2 = paste('B', 1:nlevels[2], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           sigma)

	data$s = factor(data$s)

	return(data)
}


# A within-subjects design with 1 factor
sim.within.1f <- function(n = 10, nlevels = 3, effect = 0.3, sd_0 = 1, sd_error = 1, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels[1], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effect * x1.t + sigma)

	data$s = factor(data$s)
	return(data)
}

# A within-subjects design with 2 factors
# The effects vector contains the two main effects and the interaction effect
sim.within.2f <- function(n = 10, nlevels = c(3,3), effects = c(0.3,0,0), sd_0 = 1, sd_error = 1, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels[1], sep=""), x2 = paste('B', 1:nlevels[2], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           sigma)

	data$s = factor(data$s)
	return(data)
}


# A within-subjects design with 3 factors
# The effects vector contains the three main effects and a single interaction effect (x1*x2)
sim.within.3f <- function(n = 10, nlevels = c(2,3,3), effects = c(0.3,0,0, 0), sd_0 = 1, sd_error = 1, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels[1], sep=""), x2 = paste('B', 1:nlevels[2], sep=""), x3 = paste('C', 1:nlevels[3], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
		mutate(x3.t = toNumber(plyr::mapvalues(x3, from = levels(x3), to = convertFun(nlevels[3])))) %>% 
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           effects[4] * x3.t +
           sigma)

	data$s = factor(data$s)
	data
}


# A between-subjects design with 1 factor
sim.between.1f <- function(n = 10, nlevels = 3, effect = 0.3, sd_0 = 1, sd_error = 1, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_between("s", x1 = paste('A', 1:nlevels, sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effect * x1.t + sigma)

	data$s = factor(data$s)
	return(data)
}


# A betweenn-subjects design with 2 factors
# The effects vector contains the two main effects and the interaction effect
sim.between.2f <- function(n = 10, nlevels = c(3,3), effects = c(0.3,0,0), sd_0 = 1, sd_error = 1, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_between("s", x1 = paste('A', 1:nlevels[1], sep=""), x2 = paste('B', 1:nlevels[2], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           sigma)


	data$s = factor(data$s)
	return(data)
}


##### Unequal variances 
##################################

# This function produces difference variance ratios for each variable level
# where the most extreme will be 1 to maxratio
sd.ratios <- function(nlevels = 4, maxratio = 1.5) {
	ratios <- c(1, maxratio, 1 + runif(nlevels - 2)*(maxratio - 1))
	sample(ratios, replace=FALSE) - mean(ratios) + 1
}

# A mixed design with the variance of the first between-participants factor is not constant. 
# sd.ratio stands for the maximum variance ratio within levels 
sim.mixed.2f.hetero <- function(n = 10, nlevels = c(2,3), effects = c(0.3,0,0), sd_0 = 1, sd_error = 1, sd_ratio = 1.5, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_between("s", x1 = paste('A', 1:nlevels[1], sep="")) %>% 
		add_within("s", x2 = paste('B', 1:nlevels[2], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
		mutate(x1.w = toNumber(plyr::mapvalues(x1, from = levels(x1), to = sd.ratios(nlevels[1], sd_ratio)))) %>% # This is to assign random weights to levels different variances within x1
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           x1.w*sigma)

	data$s = factor(data$s)

	return(data)
}


# A between-subjects design with 1 factor
sim.between.1f.hetero <- function(n = 10, nlevels = 3, effect = 0.3, sd_0 = 1, sd_error = 1, sd_ratio = 1.5, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_between("s", x1 = paste('A', 1:nlevels, sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		mutate(x1.w = toNumber(plyr::mapvalues(x1, from = levels(x1), to = sd.ratios(nlevels, sd_ratio)))) %>% # This is to assign random weights to levels different variances within x1
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept + effect * x1.t + x1.w * sigma)

	data$s = factor(data$s)
	return(data)
}


sim.between.2f.hetero <- function(n = 10, nlevels = c(2,3), effects = c(0.3,0,0), sd_0 = 1, sd_error = 1,  sd_ratio = 1.5, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_between("s", x1 = paste('A', 1:nlevels[1], sep=""), x2 = paste('B', 1:nlevels[2], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
				mutate(x1.w = toNumber(plyr::mapvalues(x1, from = levels(x1), to = sd.ratios(nlevels[1], sd_ratio)))) %>% # This is to assign random weights to levels different variances within x1
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           x1.w*sigma)

	data$s = factor(data$s)

	return(data)
}


# A within-subjects design with 1 factor
sim.within.1f.hetero <- function(n = 10, nlevels = 3, effect = 0.3, sd_0 = 1, sd_error = 1, sd_ratio = 1.5, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels[1], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		mutate(x1.w = toNumber(plyr::mapvalues(x1, from = levels(x1), to = sd.ratios(nlevels, sd_ratio)))) %>% # This is to assign random weights to levels different variances within x1
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effect * x1.t + x1.w*sigma)

	data$s = factor(data$s)
	return(data)
}


# A within-subjects design with 2 factors
# The effects vector contains the two main effects and the interaction effect
sim.within.2f.hetero <- function(n = 10, nlevels = c(3,3), effects = c(0.3,0,0), sd_0 = 1, sd_error = 1, sd_ratio = 1.5, convertFun = categ2ratio) {
	data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels[1], sep=""), x2 = paste('B', 1:nlevels[2], sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels[1])))) %>%  # Decode the categorical values to numeric ones
		mutate(x2.t = toNumber(plyr::mapvalues(x2, from = levels(x2), to = convertFun(nlevels[2])))) %>% 
						mutate(x1.w = toNumber(plyr::mapvalues(x1, from = levels(x1), to = sd.ratios(nlevels[1], sd_ratio)))) %>% # This is to assign random weights to levels different variances within x1
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept +
           effects[1] * x1.t + 
           effects[2] * x2.t + 
           effects[3] * x1.t * x2.t +
           x1.w*sigma)

	data$s = factor(data$s)
	return(data)
}

