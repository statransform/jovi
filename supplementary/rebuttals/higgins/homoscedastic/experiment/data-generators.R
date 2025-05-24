# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

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


# We control the standard deviation of the generated data so that it is equal
sdlog <- function(mean, sd) {
	sqrt(log(1 + sd^2/mean^2))	
}

meanlog <- function(mean, sd) {
	log(mean^2/sqrt(sd^2 + mean^2))
}

# Here, we ensure that the reponses come from log-normal populations with equal variances
toDistributionLNormHomo <- function(data, means = c(0.5, 1, 1.5, 2), sd = 1.5) {
	data$y <- 0

	data[data$x1 == "A1",]$y <- norm2lnorm(data[data$x1 == "A1",]$dv, meanlog = meanlog(means[1], sd), sdlog = sdlog(means[1], sd))
	data[data$x1 == "A2",]$y <- norm2lnorm(data[data$x1 == "A2",]$dv, meanlog = meanlog(means[2], sd), sdlog = sdlog(means[2], sd))
	data[data$x1 == "A3",]$y <- norm2lnorm(data[data$x1 == "A3",]$dv, meanlog = meanlog(means[3], sd), sdlog = sdlog(means[3], sd))
	data[data$x1 == "A4",]$y <- norm2lnorm(data[data$x1 == "A4",]$dv, meanlog = meanlog(means[4], sd), sdlog = sdlog(means[4], sd))

	data
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

