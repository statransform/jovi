# author: Theophanis Tsandilas
# This code reads a template of a 4x3 repeated-measures design and makes the response variable take values from lognormal ditributions
# where all distributions have equal variances on the scale of the actual observations
# It then evaluates the Type I error rate of ART for the effect on x2 and the interaction effect when there is a strong effect on x1 
# We assume here a between-subjects design.

rm(list=ls())

library(dplyr)
library(tidyr)

library(lmerTest)
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)


# These methods enable us to control the mean and standard deviation of the lognormal distributions on the scale of the actual observations
# See: https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters
sdlog <- function(mean, sd) {
	sqrt(log(1 + sd^2/mean^2))	
}
meanlog <- function(mean, sd) {
	log(mean^2/sqrt(sd^2 + mean^2))
}


# This method takes the template and assigns lognormal values to the response variable y 
# For each of the four levels of x1 we draw samples from a differet lognormal distribution
# mean: vector containing the means of these distributions (on the scale of the actual observations)
# sd: the common standard deviation of the distributions (on the scale of the actual observations)
# Here, we fix the meanslog parameters to specific values, but you can try different combinations: 
# (ART's errors will further increase if you choose a larger sd) 
createRandomSample <- function(df, means = c(0.5, 1, 1.5, 2), sd = 1.5){
	#  I know that x1 has four levels: A1, A2, A3, A4

	ncell <- nrow(df) / 4 # This is the number of observations for each level of x1

	# Let's now draw ncell1 observations for each level but from a different lognormal distribution.  
	# This sampling process does not make any distinction among the different levels of x2
	df[df$x1 == "A1",]$y <- rlnorm(ncell, meanlog = meanlog(means[1], sd), sdlog(means[1], sd))
	df[df$x1 == "A2",]$y <- rlnorm(ncell, meanlog = meanlog(means[2], sd), sdlog(means[2], sd))
	df[df$x1 == "A3",]$y <- rlnorm(ncell, meanlog = meanlog(means[3], sd), sdlog(means[3], sd))
	df[df$x1 == "A4",]$y <- rlnorm(ncell, meanlog = meanlog(means[4], sd), sdlog(means[4], sd))

	df
}

# INT implementation
INT <- function(x){
    qnorm((rank(x) - 0.5)/length(x))
}



# Analysis with PAR, ART, RNK, and INT
analyze <- function(df) {
	m.par <- suppressMessages(aov(y ~ x1*x2, data=df)) # Parametric
	m.art <- suppressMessages(art(y ~ x1*x2, data=df)) # ARTool
	m.rnk <- suppressMessages(aov(rank(y) ~ x1*x2, data=df)) # RNK
	m.int <- suppressMessages(aov(INT(y) ~ x1*x2, data=df)) # INT

	vars <- c("x1", "x2", "x1:x2")
	c(# Return the p-values for the three effects 
		suppressMessages(anova(m.par)[vars, 5]), 
		suppressMessages(anova(m.art)[vars, 7]),
		suppressMessages(anova(m.rnk)[vars, 5]), 
		suppressMessages(anova(m.int)[vars, 5])
	) 
}


# Performs repetitive tests and assess Type I error rates 
test <- function(template, repetitions = 3000) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				analyze(createRandomSample(template)) # Analyze a random sample
			}, 
			error = function(cond) { }, 
			finally = { }
		)
	}

	# From p-values to Type I error rates (false positives)
	res.05 <- round(colMeans(results<.05, na.rm = TRUE), digits = 4)

	# Split the results into separate rows 
	return(tribble(~method, ~rates,
			"PAR", res.05[1:3], 
			"ART", res.05[4:6],
			"RNK", res.05[7:9],
			"INT", res.05[10:12]
		)
	)
}


#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

# Read the data template and fix the type of the factors, as required by the ARTool
template <- read.csv("template-20.csv", sep=",", header=TRUE, strip.white=TRUE)
template$s = factor(template$s)
template$x1 = factor(template$x1)
template$x2 = factor(template$x2)

# Call the simulation
results <- test(template, repetitions = 3000)

# Format the results
res <- results %>% unnest_wider(rates, names_sep = "_")
colnames(res)[1:4]=c("method", "rateX1","rateX2","rateX1X2")

# Store the results
csvfile <- paste("log/results-independent-20-x1", format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

