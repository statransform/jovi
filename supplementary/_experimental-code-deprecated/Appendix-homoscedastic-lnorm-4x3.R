# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay, 2024
# An experiment to test homoscedastic log-normal distributions

rm(list=ls())
library(lmerTest)

# The ARTool
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions

getLmerPValues <- function(model, vars) {
	return(anova(model)[vars, 6])
}

getARTPValues <- function(model, vars) {
	return(anova(model)[vars, 5]) # For within-subjects
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

# Repeated measures with two factors
test.within.2f <- function(n, design, effects, sd){ 
	df <- sim.within.2f(n, design, effects) %>% toDistributionLNormHomo(sd = sd)

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
	vars <- c("x1", "x2", "x1:x2")
	c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, effects, sd) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				test.within.2f(n, design, effects, sd)
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

	designStr <- paste(design, collapse="x")

	distr <- "lnorm-homo"

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~sd, ~method, ~alpha, ~effects, ~rates,
			n, designStr, distr, sd, "PAR", 0.05, effects, res.05[1:3],		
			n, designStr, distr, sd, "RNK", 0.05, effects, res.05[4:6],
			n, designStr, distr, sd, "ART", 0.05, effects, res.05[7:9],
			n, designStr, distr, sd, "INT", 0.05, effects, res.05[10:12],

			n, designStr, distr, sd, "PAR", 0.01, effects, res.01[1:3],		
			n, designStr, distr, sd, "RNK", 0.01, effects, res.01[4:6],
			n, designStr, distr, sd, "ART", 0.01, effects, res.01[7:9],
			n, designStr, distr, sd, "INT", 0.01, effects, res.01[10:12]
		)
	)
}

# 4 x 3 repeated measures
design = c(4,3)

effects = c(2, 0, 0)

# Common standard deviations of responses
sds <- c(1, 2, 3)

# E. Number of participants: 10, 20, and 30
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "homoscedastic-lognormal"

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(sd = sds, .combine=rbind) %do% {
		foreach(n = Ns, .combine=rbind) %do% {
			# The test function is parallelized (using multiple cores)
			test(repetitions = R, n = n, design = design, effects = effects, sd = sd)
		}
	}
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(rates, names_sep = "_")
colnames(res)[7:9]=c("effectX1","effectX2","effectX1X2")
colnames(res)[10:12]=c("rateX1","rateX2","rateX1X2")


# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

