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

# Repeated measures with two factors
test.within.2f <- function(n, design, sd){ 
	df <- sim.within.2f(n, design, effects = c(2,0,0)) %>% toDistributionLNormHomo(sd = sd)

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
	vars <- c("x1", "x2", "x1:x2")
	c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}


repeating <- function(n=20, design = c(4,3), sd) {
	keep <- TRUE

	counter <- 0
	df <- NA
	while(keep) {
		df <- sim.within.2f(n, design, effects = c(2,0,0)) %>% toDistributionLNormHomo(sd = sd)

		model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
		model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
		model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
		model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
		vars <- c("x1", "x2", "x1:x2")
		pvalues <- c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))

		if(counter > 1000  | (pvalues[8] < .001)) keep = FALSE
	}

	df
}


# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, sd) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				test.within.2f(n, design, sd)
			}, 
			error = function(cond) {
				# do nothing
			}, finally = {
				# do nothing
			}
		)
	}

	# From p-values to positive rates that represent (depending on whether there is a true effect) either Type I error rates (false positives) or power (true positives) 
	res.05 <- round(colSums(results<0.05)/nrow(results), digits = 4) # alpha = .05
	res.01 <- round(colSums(results<0.01)/nrow(results), digits = 4) # alpha = .01

	designStr <- paste(design, collapse="x")

	distr <- "lnorm-homo"
	effects <- c(1,0,0)

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

# Common standard deviations of responses
sds <- c(1, 2, 3)

# E. Number of participants: 10, 20, and 30
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "homoscedastic-lognormal"

# Set the seed for reproducibility
set.seed(1234)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(sd = sds, .combine=rbind) %do% {
		foreach(n = Ns, .combine=rbind) %do% {
			# The test function is parallelized (using multiple cores)
			test(repetitions = R, n = n, design = design, sd = sd)
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

