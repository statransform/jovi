# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment reported in the Case Studies section evaluating ART for binary reponses.


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


# Repeated measures with three factors
test.within.3f <- function(n, design, effects, distr){
	df <- sim.within.3f(n, design, effects) %>% toDistribution(distr)

	model1 <- suppressMessages(lmer(y ~ x1*x2*x3 + (1|s), data=df)) # Parametric (RNK and INT will give same results as PAR)
	model2 <- suppressMessages(art(y ~ x1*x2*x3 + (1|s), data=df)) # ARTool

	vars <- c("x1", "x2", "x1:x2", "x3", "x1:x3", "x2:x3", "x1:x2:x3")
	c(getLmerPValues(model1, vars), getARTPValues(model2, vars))
}


# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, effects, distr) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				test.within.3f(n, design, effects, distr)
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

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~method, ~alpha, ~effects, ~rates,
			n, designStr, distr, "PAR", 0.05, effects, res.05[1:7],
			n, designStr, distr, "ART", 0.05, effects, res.05[8:14],

			n, designStr, distr, "PAR", 0.01, effects, res.01[1:7],		
			n, designStr, distr, "ART", 0.01, effects, res.01[8:14]
		)
	)
}

# I will evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation and (2) ART

# B. 4 x 3 repeated measures
design = c(4,2,2) # Type (4 levels), Complexity (2 levels), Distance (2 levels), 

# C. Bernoulli trials with binary responses (46% success ratio as in Martin et al. (2023) and 6% to test a more challenging case)
distributions <- c("bernoulli46", "bernoulli06")

# D. Various combinations of effects : X1, X2, X1xX2, X3
effects <- matrix(c(0, 0, 0, 0), 
				   ncol = 4, byrow = TRUE)

n <- 22

# 5000 iterations
R <- 5000

filename = "CaseStudy_test-bernoulli"

# Set the seed for reproducibility
set.seed(1234)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
			results <- 
				foreach(distr = distributions, .combine=rbind) %do% {
					foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
						# The test function is parallelized (using multiple cores)
						test(repetitions = R, n = n, design = design, effects = effects[effId,], distr = distr)
					 }
				}
		}
)

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(rates, names_sep = "_")
colnames(res)[6:9]=c("effectX1","effectX2","effectX1X2","effectX3")
colnames(res)[10:16]=c("rateX1","rateX2","rateX1X2","rateX3","rateX1X3","rateX2X3","rateX1X2X3")

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)
