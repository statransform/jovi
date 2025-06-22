# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on ordinal scales for a 4x3 within-subjects design 
# See Fig. 14, Fig. 15, Fig. 16

# By default, this code will generate a data file for n = 20 with 5000 iteration. 
# You can edit the experimental parameters in the code to test different conditions.

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
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT
	vars <- c("x1", "x2", "x1:x2")
	c(getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, effects, distr) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				test.within.2f(n, design, effects, distr)
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

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~method, ~alpha, ~effects, ~rates,
			n, designStr, distr, "PAR", 0.05, effects, res.05[1:3],		
			n, designStr, distr, "RNK", 0.05, effects, res.05[4:6],
			n, designStr, distr, "ART", 0.05, effects, res.05[7:9],
			n, designStr, distr, "INT", 0.05, effects, res.05[10:12],

			n, designStr, distr, "PAR", 0.01, effects, res.01[1:3],		
			n, designStr, distr, "RNK", 0.01, effects, res.01[4:6],
			n, designStr, distr, "ART", 0.01, effects, res.01[7:9],
			n, designStr, distr, "INT", 0.01, effects, res.01[10:12]
		)
	)
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT. I test all three on the same datasets. 

# B. 4 x 3 repeated measures
design = c(4,3)

# C. Ordinal scales: Likert-type items with 5, 7, and 11 levels
# Thresholds can be either equidistant (likert5, likert7, likert11) or flexible (likert5B, likert7B, likert11B)  
distributions <- c("likert5", "likert7", "likert11", "likert5B", "likert7B", "likert11B")

# D. Various combinations of effects
effects <- matrix(c(0, 0, 0), 
				   ncol = 3, byrow = TRUE)

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(32, 64, 128, 256, 512) 

# 5000 iterations
R <- 5000

filename = "2_test_4x3_Ordinal_sample_size"

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
		results <- foreach(distr = distributions, .combine=rbind) %do% {
			foreach(n = Ns, .combine=rbind) %do% {
				foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
					# The test function is parallelized (using multiple cores)
					test(repetitions = R, n = n, design = design, effects = effects[effId,], distr = distr)
				}
			}
		}
})

# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% unnest_wider(effects, names_sep = "_") %>% unnest_wider(rates, names_sep = "_")
colnames(res)[6:8]=c("effectX1","effectX2","effectX1X2")
colnames(res)[9:11]=c("rateX1","rateX2","rateX1X2")


# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

