# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the effect size estimates of PAR, ART, RNK, and INT.
# See Fig. 28, Fig. 29

# You can edit the experimental parameters in the code to test different conditions.


rm(list=ls())
library(lmerTest)
library(effectsize) # Effect size functions

# The ARTool
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions


eta_values = function(model) {
	return(eta_squared(anova(model))[, 2])
}

getLmerPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 6, 5)])) # For aov models the table is different
}

getARTPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 5, 7)])) # For aov models the ART's table is different
}


# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model = suppressMessages(lmer(dv ~ x1*x2 + (1|s), data=df)) # This is the reference groundtruth model

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

#	c(effects, eta_values(model), eta_values(model0), eta_values(model1), eta_values(model2), eta_values(model3))

	vars <- c("x1", "x2", "x1:x2")
	c(effects, getLmerPValues(model, vars), getLmerPValues(model0, vars), getLmerPValues(model1, vars), getARTPValues(model2, vars), getLmerPValues(model3, vars))
}

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, distr, maxeffects) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
	#	effects = c(runif(1, min=-maxeffects[1], max=maxeffects[1]), maxeffects[2], maxeffects[3])
	effects = c(maxeffects[1], maxeffects[2], runif(1, min=-maxeffects[3], max=maxeffects[3]))

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
	
	designStr <- paste(design, collapse="x")

	# Split the results into separate rows 
	return(tibble(n=n, design=designStr, distr=distr, pvalues=results))
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT. I test all three on the same datasets. 

# B.  2 x 3 between, 2 x 4 mixed design, and 4 x 3 repeated measures
designs <- list(c(4,3))

# C. Distributions
#distributions <- c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")
distributions <- c("lnorm")

# D. Specify the range of effects for the two main effects and the interaction effect
maxeffects <- c(4,0,2) # This is for Fig. 28
#maxeffects <- c(8,8,0) # This is for Fig. 29

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(12) 

# correlations over 100 data points
R <- 100

filename = "p-values-12-interaction" # For maximum effects between -2 and 2 
#filename = "6_scatterplot_effect_sizes-max-8" # For maximum effects between -8 and 8 

# Set the seed for reproducibility
set.seed(2233)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(desId = 1:length(designs), .combine=rbind) %do% {
		foreach(distr = distributions, .combine=rbind) %do% {
			foreach(n = Ns, .combine=rbind) %do% {
					# The test function is parallelized (using multiple cores)
				test(repetitions = R, n = n, design = designs[[desId]], distr = distr, maxeffects = maxeffects)
			}
		}
	}
})


# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% mutate(pvalues = as.data.frame(pvalues)) %>% unnest_wider(pvalues)

colnames(res)[4:6]=c("effectX1","effectX2","effectX1X2")

colnames(res)[7:9]=c("pX1","pX2","pX1X2")

colnames(res)[10:12]=c("parX1","parX2","parX1X2")
colnames(res)[13:15]=c("rnkX1","rnkX2","rnkX1X2")
colnames(res)[16:18]=c("artX1","artX2","artX1X2")
colnames(res)[19:21]=c("intX1","intX2","intX1X2")

# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)
