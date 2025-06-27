# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the effect size estimates of PAR, ART, RNK, and INT.
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
	return(round(eta_squared(anova(model))[, 2], digits=4))
}

# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model = suppressMessages(lmer(dv ~ x1*x2 + (1|s), data=df)) # This is the reference groundtruth model

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

	c(effects, eta_values(model), eta_values(model0), eta_values(model1), eta_values(model2), eta_values(model3))
}

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, design, distr, effects) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
	#	effects = c(runif(1, min=-maxeffects[1], max=maxeffects[1]), maxeffects[2], maxeffects[3])
	effects = c(effects[1], effects[2], effects[3])

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
	return(tibble(n=n, design=designStr, distr=distr, eta_squared=results))
}


# 4 x 3 repeated measures
design <- c(4,3)

#distr <- "norm"
distr <- "likert5B"

effects <- matrix(c(0, 0, 0,
				    1, 1, 0,
				    2, 2, 0,
				   	4, 4, 0,
				    8, 8, 0), 
				   ncol = 3, byrow = TRUE)

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
Ns <- c(20) 

# Number of iterations for each effect size
R <- 300

filename = "effects-interactions-ordinal" 

# Set the seed for reproducibility
set.seed(2233)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
		foreach(n = Ns, .combine=rbind) %do% {
				# The test function is parallelized (using multiple cores)
			test(repetitions = R, n = n, design = design, distr = distr, effects = effects[effId,])
		}
	}
})


# split the effects and rates columns (currently vectors) into individual columns
res <- results %>% mutate(eta_squared = as.data.frame(eta_squared)) %>% unnest_wider(eta_squared)

colnames(res)[4:6]=c("effectX1","effectX2","effectX1X2")

colnames(res)[7:9]=c("X1","X2","X1X2")

colnames(res)[10:12]=c("parX1","parX2","parX1X2")
colnames(res)[13:15]=c("rnkX1","rnkX2","rnkX1X2")
colnames(res)[16:18]=c("artX1","artX2","artX1X2")
colnames(res)[19:21]=c("intX1","intX2","intX1X2")

# Store the results
options(scipen = 999)
csvfile <- paste("log/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)
