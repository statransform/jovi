# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, ART, RNK, and INT on ratio scales (distributions below) for a 4x3 within-subjects design 
# For this experiment, we evaluate contrasts: See Fig. 10

# By default, this code will generate a data file for n = 20 with 5000 iteration. 
# You can edit the experimental parameters in the code to test different conditions.

rm(list=ls())
library(lmerTest)
library("emmeans")

# The ARTool
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions


# Tests for contrasts 
getLmerPValues <- function(model, expr) {
	as.data.frame(suppressMessages(contrast(emmeans(model, expr), method="pairwise", interaction=TRUE)))$p.value
}

getARTPValues <- function(model, expr) {
	as.data.frame(suppressMessages(art.con(model, expr, interaction=TRUE)))$p.value
}


# The number of contrasts as a function of n
contrastsNum <- function(n) {
	n*(n-1)/2	
} 

# Groups the results by factor 
# modN is the number of models that I compare
groupByFactor <- function(result, design = c(4,3), modN = 4) {
	nums <- contrastsNum(design)
	coN <- rep(c(nums, prod(nums)), modN) # I will also group the contrasts that correspond to interaction between every two factors

	# Then, find the start position of each factor in the results
	pos <- cumsum(coN) - coN + 1

	return(mapply(function(start,len){mean(result[start:(start+(len-1))])}, pos, coN))
} 


# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model0 <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	model1 <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	model2 <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	model3 <- suppressMessages(lmer(inverseNormalTransform(y) ~ x1*x2 + (1|s), data=df)) # INT

	c(getLmerPValues(model0, ~x1), getLmerPValues(model0, ~x2), getLmerPValues(model0, ~x1*x2),  
	  getLmerPValues(model1, ~x1), getLmerPValues(model1, ~x2), getLmerPValues(model1, ~x1*x2), 
	  getARTPValues(model2, ~x1), getARTPValues(model2, ~x2), getARTPValues(model2, ~x1*x2),
	  getLmerPValues(model3, ~x1), getLmerPValues(model3, ~x2), getLmerPValues(model3, ~x1*x2)
	)
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
	res <- colSums(results<0.05)/nrow(results) # alpha = .05
	res.05 <- round(groupByFactor(res, design), digits = 4) 

	res <- colSums(results<0.01)/nrow(results) # alpha = .01
	res.01 <- round(groupByFactor(res, design), digits = 4) 

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
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT.

# B. 4 x 3 repeated measures
design = c(4,3)

# C. Continuous distributions (equal variance, full) and discrete distribution: binom (size = 10, prob = 0.1) and Poisson
distributions <- c("norm", "lnorm", "exp", "cauchy", "binom", "poisson")

# D. Various combinations of effects
effects <- matrix(c(0, 0, 0,
				    0.5, 0.5, 0,
				    1, 1, 0,
				   	2, 2, 0,
				    4, 4, 0,
				    8, 8, 0,
				    0.5, 0, 0,
				    1, 0, 0,
				    2, 0, 0,
				    4, 0, 0,
				    8, 0, 0), 
				   ncol = 3, byrow = TRUE)

# E. Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "1_test_4x3_Contrasts"

# Set the seed for reproducibility
set.seed(1234)

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


