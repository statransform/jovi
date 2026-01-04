# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Comparison of Power of RNK and INT methods against the multifactorial generalizations of the van der Waerden test 
# and the Kruskal-Wallis and Friedman tests
# For this experiment, we evaluate how the effect on a factor affects the Power to detect effects on a different factor

# See: http://www.uni-koeln.de/~luepsen/R/
# See: http://www.uni-koeln.de/~luepsen/R/manual.pdf

# See Appendix: Fig. 36, Fig. 38, Fig. 39


rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)

library("POSSA") # to get access to the get_p function

source("np.anova.R") # From http://www.uni-koeln.de/~luepsen/R/

source("data-generators.R") # Import the data-generation functions

getAOVPalues <- function(model, vars, isbetween = FALSE) {
	if(isbetween) return(anova(model)[vars, 5])
	else return(unname(unlist(get_p(model))))
}

getPValues <- function(model, vars) {
	return(model[vars, 4])
}

# Notice that I include slope random effects in my models, which is required by the np.anova functions  
test.within.2f = function(n, design, effects, distr){
	df = sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model1 = aov(rank(y) ~ x1*x2 + Error(s/(x1*x2)), data=df) # RNK
	model2 = aov(inverseNormalTransform(y) ~ x1*x2 + Error(s/(x1*x2)), data=df) # INT

    model3 = np.anova(y ~ x1*x2 + Error(s/(x1*x2)), data = as.data.frame(df), method=1) # generalized van der Waerden tests
	model4 = np.anova(y ~ x1*x2 + Error(s/(x1*x2)), data = as.data.frame(df), method=0) # generalized Kruskal-Wallis-Friedman tests

	vars <- c("x1 ", "x2", "x1:x2")
	c(getAOVPalues(model1, vars), getAOVPalues(model2, vars), getPValues(model3, vars),getPValues(model4, vars))
}

test.between.2f = function(n, design, effects, distr){
	df = sim.between.2f(prod(design)*n, design, effects) %>% toDistribution(distr)

	model1 = aov(rank(y) ~ x1*x2, data=df) # rank
	model2 = aov(inverseNormalTransform(y) ~ x1*x2, data=df) # INT

	model3 = np.anova(y ~ x1*x2, data = as.data.frame(df), method=1) # generalized van der Waerden tests
	model4 = np.anova(y ~ x1*x2, data = as.data.frame(df), method=0) # generalized Kruskal-Wallis-Friedman tests

	vars <- c("x1", "x2", "x1:x2")
	c(getAOVPalues(model1, vars, isbetween = TRUE), getAOVPalues(model2, vars, isbetween = TRUE), getPValues(model3, vars),getPValues(model4, vars))
}

test.mixed.2f = function(n, design, effects, distr){
	df = sim.mixed.2f(design[1]*n, design, effects) %>% toDistribution(distr)

	model1 = aov(rank(y) ~ x1*x2 + Error(s/x2), data=df) # RNK
	model2 = aov(inverseNormalTransform(y) ~ x1*x2 + Error(s/x2), data=df) # INT

    model3 = np.anova(y ~ x1*x2 + Error(s/x2), data = as.data.frame(df), method=1) # generalized van der Waerden tests
	model4 = np.anova(y ~ x1*x2 + Error(s/x2), data = as.data.frame(df), method=0) # generalized Kruskal-Wallis-Friedman tests

	vars <- c("x1 ", "x2", "x1:x2")
	c(getAOVPalues(model1, vars), getAOVPalues(model2, vars), getPValues(model3, vars),getPValues(model4, vars))
}

testFun <- function(id) {switch(id,test.between.2f, test.mixed.2f, test.within.2f)} 

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, designId, design, effects, distr) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				testFun(designId)(n, design, effects, distr)
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
			n, designStr, distr, "RNK", 0.05, effects, res.05[1:3],		
			n, designStr, distr, "INT", 0.05, effects, res.05[4:6],
			n, designStr, distr, "VDW", 0.05, effects, res.05[7:9],
			n, designStr, distr, "KWF", 0.05, effects, res.05[10:12],

			n, designStr, distr, "PAR", 0.01, effects, res.01[1:3],		
			n, designStr, distr, "RNK", 0.01, effects, res.01[4:6],
			n, designStr, distr, "VDW", 0.01, effects, res.01[7:9],
			n, designStr, distr, "KWF", 0.01, effects, res.01[10:12]
		)
	)
}


# 2 x 3 between-subjects, 2 x 4 mixed-subjects, and 4 x 3 within-subjects designs
designs <- list(c(2,3), c(2,4), c(4,3))

# Distributions to evaluate
distributions <- c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")

# Various combinations of effects
effects <- matrix(c(0.8, 0, 0,
				    0.8, 1, 0,
				    0.8, 2, 0,
				    0.8, 4, 0,
				    0.8, 8, 0,
					
					0, 0, 1.5,
				    0, 1, 1.5,
				    0, 2, 1.5,
				    0, 4, 1.5,
				    0, 8, 1.5
				    ), 
				   ncol = 3, byrow = TRUE)


# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Appendix_test-vdWaerden-Power-Multieffect"

# Set the seed for reproducibility
set.seed(2233)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

options (contrasts=c("contr.sum","contr.poly")) # See http://www.uni-koeln.de/~luepsen/R/manual.pdf

time <- system.time({ 
	results <- foreach(desId = 1:length(designs), .combine=rbind) %do% {
		foreach(distr = distributions, .combine=rbind) %do% {
			foreach(n = Ns, .combine=rbind) %do% {
				foreach(effId = 1:nrow(effects), .combine=rbind) %do% {
					# The test function is parallelized (using multiple cores)
					test(repetitions = R, n = n, designId = desId, design = designs[[desId]], effects = effects[effId,], distr = distr)
				}
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

