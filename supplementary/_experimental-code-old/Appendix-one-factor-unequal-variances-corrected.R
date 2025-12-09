# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the positives rate of PAR, ART, RNK, and nonparametric tests:
# For within-subjects designs, we use the Wilcoxon sign-rank test if the factor has two levels (2 within) 
# and the Friedman test if the factor has three (3 within) or four (4 within) levels. 
# For between-subjects designs (2 between, 3 between, and 4 between), we use the Kruskal–Wallis test.
# Under certain assumptions (i.e., when the null hypothesis is that means over the latent variable are equal), 
# the positives rates can be interpreted as Type I error rates.

# See discussion in Appendix (Section 8). Here, we evaluate the methods when using corrections for unequal variances.

rm(list=ls())
# Parallel computation
library(foreach)
library(doParallel)

library("POSSA") # to get access to the get_p function

source("data-generators.R") # Import the data-generation functions

library(ez) #Library for ezANOVA

getAOVPvalues <- function(model, vars, paired = TRUE) {
	if(!paired) return(anova(model)[vars, 5])
	else return(unname(unlist(get_p(model))))
}

getEzPvalues <- function(model, vars) { # If sphericity is violated (p < .05) apply a Greenhouse–Geisser correction 
	if(model$`Mauchly's Test for Sphericity`["p"] <0.05) return(model$`Sphericity Corrections`[["p[GG]"]])
	else return (model$ANOVA[["p"]])
}

rank.test <- function(nlevels, df, paired = TRUE) {
	# Do either the wilcoxon or the friedman (more than two levels)
	if(paired) return (ifelse(nlevels==2, wilcox.test(y~x1, df, paired=TRUE)$p.value, friedman.test(y~x1|s, df)$p.value)) 
	else return (kruskal.test(y~x1, df)$p.value) 
}

suppress <- function(call){
	suppressWarnings(suppressMessages(call))
}

# Repeated measures with two factors
test.within.1f <- function(n, nlevels, effect, sd_ratio, distr){ 
	df <- sim.within.1f.hetero(n, nlevels, effect, sd_ratio = sd_ratio) %>% toDistribution(distr)

#	model0 <- suppress(aov(y ~ x1 + Error(s), data=df)) # Parametric
#	model1 <- suppress(aov(rank(y) ~ x1 + Error(s), data=df)) # RNK

	df$rnk <- rank(df$y)
	df$int <- inverseNormalTransform(df$y)

	model0 <- ezANOVA(data = df, dv=.(y), wid=.(s), within=.(x1))
	model1 <- ezANOVA(data = df, dv=.(rnk), wid=.(s), within=.(x1))
	model2 <- ezANOVA(data = df, dv=.(int), wid=.(s), within=.(x1))

	vars <- c("x1")
	c(getEzPvalues(model0, vars), getEzPvalues(model1, vars), getEzPvalues(model2, vars), suppress(rank.test(nlevels, df, paired = TRUE)))
}

# Between subjects with two factors
test.between.1f <- function(n, nlevels, effect, sd_ratio, distr){ 
	df <- sim.between.1f.hetero(nlevels*n, nlevels, effect, sd_ratio = sd_ratio) %>% toDistribution(distr)  # Ensure n participants per cell

	df$rnk <- rank(df$y)
	df$int <- inverseNormalTransform(df$y)

	model0 <- oneway.test(y ~ x1, data = df, var.equal = FALSE)
	model1 <- oneway.test(rnk ~ x1, data = df, var.equal = FALSE)
	model2 <- oneway.test(int ~ x1, data = df, var.equal = FALSE)

	vars <- c("x1")
	c(model0$p.value, model1$p.value, model2$p.value, suppress(rank.test(nlevels, df, paired = FALSE)))
}

# Choose the data generation function for each design
testFun <- function(id) {switch(id, test.within.1f, test.between.1f)} 

# Performs repetitive tests for the given configuration
test <- function(repetitions, n, designId, nlevels, effect, ratio, distr) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				testFun(designId)(n, nlevels, effect, ratio, distr)
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

	designStr <- paste(ifelse(designId == 1, "within", "between"), "-", nlevels, sep="")

	# Split the results into separate rows 
	return(tribble(~n, ~design, ~distr, ~method, ~alpha, ~sd_ratio, ~effect, ~rates,
			n, designStr, distr, "PAR", 0.05, ratio, effect, res.05[1],		
			n, designStr, distr, "RNK", 0.05, ratio, effect, res.05[2],
			n, designStr, distr, "INT", 0.05, ratio, effect, res.05[3],
			n, designStr, distr, "NON", 0.05, ratio, effect, res.05[4],

			n, designStr, distr, "PAR", 0.01, ratio, effect, res.01[1],		
			n, designStr, distr, "RNK", 0.01, ratio, effect, res.01[2],
			n, designStr, distr, "INT", 0.01, ratio, effect, res.01[3],
			n, designStr, distr, "NON", 0.01, ratio, effect, res.01[4]
		)
	)
}

# Evaluate a factor with 2, 3, and 4 levels
nlevelsList <- c(4)
designs <- c("within", "between")

# Distributions to evaluate
distributions <- c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")

effect <- 0 # Test Type I error rates, so the effect size is set to zero
sd_ratios <- c(1, 1.5, 2, 2.5, 3) # SD ratios (unequal variances)

# Cell sizes (n in the paper) 
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Appendix_test-One-Factor-Unequal-Var-Corrected"

# Set the seed for reproducibility
set.seed(2233)

#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

time <- system.time({ 
	results <- foreach(desId = 1:1, .combine=rbind) %do% {
#	results <- foreach(desId = 1:length(designs), .combine=rbind) %do% {
		foreach(nlevels = nlevelsList, .combine=rbind) %do% {
			foreach(distr = distributions, .combine=rbind) %do% {
				foreach(n = Ns, .combine=rbind) %do% {
					foreach(ratio = sd_ratios, .combine=rbind) %do% {
						# The test function is parallelized (using multiple cores)
						test(repetitions = R, n = n, designId = desId, nlevels = nlevels, effect = effect, ratio = ratio, distr = distr)
					}
				}
			}
		}
	}
})


# Store the results
csvfile <- paste("logs/", filename, format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(results, file = csvfile, row.names=FALSE, quote=F)

