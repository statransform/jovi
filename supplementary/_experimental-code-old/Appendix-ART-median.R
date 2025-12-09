# Author: Theophanis Tsandilas, 2023-2024
# Inria & Université Paris-Saclay

# Experiment evaluating the Type I error rates of PAR, INT, ART, and ART with median alignement for a 4x3 within-subjects design 
# See Appendix: Fig. 20, Fig. 21, Fig. 22

# By default, this code will generate a data file for n = 20 with 5000 iteration.
# You can edit the experimental parameters in the code to test different conditions.


rm(list=ls())

# Parallel computation
library(foreach)
library(doParallel)

source("data-generators.R") # Import the data-generation functions

getPValues <- function(model) {
  return(summary(model)[[2]][[1]][1:3,5])
}

# A simple implementation of ART for repeated measures: y ~ x1*x2 + (1|s)
# fun: it can be median or mean
art <- function(data, fun = median) {
  df <- data

  Ai <- aggregate(y ~ x1, data = df, fun)
  Bj <- aggregate(y ~ x2, data = df, fun) 
  AiBj <- aggregate(y ~ x1+x2, data = df, fun) 
  cmean <- aggregate(y ~ x1+x2, data = df, fun) 

  df <- df %>% group_by(x1) %>% mutate(Ai = fun(y)) %>% 
    group_by(x2) %>% mutate(Bj = fun(y)) %>% 
    group_by(x1,x2) %>% mutate(AiBj = fun(y)) %>% mutate(Cell = fun(y))

  df$m <- fun(df$y)

  df$artA <- df$y - df$Cell + df$Ai - df$m
  df$artB <- df$y - df$Cell + df$Bj - df$m
  df$artAB <- df$y - df$Cell + df$AiBj - df$Ai - df$Bj + df$m 

  df$rnkA <- rank(round(df$artA, digits = 7))
  df$rnkB <- rank(round(df$artB, digits = 7))
  df$rnkAB <- rank(round(df$artAB, digits = 7))

  modelA <- suppressMessages(aov(rnkA ~ x1*x2 + Error(s), data=df))
  modelB <- suppressMessages(aov(rnkB ~ x1*x2 + Error(s), data=df))
  modelAB <- suppressMessages(aov(rnkAB ~ x1*x2 + Error(s), data=df))

  c(getPValues(modelA)[1], getPValues(modelB)[2], getPValues(modelAB)[3])
}

# Repeated measures with two factors
test.within.2f <- function(n, design, effects, distr){ 
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)

	model0 <- suppressMessages(aov(y ~ x1*x2 + Error(s), data=df)) # Parametric
	artmean <- suppressMessages(art(df, fun = mean)) # ART mean
	artmedian <- suppressMessages(art(df, fun = median)) # ART median
	model3 <- suppressMessages(aov(inverseNormalTransform(y) ~ x1*x2 + Error(s), data=df)) # INT

	c(getPValues(model0), artmedian, artmean, getPValues(model3))
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
			n, designStr, distr, "ART-MED", 0.05, effects, res.05[4:6],
			n, designStr, distr, "ART", 0.05, effects, res.05[7:9],
			n, designStr, distr, "INT", 0.05, effects, res.05[10:12],

			n, designStr, distr, "PAR", 0.01, effects, res.01[1:3],		
			n, designStr, distr, "ART-MED", 0.01, effects, res.01[4:6],
			n, designStr, distr, "ART", 0.01, effects, res.01[7:9],
			n, designStr, distr, "INT", 0.01, effects, res.01[10:12]
		)
	)
}

# Evaluate the following for main effects and interactions:
# A. Methods: (1) ANOVA/LMER with no transformation, (2) Rank, (3) ART, (4) INT. I test all three on the same datasets. 

# B. 4 x 3 repeated measures
design = c(4,3)

# C. Distributions to evaluate
distributions <- c("lnorm", "exp", "cauchy", "binom", "poisson", "likert5B")

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


# Cell sizes (n in the paper) -- for within-subject designs, it's also the number of subjects
#Ns <- c(10, 20, 30) 
Ns <- c(20) 

# 5000 iterations
R <- 5000

filename = "Appendix_test_4x3_ART-Median"

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

