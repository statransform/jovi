# author: Theophanis Tsandilas
# This code reads a template of a 4x3 repeated-measures design and makes the response variable take random values from a binomial distribution 
# It then evaluates the Type I error rate of ART

rm(list=ls())

library(dplyr)
library(tidyr)

library(lmerTest)
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)


# This method takes the template and randomly assigns values drawn from a binomial distribution to the response variable y 
# The parameter prob determines the probability of error (or succes), and size the number of Bernoulli trials
# shape1 is a parameter for the beta distribution for individual participant differences
createRandomSample <- function(df, size = 10, prob = 0.05, shape1 = 2){
	# I add subject-level effects. For each subject, I randomly pick a different probabily drawn from a beta distribution with shape1 = 2
	# See: https://en.wikipedia.org/wiki/Beta_distribution
	# I derive the second parameter of the beta distribution from the desired mean probability
	shape2 = shape1/prob - shape1
	df <- df %>% group_by(s) %>% mutate(probs = rbeta(1, shape1 = 1, shape2))

	# from that, generate responses from a binomial distribution
	df$y <- rbinom(nrow(df), size, df$probs)
	df
}

# INT implementation
INT <- function(x){
    qnorm((rank(x) - 0.5)/length(x))
}


# Analysis with two methods: PAR and ART
analyze <- function(df) {
	m.par <- suppressMessages(lmer(y ~ x1*x2 + (1|s), data=df)) # Parametric
	m.art <- suppressMessages(art(y ~ x1*x2 + (1|s), data=df)) # ARTool
	m.rnk <- suppressMessages(lmer(rank(y) ~ x1*x2 + (1|s), data=df)) # RNK
	m.int <- suppressMessages(lmer(INT(y) ~ x1*x2 + (1|s), data=df)) # INT

	vars <- c("x1", "x2", "x1:x2")
	c(# Return the p-values for the three effects 
		suppressMessages(anova(m.par)[vars, 6]), 
		suppressMessages(anova(m.art)[vars, 5]),
		suppressMessages(anova(m.rnk)[vars, 6]), 
		suppressMessages(anova(m.int)[vars, 6])
	) 
}


# Performs repetitive tests and assess Type I error rates 
test <- function(template, repetitions = 3000) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				analyze(createRandomSample(template)) # Analyze a random sample
			}, 
			error = function(cond) { }, 
			finally = { }
		)
	}

	# From p-values to Type I error rates (false positives)
	res.05 <- round(colMeans(results<.05, na.rm = TRUE), digits = 4)

	# Split the results into separate rows 
	return(tribble(~method, ~rates,
			"PAR", res.05[1:3], 
			"ART", res.05[4:6],
			"RNK", res.05[7:9],
			"INT", res.05[10:12]
		)
	)
}


#Parallel: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
CoresNum <- 4
registerDoParallel(CoresNum)  # use multicore, set to the number of our cores

# Read the data template and fix the type of the factors, as required by the ARTool
template <- read.csv("template-1000.csv", sep=",", header=TRUE, strip.white=TRUE)
template$s = factor(template$s)
template$x1 = factor(template$x1)
template$x2 = factor(template$x2)

# Call the simulation
results <- test(template, repetitions = 100)

# Format the results
res <- results %>% unnest_wider(rates, names_sep = "_")
colnames(res)[1:4]=c("method", "rateX1","rateX2","rateX1X2")

# Store the results
csvfile <- paste("log/results-1000", format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

