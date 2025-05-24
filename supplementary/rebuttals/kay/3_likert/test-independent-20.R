# author: Theophanis Tsandilas
# This code reads a template of a 4x3 repeated-measures design and makes the response variable take values from an ordinal scale
# by assuming a between-subjects design
# It then evaluates the Type I error rate of ART

rm(list=ls())

library(dplyr)
library(tidyr)

library(lmerTest)
library(ARTool)

# Parallel computation
library(foreach)
library(doParallel)


# Transforms a set of values to ordinal based on the given thresholds
# See: https://doi.org/10.1016/j.jesp.2018.08.009
toOrdinal = function(values, thresholds = c(-2,-1, 1,2)) {
	discretizeValue = function(x){
		for(i in 1:length(thresholds)) {
			if(x <= thresholds[i]) return(i)
		}
		return(length(thresholds) + 1)
	}

	sapply(values, discretizeValue)
}

# Transforms a set of values to ordinal based on the given thresholds
# See: https://doi.org/10.1016/j.jesp.2018.08.009
# Here, the thresholds take into account the variance of the data, trying to better cover the full range of values.
toOrdinalDistr = function(values, nlevels, equidistant = FALSE) {
	lim <- 2*sd(values) # I take 2 standard deviations that should contain the largest portion of the data (around 95% of the data values)

	thresholds <- NA
	if(equidistant) {
		thresholds <- seq(from=-lim, to=lim, by=2*lim/nlevels)[c(-1, -(nlevels + 1))]
	} else {
		thresholds <- sort(runif(nlevels - 1))*2*lim-lim
	}

	toOrdinal(values, thresholds)
}


# This method takes the template and randomly assigns ordinal values to the response variable y 
# We set the number of levels in the ordinal scale
createRandomSample <- function(df, nlevels = 5){
	# We first draw values from a standard normal distribution
	df$y <- rnorm(nrow(df), mean = 0, sd = 1) 

	# We then descretize these values usign either equidistant or flexible thresholds	
	df$y <- toOrdinalDistr(df$y, nlevels, equidistant = FALSE)

	df
}

# INT implementation
INT <- function(x){
    qnorm((rank(x) - 0.5)/length(x))
}


# Analysis with PAR, ART, RNK, and INT
analyze <- function(df) {
	m.par <- suppressMessages(aov(y ~ x1*x2, data=df)) # Parametric
	m.art <- suppressMessages(art(y ~ x1*x2, data=df)) # ARTool
	m.rnk <- suppressMessages(aov(rank(y) ~ x1*x2, data=df)) # RNK
	m.int <- suppressMessages(aov(INT(y) ~ x1*x2, data=df)) # INT

	vars <- c("x1", "x2", "x1:x2")
	c(# Return the p-values for the three effects 
		suppressMessages(anova(m.par)[vars, 5]), 
		suppressMessages(anova(m.art)[vars, 7]),
		suppressMessages(anova(m.rnk)[vars, 5]), 
		suppressMessages(anova(m.int)[vars, 5])
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
template <- read.csv("template-20.csv", sep=",", header=TRUE, strip.white=TRUE)
template$s = factor(template$s)
template$x1 = factor(template$x1)
template$x2 = factor(template$x2)

# Call the simulation
results <- test(template, repetitions = 3000)

# Format the results
res <- results %>% unnest_wider(rates, names_sep = "_")
colnames(res)[1:4]=c("method", "rateX1","rateX2","rateX1X2")

# Store the results
csvfile <- paste("log/results-independent-20", format(Sys.time(), "_%s"), ".csv", sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)

