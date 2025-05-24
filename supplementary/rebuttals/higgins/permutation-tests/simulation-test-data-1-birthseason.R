# Author: Theophanis Tsandilas, 2024
# This code demonstrates failures of ART when permutating the data of our illustrative example in Fig 1.
# It shows that when we randomly assign Techniques to observations, ART still finds a main and interaction effect with a very high error rate  
# But we now also add spurious variable Birthseason, producing a partial imbalance in the data

rm(list=ls())

library(lmerTest) # Lmer functions
library(ARTool) # ARTool library
library(tidyverse)

# This function randomly assigns techniques (A,B,C) to each Difficulty level
# We also add a spurious variable, the Birthseason of participants (1:spring, 2:summer, 3:automn, 4:winter) randomly assigned to independent observations
createSample <- function(df, nlevels=4, n=12, toreplace){
	df[df$Difficulty == "Level1",]$Time <- sample(df[df$Difficulty == "Level1",]$Time, replace = toreplace)
	df[df$Difficulty == "Level2",]$Time <- sample(df[df$Difficulty == "Level2",]$Time, replace = toreplace)
	df[df$Difficulty == "Level3",]$Time <- sample(df[df$Difficulty == "Level3",]$Time, replace = toreplace)
	df[df$Difficulty == "Level4",]$Time <- sample(df[df$Difficulty == "Level4",]$Time, replace = toreplace)

	df$Technique <- rep(c("A","B","C"), nlevels*n)
	df$Technique <- factor(df$Technique)
	df$Birthseason <- sample(c(1,2,3,4),144,replace=TRUE)
	df$Birthseason <- factor(df$Birthseason)

	df
}


# INT implementation
inverseNormalTransform <- function(x){
    qnorm((rank(x) - 0.5)/length(x))
}

# Functions to extract the p-values
getLmerPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 6, 5)])) # For aov models the table is different
}

getARTPValues <- function(model, vars, lmer = TRUE) {
	return(suppressMessages(anova(model)[vars, ifelse(lmer, 5, 7)])) # For aov models the ART's table is different
}

# Analysis with all five methods: PAR, RNK, ART, INT, and LOG
analyze <- function(df, n, toreplace) {
	df = createSample(df, n=n, toreplace = toreplace) 

	model0 <- suppressMessages(aov(Time ~ Difficulty*Technique*Birthseason, data=df)) # Parametric
	model1 <- suppressMessages(aov(rank(Time) ~ Difficulty*Technique*Birthseason, data=df)) # RNK
	model2 <- suppressMessages(art(Time ~ Difficulty*Technique*Birthseason, data=df)) # ARTool
	model3 <- suppressMessages(aov(inverseNormalTransform(Time) ~ Difficulty*Technique*Birthseason, data=df)) # INT
	model4 <- suppressMessages(aov(log(Time) ~ Difficulty*Technique*Birthseason, data=df)) # LOG

	vars <- c("Difficulty", "Technique", "Difficulty:Technique", "Birthseason", "Difficulty:Birthseason", "Technique:Birthseason", "Difficulty:Technique:Birthseason")
	c(getLmerPValues(model0, vars, FALSE), getLmerPValues(model1, vars, FALSE), getARTPValues(model2, vars, FALSE), getLmerPValues(model3, vars, FALSE), getLmerPValues(model4, vars, FALSE))
}


# Performs repetitive tests and calculates the rates of positives 
test <- function(df, repetitions, n, toreplace = FALSE) {
	results <- foreach(rid = 1:repetitions, .combine=rbind) %dopar% {
		tryCatch(
			{
				analyze(df, n, toreplace = toreplace)
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

	# Split the results into separate rows 
	return(tribble(~method, ~alpha, ~rates,
			"PAR", 0.05, res.05[1:7],
			"RNK", 0.05, res.05[8:14],
			"ART", 0.05, res.05[15:21],
			"INT", 0.05, res.05[22:28],
			"LOG", 0.05, res.05[29:35],

			"PAR", 0.01, res.01[1:7],		
			"RNK", 0.01, res.01[8:14],
			"ART", 0.01, res.01[15:21],
			"INT", 0.01, res.01[22:28],
			"LOG", 0.01, res.01[29:35]
		)
	)
}

set.seed(2222)

# Data from our illustrative example (n=12)
df <- read.csv("example-data-1.csv", sep=",", header=TRUE, strip.white=TRUE)
n = 12

df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df <- df[c(2,4)][order(df$Difficulty),] # We remove the the Participant and Technique columns and order the values by difficulty

results <- test(df, repetitions = 5000, n)
res <- results %>% unnest_wider(rates, names_sep = "_")
colnames(res)[3:9]=c("rateDifficulty","rateTechnique","rateInteraction1","rateBirthseason", "rateInteraction2","rateInteraction3","rateInteraction4")

# Store the results
filename <- "permutation-example-rates-birthseason.csv"
csvfile <- paste("logs/", filename, sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)
