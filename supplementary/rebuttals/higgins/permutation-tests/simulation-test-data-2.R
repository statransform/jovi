# Author: Theophanis Tsandilas, 2024
# This code demonstrates failures of ART when permutating the additional dataset (example-data-2.csv)
# It shows that when we randomly assign Techniques to observations, ART still finds a main and interaction effect with a very high error rate  

rm(list=ls())

library(lmerTest) # Lmer functions
library(ARTool) # ARTool library
library(tidyverse)

# This function randomly assigns techniques (A,B,C) to each Difficulty level
createSample <- function(df, nlevels=4, n=12, toreplace){
	df[df$Difficulty == "Level1",]$Time <- sample(df[df$Difficulty == "Level1",]$Time, replace = toreplace)
	df[df$Difficulty == "Level2",]$Time <- sample(df[df$Difficulty == "Level2",]$Time, replace = toreplace)
	df[df$Difficulty == "Level3",]$Time <- sample(df[df$Difficulty == "Level3",]$Time, replace = toreplace)
	df[df$Difficulty == "Level4",]$Time <- sample(df[df$Difficulty == "Level4",]$Time, replace = toreplace)

	df$Technique <- rep(c("A","B","C"), nlevels*n)
	df$Technique <- factor(df$Technique)

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

	model0 <- suppressMessages(aov(Time ~ Difficulty*Technique, data=df)) # Parametric
	model1 <- suppressMessages(aov(rank(Time) ~ Difficulty*Technique, data=df)) # RNK
	model2 <- suppressMessages(art(Time ~ Difficulty*Technique, data=df)) # ARTool
	model3 <- suppressMessages(aov(inverseNormalTransform(Time) ~ Difficulty*Technique, data=df)) # INT
	model4 <- suppressMessages(aov(log(Time) ~ Difficulty*Technique, data=df)) # LOG

	vars <- c("Difficulty", "Technique", "Difficulty:Technique")
	c(getLmerPValues(model0, vars, FALSE), getLmerPValues(model1, vars, FALSE), getARTPValues(model2, vars, FALSE), getLmerPValues(model3, vars, FALSE), getLmerPValues(model4, vars, FALSE))
}


# Performs repetitive tests and calculates the rates of positives 
test <- function(df, repetitions, n, toreplace = FALSE) { # By default, we resample without replacement
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
			"PAR", 0.05, res.05[1:3],
			"RNK", 0.05, res.05[4:6],
			"ART", 0.05, res.05[7:9],
			"INT", 0.05, res.05[10:12],
			"LOG", 0.05, res.05[13:15],

			"PAR", 0.01, res.01[1:3],		
			"RNK", 0.01, res.01[4:6],
			"ART", 0.01, res.01[7:9],
			"INT", 0.01, res.01[10:12],
			"LOG", 0.01, res.01[13:15]
		)
	)
}

set.seed(3000)

# Data from our illustrative example (n=20)
df <- read.csv("example-data-2.csv", sep=",", header=TRUE, strip.white=TRUE)
n = 20

df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df <- df[c(2,4)][order(df$Difficulty),] # We remove the the Participant and Technique columns and order the values by difficulty

results <- test(df, repetitions = 5000, n)
res <- results %>% unnest_wider(rates, names_sep = "_")
colnames(res)[3:5]=c("rateDifficulty","rateTechnique","rateInteraction")

# Store the results
filename <- "permutation-example-rates-2.csv"
csvfile <- paste("logs/", filename, sep="")
write.csv(res, file = csvfile, row.names=FALSE, quote=F)
