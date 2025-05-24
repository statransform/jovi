rm(list=ls())


library(lmerTest) # Lmer functions
library(ARTool) # ARTool library
#library(effectsize) # To estimate effect sizes 

options(scipen = 999) # To show full decimal digits

# Implementation of the Inverse Normal Transform
int = function(x){
   qnorm((rank(x) - 0.5)/length(x))
}

printAnalysis <- function(num, method, model) {
  cat(num, ". Analysis using ", method, "\n\n", sep="")

  print(anova(model))
  #print(eta_squared(model))
  #print(cohens_f(model))
}

df <- read.csv("large-sample-ratings.csv", sep=",", header=TRUE, strip.white=TRUE)
df$Participant <- factor(df$Participant)
df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df$Technique <- factor(df$Technique)

mpar <- lmer(Rating ~ Difficulty*Technique + (1|Participant), data=df) # Parametric - no transformation
mart <- art(Rating ~ Difficulty*Technique + (1|Participant), data=df) # ARTool


df$rnk <- rank(df$Rating)
df$int <- int(df$Rating)

mrnk <- lmer(rnk ~ Difficulty*Technique + (1|Participant), data=df) # Rank transformation 
mint <- lmer(int ~ Difficulty*Technique + (1|Participant), data=df) # Inverse Transform transformation
mrnkart <- art(rnk ~ Difficulty*Technique + (1|Participant), data=df) # RNK then ART
mintart <- art(int ~ Difficulty*Technique + (1|Participant), data=df) # INT then ART

printAnalysis(1, "PAR", mpar)
printAnalysis(2, "ART", mart)
printAnalysis(3, "RNK", mrnk)
printAnalysis(4, "INT", mint)

printAnalysis(5, "RNK -> ART", mrnkart)
printAnalysis(6, "INT -> ART", mintart)
