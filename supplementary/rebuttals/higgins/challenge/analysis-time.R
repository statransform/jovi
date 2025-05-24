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

df <- read.csv("large-sample-times.csv", sep=",", header=TRUE, strip.white=TRUE)
df$Participant <- factor(df$Participant)
df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df$Technique <- factor(df$Technique)

mpar <- lmer(Time ~ Difficulty*Technique + (1|Participant), data=df) # Parametric - no transformation
mart <- art(Time ~ Difficulty*Technique + (1|Participant), data=df) # ARTool

mlog <- lmer(log(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Parametric - Log-transformation
mrnk <- lmer(rank(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Rank transformation 
mint <- lmer(int(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Inverse Transform transformation

printAnalysis(1, "PAR", mpar)
printAnalysis(2, "ART", mart)
printAnalysis(5, "LOG", mlog)
printAnalysis(4, "RNK", mrnk)
printAnalysis(5, "INT", mint)

