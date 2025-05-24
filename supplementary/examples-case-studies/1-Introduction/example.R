rm(list=ls())
library(lmerTest) # Lmer functions
library(ARTool) # ARTool library
library(effectsize) # To estimate effect sizes 

library(tidyverse)
library("emmeans")


df <- read.csv("example-data.csv", sep=",", header=TRUE, strip.white=TRUE)

df$Participant <- factor(df$Participant)
df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df$Technique <- factor(df$Technique)


# Implementation of the Inverse Normal Transform
int = function(x){
   qnorm((rank(x) - 0.5)/length(x))
}

mpar <- lmer(Time ~ Difficulty*Technique + (1|Participant), data=df) # Parametric - no transformation
mlog <- lmer(log(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Parametric - Log-transformation
mart <- art(Time ~ Difficulty*Technique + (1|Participant), data=df) # ARTool
mrnk <- lmer(rank(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Rank transformation 
mint <- lmer(int(Time) ~ Difficulty*Technique + (1|Participant), data=df) # Inverse Transform transformation





