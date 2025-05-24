library(ARTool) # ARTool library

df <- read.csv("example-data.csv", sep=",", header=TRUE, strip.white=TRUE)

df$Difficulty <- factor(df$Difficulty, ordered = TRUE) # We can consider it as ordinal
df$Technique <- factor(df$Technique)

mart <- art(Time ~ Difficulty*Technique + (1|Participant), data=df) # ARTool

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot(df$Time,rank(df$Time), bty="l", xlab = "Time", ylab = "rank (Time)")
#text(7,40, paste("Spearman correlation = ", round(cor(df$Time,rank(df$Time), method='spearman'), digits=2), sep=""))

plot(df$Time,mart$aligned.ranks[,1], bty="l", xlab = "Time", ylab = "ART-Difficulty (Time)") 
#text(7,40, paste("Spearman correlation = ", round(cor(df$Time,mart$aligned.ranks[,1], method='spearman'), digits=2), sep=""))

plot(df$Time,mart$aligned.ranks[,2], bty="l", xlab = "Time", ylab="ART-Technique (Time)")
#text(7,40, paste("Spearman correlation = ", round(cor(df$Time,mart$aligned.ranks[,2], method='spearman'), digits=2), sep=""))

