rm(list=ls())

data <- read.csv("log/effects-ordinal.csv", sep=",", header=TRUE, strip.white=TRUE)
data$n <- factor(data$n)

ylab = "X1 partial eta squared"
xlab = "n"

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

df <- data[data$effectX1 == 0,]
min = 0
max = 0.21

plot(df$n, df$X1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "Groundtruth", col = "white")
plot(df$n, df$parX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "PAR", col = "grey")
plot(df$n, df$intX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "INT", col = "green3")
plot(df$n, df$artX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "ART", col = "tomato1")
