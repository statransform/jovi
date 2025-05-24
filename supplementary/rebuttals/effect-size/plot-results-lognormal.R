rm(list=ls())

data <- read.csv("log/effects-20.csv", sep=",", header=TRUE, strip.white=TRUE)
data$effectX2 <- factor(data$effectX2)

ylab = "X1 partial eta squared"
xlab = "X2 effect (a2)"

par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))

df <- data[data$effectX1 == 0,]
min = 0
max = 0.21

plot(df$effectX2, df$X1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "Groundtruth", col = "white")
plot(df$effectX2, df$parX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "PAR", col = "grey")
plot(df$effectX2, df$intX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "INT", col = "green3")
plot(df$effectX2, df$artX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "ART", col = "tomato1")

df <- data[data$effectX1 == 1,]
min = 0
max = 0.41

plot(df$effectX2, df$X1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "Groundtruth", col = "white")
plot(df$effectX2, df$parX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "PAR", col = "grey")
plot(df$effectX2, df$intX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "INT", col = "green3")
plot(df$effectX2, df$artX1, ylim = c(min, max), ylab = ylab, xlab = xlab, main = "ART", col = "tomato1")
