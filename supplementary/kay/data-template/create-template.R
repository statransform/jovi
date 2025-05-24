# author: Theophanis Tsandilas
# This code create a 4x3 repeated-measures design, where all response values are zero

rm(list=ls())

library(dplyr)
library(tidyr)
library(faux) # A great library for simulating data for a large range of desings: https://debruine.github.io/faux

# Number of subjects  -- We propose testing both a small sample size a large one n = 1000 (to assess the asymptotic behavior of the methods)
n <- 20
# n <- 1000

design <- c(4,3) # Focus on a 4x3 design

data <- add_random(s = n) %>%
	add_within("s", x1 = paste('A', 1:design[1], sep=""), x2 = paste('B', 1:design[2], sep="")) %>% 
	mutate(y = 0) # All responses are set to 0
data$s <- factor(data$s)

# Store the data template
csvfile <- paste("template-", n, ".csv", sep="")
write.csv(data, file = csvfile, row.names=FALSE, quote=F)
