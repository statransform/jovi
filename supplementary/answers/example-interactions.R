rm(list=ls())
library(tidyverse)
library(ggplot2)
library(gridExtra)

source("data-generators.R") # Import the data-generation functions

design <- c(4,3)

effects <- c(2, 0, 0)
distr <- "lnorm"
n <- 12

plotInteraction <- function(){
	df <- sim.within.2f(n, design, effects) %>% toDistribution(distr)
	df <- df[,c(1,2,3,9)] %>% 
		rename(Participant=s, Difficulty=x1, Technique=x2,Time=y) %>%
		mutate(Difficulty=recode(Difficulty,"A1" = "Level1", "A2"="Level2","A3"="Level3","A4"="Level4")) %>%
		mutate(Technique=recode(Technique,"B1" = "Technique A", "B2" = "Technique B", "B3" = "Technique C"))

	dfmeans <- aggregate(df, Time ~ Difficulty + Technique, FUN = mean)


	cbPalette <- c("#999999", "#E69F00", "#F15854")

	p <- dfmeans %>% ggplot(aes(x = Difficulty, y = Time, color = Technique, group=Technique)) + 
		geom_line(linewidth=1.2) +
		geom_point(size=2) +
		ylab("Mean Time (min)") +
		ylim(0, 4) + 
		theme_bw() + theme(legend.position = "none") + 
		scale_color_manual(values=cbPalette)
}

set.seed(3000)
p1 <- plotInteraction()
p2 <- plotInteraction()
p3 <- plotInteraction()
p4 <- plotInteraction()
p5 <- plotInteraction()
p6 <- plotInteraction()

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, widths=c(2.3, 2.3, 2.3))


