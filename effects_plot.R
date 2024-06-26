# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

# A great library for simulating data for a large range of desings: https://debruine.github.io/faux
library(faux)

library(dplyr)
library(tidyr)
library(egg)
library(latex2exp)

source("data-generators.R")

############################################################################################
# Data generation for various experimental designs

# Encodes the 1:nlevels to numerical values
# It ensures a fixed distance between the two extreme levels (equal to 1) and random distances in between
# It also ensures that the sum is zero
categ2ratio <- function(nlevels) { 
	levels <- sort(c(0,1, runif(nlevels-2)))

	return(levels - mean(levels))
}

# Transforms categorical to numverical
toNumber <- function(factorvalues) {
	as.numeric(as.character(factorvalues))
}

# This function produces difference variance ratios for each variable level
# where the most extreme will be 1 to maxratio
sd.ratios <- function(nlevels = 4, maxratio = 1.5) {
	ratios <- c(1, maxratio, 1 + runif(nlevels - 2)*(maxratio - 1))
	sample(ratios, replace=FALSE) - mean(ratios) + 1
}


getThresholds = function(values, nlevels = 5, equidistant = TRUE) {
	lim <- 2*sd(values) # I take 2 standard deviations that should contain the largest portion of the data

	thresholds <- NA
	if(equidistant) {
		thresholds <- seq(from=-lim, to=lim, by=2*lim/nlevels)[c(-1, -(nlevels + 1))]
	} else {
		thresholds <- sort(runif(nlevels - 1))*2*lim-lim
	}

	thresholds
}


simulate <- function(n = 30, nlevels = 2, effectSize = 1,  sd_0 = 1, sd_error = 1, convertFun = categ2ratio){
		data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels, sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept + effectSize * x1.t + sigma)

	data$s = factor(data$s)
	return(data)
}

# Simulate data under unequal variances
simulate.sd <- function(n = 30, nlevels = 2, effectSize = 1,  sd_0 = 1, sd_error = 1, convertFun = categ2ratio, sd_ratio = 1){
		data <- add_random(s = n) %>%
		add_within("s", x1 = paste('A', 1:nlevels, sep="")) %>% 
		mutate(x1.t = toNumber(plyr::mapvalues(x1, from = levels(x1), to = convertFun(nlevels)))) %>%  # Decode the categorical values to numeric ones
		mutate(x1.w = toNumber(plyr::mapvalues(x1, from = levels(x1), to = sd.ratios(nlevels[1], sd_ratio)))) %>% # This is to assign random weights to levels different variances within x1
		add_ranef(.by ="s", intercept = sd_0) %>% # Intercept random effect
		add_ranef(sigma = sd_error) %>% # error term
		mutate(dv = intercept + effectSize * x1.t + x1.w*sigma)

	data$s = factor(data$s)
	return(data)
}


plotDensities <- function(n = 10000, nlevels = 2, effectSize = 4, alpha = 0.5) {
  data <- simulate(n, nlevels, effectSize) 

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_density(aes(x = dv, fill = x1, color = x1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-8, 8, by=4), limits = c(-10, 10))

  plot
}


plotDensitiesThres <- function(data, nlevels = 2, alpha = 0.4,  equidistant = TRUE) {
  thres = getThresholds(data$dv, 5, equidistant)

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_density(aes(x = dv, fill = x1, color = x1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-8, 8, by=4), limits = c(-10, 10)) + 
  geom_vline(xintercept = thres, linetype="solid", color = "black", linewidth=0.5)

  plot
}

plotDensitiesVar <- function(n = 10000, nlevels = 2, sd_ratio = 2, alpha = 0.55) {
  data <- simulate.sd(n, nlevels, effectSize = 0, sd_ratio = sd_ratio) 

  palette <- c("#4681C6", "#FF5E00", "red", "#888888")

  plot <- ggplot(data) + geom_density(aes(x = dv, fill = x1, color = x1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-8, 8, by=4), limits = c(-10, 10))

  plot
}


plotEffects <- function(){
	set.seed(200)

	plot1 <- plotDensities(effectSize=0.5,nlevels=2)
	plot2 <- plotDensities(effectSize=1,nlevels=2)
	plot3 <- plotDensities(effectSize=2,nlevels=2)
	plot4 <- plotDensities(effectSize=4,nlevels=2)
	plot5 <- plotDensities(effectSize=8,nlevels=2)

	plot6 <- plotDensities(effectSize=0.5,nlevels=3)
	plot7 <- plotDensities(effectSize=1,nlevels=3)
	plot8 <- plotDensities(effectSize=2,nlevels=3)
	plot9 <- plotDensities(effectSize=4,nlevels=3)
	plot10 <- plotDensities(effectSize=8,nlevels=3)

	plot11 <- plotDensities(effectSize=0.5,nlevels=4)
	plot12 <- plotDensities(effectSize=1,nlevels=4)
	plot13 <- plotDensities(effectSize=2,nlevels=4)
	plot14 <- plotDensities(effectSize=4,nlevels=4)
	plot15 <- plotDensities(effectSize=8,nlevels=4)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 0.5$")) + centered,
	  plot2 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 1$")) + centered,
	  plot3 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 2$")) + centered,
	  plot4 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 4$")) + centered,
	  plot5 + ggtitle(TeX("$\\alpha_1$ or $\\alpha_2 = 8$")) + centered,
		 plot6, plot7, plot8, plot9, plot10, 
		 plot11, plot12, plot13, plot14, plot15, 
		 ncol= 5) 
} 


plotVariances <- function(){
	set.seed(300)
	plot1 <- plotDensitiesVar(sd_ratio=1,nlevels=2)
	set.seed(300)
	plot2 <- plotDensitiesVar(sd_ratio=1.5,nlevels=2)
	set.seed(300)
	plot3 <- plotDensitiesVar(sd_ratio=2,nlevels=2)
	set.seed(300)
	plot4 <- plotDensitiesVar(sd_ratio=2.5,nlevels=2)
	set.seed(300)
	plot5 <- plotDensitiesVar(sd_ratio=3,nlevels=2)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$r_{sd} = 1$")) + centered,
	  plot2 + ggtitle(TeX("$r_{sd} = 1.5$")) + centered,
	  plot3 + ggtitle(TeX("$r_{sd} = 2$")) + centered,
	  plot4 + ggtitle(TeX("$r_{sd} = 2.5$")) + centered,
	  plot5 + ggtitle(TeX("$r_{sd} = 3$")) + centered,
		 ncol= 5) 
}

middleY <- function(plot) {
	layer_scales(plot)$y$range$range[2]/2
}


plotThresholds <- function(){
	set.seed(600)

  data1 <- simulate(n = 1000, nlevels = 4, effectSize = 2) 
  data2 <- simulate(n = 1000, nlevels = 4, effectSize = 8) 

	plot1 <- plotDensitiesThres(data1, nlevels=4, alpha = 0.35, equidistant = TRUE)
	plot2 <- plotDensitiesThres(data2, nlevels=4, alpha = 0.35, equidistant = TRUE)
	plot3 <- plotDensitiesThres(data1, nlevels=4, alpha = 0.35, equidistant = FALSE)
	plot4 <- plotDensitiesThres(data2, nlevels=4, alpha = 0.35, equidistant = FALSE)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$\\alpha_1 = 2$")) + centered + annotate("text", x=-10, y=middleY(plot1), label="Equidistant", family = "Arial", size = 4, hjust = 0),
	  plot2 + ggtitle(TeX("$\\alpha_1 = 8$")) + centered,
	  plot3 + annotate("text", x=-10, y=middleY(plot3), label="Flexible", family = "Arial", size = 4, hjust = 0),
	  plot4,
		ncol= 2) 
} 


# =======================
# Ploting the log-normal distributions in the appendix
plotDensitiesLognormal <- function(distr = "lnorm-0.2", n=10000, nlevels, effectSize, alpha = 0.6, limits = c(-10, 10)) {
	data <- simulate(n = n, nlevels = nlevels, effectSize = effectSize) %>% toDistribution(distr)

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_density(aes(x = y, fill = x1, color = x1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), axis.line.x  = element_blank(),
  	axis.title.y = element_blank(), axis.ticks  = element_blank(), axis.text  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-8, 8, by=4), limits = limits)

  plot
}


plotEffectsLognormal <- function(nlevels = 2, effectSize = 2){
	set.seed(200)

	plot1 <- plotDensitiesLognormal(distr = "lnorm-0.2", nlevels = nlevels, effectSize = effectSize, limits = c(0.3, 2))
	plot2 <- plotDensitiesLognormal(distr = "lnorm-0.4", nlevels = nlevels, effectSize = effectSize, limits = c(0,3))
	plot3 <- plotDensitiesLognormal(distr = "lnorm-0.6", nlevels = nlevels, effectSize = effectSize, limits = c(0,4))
	plot4 <- plotDensitiesLognormal(distr = "lnorm-0.8", nlevels = nlevels, effectSize = effectSize, limits = c(0,5.5))
	plot5 <- plotDensitiesLognormal(distr = "lnorm", nlevels = nlevels, effectSize = effectSize, limits = c(0,6.5))
	plot6 <- plotDensitiesLognormal(distr = "lnorm-1.2", nlevels = nlevels, effectSize = effectSize, limits = c(0,7.5))

	centered <- theme(plot.title = element_text(hjust = 0.5, vjust = -1, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$sigma = 0.2$")) + centered,
	  plot2 + ggtitle(TeX("$sigma = 0.4$")) + centered,
	  plot3 + ggtitle(TeX("$sigma = 0.6$")) + centered,
	  plot4 + ggtitle(TeX("$sigma = 0.8$")) + centered,
	  plot5 + ggtitle(TeX("$sigma = 1.0$")) + centered,
	  plot6 + ggtitle(TeX("$sigma = 1.2$")) + centered,
			ncol= 6) 
}
