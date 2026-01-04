# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

library(dplyr)
library(tidyr)
library(egg)
library(latex2exp)

source("utils-data-generation.R")

# Distribution parameters used in simulation
params_norm   <- list(sigma_e = 1, sigma_s = c(0, 0))
params_lnorm  <- list(sigma_e = 1, mean_target = 1, sigma_s = c(0, 0))
params_cauchy <- list(gamma = 1, sigma_s = c(0, 0))
params_exp    <- list(mean_target = 0.5, sigma_s = c(0, 0))
params_poisson <- list(mean_target = 3, sigma_s = c(0, 0))
params_binom  <- list(size = 10, p_target = 0.1, sigma_s = c(0, 0))

plot_densities <- function(
			n = 10000, nlevels = 2, effectSize = 4, family = "norm", params = params_norm, 
			alpha = 0.5, breaks = seq(-8, 8, by=4), xlim = c(-10, 10)
) {
  data <- simulate_response(nlevels = nlevels, within = 1, n = n, coeffs = c("X1"=effectSize), family = family, params = params) 

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_density(aes(x = Y, fill = X1, color = X1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = breaks, limits = xlim)

  plot
}

plot_densities_discrete <- function(
			n = 10000, nlevels = 2, effectSize = 4, family = "binom", params = params_binom, 
			alpha = 0.5, breaks = seq(0, 10, by=1), xlim = c(0, 10)
) {
  data <- simulate_response(nlevels = nlevels, within = 1, n = n, coeffs = c("X1"=effectSize), family = family, params = params) 

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_bar(aes(x = Y, fill = X1, color = X1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = breaks, limits = xlim)

  plot
}

plot_densities_sd <- function(n = 10000, nlevels = 2, sd_ratio, alpha = 0.55) {
	params <- list(sigma_e = 1, sigma_s = c(0, 0), ratio_sd = sd_ratio)
  data <- simulate_heteroscedastic_response(nlevels = nlevels, within = 1, n = n, coeffs = c("X1"=0), family = "norm", params = params) 

  palette <- c("#4681C6", "#FF5E00", "red", "#888888")

  plot <- ggplot(data) + geom_density(aes(x = Y, fill = X1, color = X1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-4, 4, by=2), limits = c(-5, 5))

  plot
}


plot_effects_normal <- function(){
	set.seed(200)

	plot1 <- plot_densities(effectSize=0.5,nlevels=3)
	plot2 <- plot_densities(effectSize=1,nlevels=3)
	plot3 <- plot_densities(effectSize=2,nlevels=3)
	plot4 <- plot_densities(effectSize=4,nlevels=3)
	plot5 <- plot_densities(effectSize=8,nlevels=3)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$\\alpha_1 = 0.5$")) + centered,
	  plot2 + ggtitle(TeX("$\\alpha_1 = 1$")) + centered,
	  plot3 + ggtitle(TeX("$\\alpha_1 = 2$")) + centered,
	  plot4 + ggtitle(TeX("$\\alpha_1 = 4$")) + centered,
	  plot5 + ggtitle(TeX("$\\alpha_1 = 8$")) + centered,
		 ncol= 5) 
} 


plot_effects_lognormal <- function(){
	set.seed(200)

	family <- "lnorm"
  params <- list(sigma_e = 0.2, mean_target = 1, normalize = TRUE, sigma_s = c(0, 0), normalize = TRUE) 
	plot1 <- plot_densities(effectSize=2,	nlevels=2, family = family, params = params, xlim = c(0, 3), breaks = 0:3)

  params$sigma_e = 0.4
	plot2 <- plot_densities(effectSize=2,	nlevels=2, family = family, params = params, xlim = c(0, 3), breaks = 0:3)

  params$sigma_e = 0.6
	plot3 <- plot_densities(effectSize=2,	nlevels=2, family = family, params = params, xlim = c(0, 3), breaks = 0:3)

  params$sigma_e = 0.8
	plot4 <- plot_densities(effectSize=2,	nlevels=2, family = family, params = params, xlim = c(0, 3), breaks = 0:3)

  params$sigma_e = 1.0
	plot5 <- plot_densities(effectSize=2,	nlevels=2, family = family, params = params, xlim = c(0, 3), breaks = 0:3)

  params$sigma_e = 1.2
	plot6 <- plot_densities(effectSize=2,	nlevels=2, family = family, params = params, xlim = c(0, 3), breaks = 0:3)

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


plot_effects_distr <- function(){
	set.seed(200)

	plot1 <- plot_densities(effectSize=2,nlevels=3, family = "lnorm", params = params_lnorm, xlim = c(0, 3), breaks = seq(0, 3, by=1))
	plot2 <- plot_densities(effectSize=2,nlevels=3, family = "exp", params = params_exp, xlim = c(0, 3), breaks = seq(0, 3, by=1))
	plot3 <- plot_densities(effectSize=2,nlevels=3, family= "cauchy", params = params_cauchy)
	plot4 <- plot_densities_discrete(effectSize=2,nlevels=3, family="poisson", params = params_poisson, xlim = c(-0.5, 13), breaks = seq(0, 12, by=2))
	plot5 <- plot_densities_discrete(effectSize=2,nlevels=3, family="binom", params = params_binom, xlim = c(-0.5, 7), breaks = seq(0, 7, by=1))

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("Log-normal")) + centered,
	  plot2 + ggtitle(TeX("Exponential")) + centered,
	  plot3 + ggtitle(TeX("Cauchy")) + centered,
	  plot4 + ggtitle(TeX("Poisson")) + centered,
	  plot5 + ggtitle(TeX("Binomial")) + centered,
		ncol= 5) 
} 

########################################
## Plot the thresholds for the ordinal scale
plot_densities_thres <- function(data, nlevels = 2, alpha = 0.4,  equidistant = TRUE) {
	get_thresholds <- function(values, nlevels = 5, equidistant = TRUE) {
		lim <- 2*sd(values) # I take 2 standard deviations that should contain the largest portion of the data

		thresholds <- NA
		if(equidistant) {
			thresholds <- seq(from=-lim, to=lim, by=2*lim/nlevels)[c(-1, -(nlevels + 1))]
		} else {
			thresholds <- sort(runif(nlevels - 1))*2*lim-lim
		}

		thresholds
	}

  thres <- get_thresholds(data$Y, 5, equidistant)

  palette <- c("#888888", "#4E84C4", "red", "#FF5E00")

  plot <- ggplot(data) + geom_density(aes(x = Y, fill = X1, color = X1), alpha = alpha) + scale_color_manual(values = rep("#FFFFFF00",nlevels)) + scale_fill_manual(values = palette) + 
  theme_classic() +
  theme(plot.margin=unit(c(0,0,0,0), "cm"), legend.position = "none", axis.title.x  = element_blank(), axis.line.y  = element_blank(), 
  	axis.title.y = element_blank(), axis.ticks.y  = element_blank(), axis.text.y  = element_blank()) +
  scale_x_continuous(name =element_blank(), breaks = seq(-8, 8, by=4), limits = c(-10, 10)) + 
  geom_vline(xintercept = thres, linetype="solid", color = "black", linewidth=0.5)

  plot
}

plot_thresholds <- function(){
	middleY <- function(plot) {
		layer_scales(plot)$y$range$range[2]/2
	}

#	set.seed(7849)
#set.seed(2119)
set.seed(6020)
  data1 <- simulate_response(nlevels = 3, within = 1, n = 10000, coeffs = c("X1" = 2), family = "norm", params = params_norm) 
  data2 <- simulate_response(nlevels = 3, within = 1, n = 10000, coeffs = c("X1" = 8), family = "norm", params = params_norm) 

	plot1 <- plot_densities_thres(data1, nlevels=4, alpha = 0.35, equidistant = TRUE)
	plot2 <- plot_densities_thres(data2, nlevels=4, alpha = 0.35, equidistant = TRUE)
	plot3 <- plot_densities_thres(data1, nlevels=4, alpha = 0.35, equidistant = FALSE)
	plot4 <- plot_densities_thres(data2, nlevels=4, alpha = 0.35, equidistant = FALSE)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$\\alpha_1 = 2$")) + centered + annotate("text", x=-10, y=middleY(plot1), label="Equidistant", family = "Arial", size = 4, hjust = 0),
	  plot2 + ggtitle(TeX("$\\alpha_1 = 8$")) + centered,
	  plot3 + annotate("text", x=-10, y=middleY(plot3), label="Flexible", family = "Arial", size = 4, hjust = 0),
	  plot4,
		ncol= 2) 
} 

plot_unequal_variances <- function(){
	set.seed(300)
	plot1 <- plot_densities_sd(sd_ratio=1,nlevels=2)
	set.seed(300)
	plot2 <- plot_densities_sd(sd_ratio=1.5,nlevels=2)
	set.seed(300)
	plot3 <- plot_densities_sd(sd_ratio=2,nlevels=2)
	set.seed(300)
	plot4 <- plot_densities_sd(sd_ratio=2.5,nlevels=2)
	set.seed(300)
	plot5 <- plot_densities_sd(sd_ratio=3,nlevels=2)

	centered <- theme(plot.title = element_text(hjust = 0.5, size = 12))

	ggarrange(
	  plot1 + ggtitle(TeX("$r_{sd} = 1$")) + centered,
	  plot2 + ggtitle(TeX("$r_{sd} = 1.5$")) + centered,
	  plot3 + ggtitle(TeX("$r_{sd} = 2$")) + centered,
	  plot4 + ggtitle(TeX("$r_{sd} = 2.5$")) + centered,
	  plot5 + ggtitle(TeX("$r_{sd} = 3$")) + centered,
		 ncol= 5) 
}