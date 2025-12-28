# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

# the effectType parameter determines if I'm interested in results where there are effects on both factors (effectType = 0) 
# or only on the first factor (effectType = 1), second only factor (effectType = 2) or in all the results (effectType = 3)
read_data <- function(prefix, alpha = .05, effectType = 0, 
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		methods = c("PAR", "RNK", "INT", "ART")) {

	df <- read.csv(paste("results/", prefix, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)

	df <- df[df$family %in% distributions,]
	df$family <- factor(df$family, levels=distributions)
	df$method <- factor(df$method, levels=methods)

	if(effectType == 0) {
		df <- df[(df$effectX2 > 0 & df$effectX1 > 0) | (df$effectX1 == 0 & df$effectX2 == 0),]
	} else if(effectType == 1) {
		df <- df[df$effectX2 == 0,]
	} else if(effectType == 2) {
		df <- df[df$effectX1 == 0,]
	} else if(effectType == 3) {
		df <- df[df$effectX1 > 0,]
	} else if(effectType == 4) {
		df <- df[df$effectX2 > 0,]
	} 
	else if(effectType == 5) {
		df <- df[df$effectX1X2 > 0,]
	} else if(effectType == 6) {
		df <- df[(df$effectX1X2 > 0 & df$effectX1 > 0) | (df$effectX1X2 == 0 & df$effectX1 == 0),]
	}

	if(is.na(alpha)) return(df)
	else return(df[df$alpha == alpha,])
}

# the effectType parameter determines if I'm interested in results where there are effects on both factors (effectType = 0) 
# or only on the first factor (effectType = 1) or in all the results (effectType = 2)
readly_data <- function(prefix, alpha = .05, effectType = 0,
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		methods = c("PAR", "RNK", "INT", "ART"), effectvars = c("effectX1","effectX2","effectX1X2")) {

	df <- read_data(prefix, alpha, effectType, distributions, methods)

	# Different column for each n
	df <- reshape(df, idvar=c("design", "family","method","alpha", effectvars), timevar = "n", direction = "wide")
	df <- df %>% group_by(family) %>%  mutate(family=dnames[cur_group_id()])
	df$family <- factor(df$family, levels = dnames)

	df
}

# Keep the column for sample size
readly_data_1 <- function(prefix, alpha = .05, effectType = 0,
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		methods = c("PAR", "RNK", "INT", "ART"), effectvars = c("effectX1","effectX2","effectX1X2")) {

	df <- read_data(prefix, alpha, effectType, distributions, methods)

	# Different column for each n
#	df <- reshape(df, idvar=c("design", "family","method","alpha", effectvars), timevar = "n", direction = "wide")
	df <- df %>% group_by(family) %>%  mutate(family=dnames[cur_group_id()])
	df$family <- factor(df$family, levels = dnames)

	df
}

# Alternative reader when a single file contains all sample sizes
readly_data_2 <- function(prefix, alpha = .05, effectType = 0,
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		methods = c("PAR", "RNK", "INT", "ART"), effectvars = c("effectX1","effectX2","effectX1X2")) {

	df <- read.csv(paste("data/", prefix, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
	df$family <- factor(df$family, levels=distributions)
	df$method <- factor(df$method, levels=methods)

	if(effectType == 0) {
		df <- df[(df$effectX2 > 0 & df$effectX1 > 0) | (df$effectX1 == 0 & df$effectX2 == 0),]
	} else if(effectType == 1) {
		df <- df[df$effectX2 == 0,]
	} else if(effectType == 2) {
		df <- df[df$effectX1 == 0,]
	}

	df <- df[df$alpha == alpha,]

	# Different column for each n
	df <- reshape(df, idvar=c("design", "family","method","alpha", effectvars), timevar = "n", direction = "wide")
	df <- df %>% group_by(family) %>%  mutate(family=dnames[cur_group_id()])
	df$family <- factor(df$family, levels = dnames)

	df
}

reshape_by_design <- function(df, dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		effectvars = c("effectX1","effectX2","effectX1X2", "effectX3"), groupvars = c("family","method","alpha", "n")) {

	# Different column for each design
	df <- reshape(df, idvar=c(groupvars, effectvars), timevar = "design", direction = "wide")
	df <- df %>% group_by(family) %>%  mutate(family=dnames[cur_group_id()])
	df$family <- factor(df$family, levels = dnames)		

	df
}

