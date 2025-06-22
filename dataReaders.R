# Author: Theophanis Tsandilas, Inria & Université Paris-Saclay

# the effectType parameter determines if I'm interested in results where there are effects on both factors (effectType = 0) 
# or only on the first factor (effectType = 1), second only factor (effectType = 2) or in all the results (effectType = 3)
readData <- function(prefix, n = 20, alpha = .05, effectType = 0, 
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		methods = c("PAR", "RNK", "INT", "ART")) {

	if(n < 0) {
		df <- read.csv(paste("data/", prefix, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
	}
	else {
		df <- read.csv(paste("data/", prefix, "_", n, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
	}

	df <- df[df$distr %in% distributions,]
	df$distr <- factor(df$distr, levels=distributions)
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
readlyData <- function(prefix, alpha = .05, effectType = 0,
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		methods = c("PAR", "RNK", "INT", "ART"), effectvars = c("effectX1","effectX2","effectX1X2")) {

	df <- rbind(
	  readData(prefix, 10, alpha, effectType, distributions, methods), 
	  readData(prefix, 20, alpha, effectType, distributions, methods),
	  readData(prefix, 30, alpha, effectType, distributions, methods)
	)

	# Different column for each n
	df <- reshape(df, idvar=c("design", "distr","method","alpha", effectvars), timevar = "n", direction = "wide")
	df <- df %>% group_by(distr) %>%  mutate(distr=dnames[cur_group_id()])
	df$distr <- factor(df$distr, levels = dnames)

	df
}

# Keep the column for sample size
readlyData1 <- function(prefix, alpha = .05, effectType = 0,
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		methods = c("PAR", "RNK", "INT", "ART"), effectvars = c("effectX1","effectX2","effectX1X2")) {

	df <- readData(prefix, -1, alpha, effectType, distributions, methods)

	# Different column for each n
#	df <- reshape(df, idvar=c("design", "distr","method","alpha", effectvars), timevar = "n", direction = "wide")
	df <- df %>% group_by(distr) %>%  mutate(distr=dnames[cur_group_id()])
	df$distr <- factor(df$distr, levels = dnames)

	df
}

# Alternative reader when a single file contains all sample sizes
readlyData2 <- function(prefix, alpha = .05, effectType = 0,
		distributions=c("norm", "lnorm", "exp", "cauchy", "poisson", "binom"),
		dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		methods = c("PAR", "RNK", "INT", "ART"), effectvars = c("effectX1","effectX2","effectX1X2")) {

	df <- read.csv(paste("data/", prefix, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
	df$distr <- factor(df$distr, levels=distributions)
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
	df <- reshape(df, idvar=c("design", "distr","method","alpha", effectvars), timevar = "n", direction = "wide")
	df <- df %>% group_by(distr) %>%  mutate(distr=dnames[cur_group_id()])
	df$distr <- factor(df$distr, levels = dnames)

	df
}

reshapeByDesign <- function(df, dnames = c("Normal", "Log-normal", "Exponential", "Cauchy", "Poisson", "Binomial"),
		effectvars = c("effectX1","effectX2","effectX1X2", "effectX3"), groupvars = c("distr","method","alpha", "n")) {

	# Different column for each design
	df <- reshape(df, idvar=c(groupvars, effectvars), timevar = "design", direction = "wide")
	df <- df %>% group_by(distr) %>%  mutate(distr=dnames[cur_group_id()])
	df$distr <- factor(df$distr, levels = dnames)

	df
}




#prefix <- "5_test-Power"
#distributions = c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")
#dnames = c("Normal", "Log-normal", "Exponential", "Poisson", "Binomial", "Ordinal (5 levels)")

#df <- readData(prefix, n = 20, alpha, effectType = 3, distributions)
#df1 <- df %>% arrange(design,distr,effectX1,rateX1)  %>% group_by(design,distr,effectX1) %>% mutate(rank = rank(rateX1))
#df <- as.data.frame(df1) %>% reshapeByDesign(dnames, effectvars = c("effectX1","effectX2","effectX1X2"))
