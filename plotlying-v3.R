# Used in the appendix for some specific variations of the visualizations we produce

library(plotly)

# This is for alterative graphs for a different set of designs : 2x3, 2x4, 4x3 (writing more generic code with plotly is painful)
plotlyErrorByDesign_v3<- function(df, xlab = "magnitude of main effects", var = "rateX1X2", xvar = "effectX1", min = 0, max = 100, ytitle = 'Type I errors (%)', cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00"), nticks=8){
	# aesthetics
	symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

	menuItems <- c("2 within", "3 within", "4 within", "2 between", "3 between", "4 between")

	yvarnames <- c(paste(var,"within-2", sep="."), paste(var, "within-3", sep=".") ,paste(var, "within-4", sep="."), 
					paste(var, "between-2", sep="."), paste(var, "between-3", sep="."), paste(var, "between-4", sep="."))

	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[1])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[2])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[3])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[4])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[5])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[6])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
	  layout(
	    legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
	    xaxis = list(title = NA, showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=60, tickfont = list(size = 11)),
	    yaxis = list(title = ytitle, font = list(size = 13), zeroline = F, showline=T, linewidth=1, mirror = F,  nticks=nticks, ticks="inside", tickfont = list(size = 11), range=c(min, max))
	  ) 

	  p
	}

	fig <- df %>% 
	      do(
	        p = createPlot(., dnames, symbols) 
	      ) %>% 
	      subplot(nrows = 1, shareX = TRUE, shareY = T, margin = 0.004) %>% layout(annotations = annots) %>% 
	        layout(annotations = list(x = 0.5, y = 0, yshift = -36, xref = "paper", yref = "paper", xanchor = "center", yanchor = "top", showarrow = F, text = xlab), margin = margins) %>%
	      layout(
	        updatemenus = list(list(
	          y = 1.25, x = -0.06, yanchor = 'bottom', xanchor = 'left', direction = 'right', active = 0,
	          buttons = list(
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T,  F, F, F, F,  F, F, F, F,  F, F, F, F,  F, F, F, F,  F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F,  T, T, T, T,  F, F, F, F,  F, F, F, F,  F, F, F, F,  F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F,  F, F, F, F,  T, T, T, T,  F, F, F, F,  F, F, F, F,  F, F, F, F))),
	            list(label = menuItems[4], method = 'restyle', args = list("visible", list(F, F, F, F,  F, F, F, F,  F, F, F, F,  T, T, T, T,  F, F, F, F,  F, F, F, F))),
	            list(label = menuItems[5], method = 'restyle', args = list("visible", list(F, F, F, F,  F, F, F, F,  F, F, F, F,  F, F, F, F,  T, T, T, T,  F, F, F, F))),
	            list(label = menuItems[6], method = 'restyle', args = list("visible", list(F, F, F, F,  F, F, F, F,  F, F, F, F,  F, F, F, F,  F, F, F, F,  T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}


# This is for alterative graphs for a different set of designs : 2x3, 2x4, 4x3 specific to Power charts (writing more generic code with plotly is painful)
plotlyPowerByDesign_v3 <- function(df, xlab = "magnitude of main effects", var = "rank", hovervar="rateX1", xvar = "effectX1", max = 100, ytitle = 'Type I errors (%)', cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00")){
	# aesthetics
	symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

	menuItems <- c("4x3 within", "2x4 mixed")
	yvarnames <- c(paste(var,"4x3",sep="."), paste(var,"2x4",sep="."))
	hvarnames <- c(paste(hovervar,"4x3",sep="."), paste(hovervar,"2x4",sep="."))

	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~eval(as.symbol(yvarnames[1])), text = ~round(100*eval(as.symbol(hvarnames[1])), digits = 2), hoverinfo ="text", color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~eval(as.symbol(yvarnames[2])), text = ~round(100*eval(as.symbol(hvarnames[2])), digits = 2), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
	  layout(
	    legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
	    xaxis = list(title = NA, showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=60, tickfont = list(size = 11)),
	    yaxis = list(title = ytitle, font = list(size = 13), zeroline = F, showline=T, linewidth=1, mirror = F, ticks="inside", tickfont = list(size = 11), range=c(0.7, max), tickvals = list(1,2,3,4), ticktext = list("4th","3rd","2nd","1st"))
	  ) 

	  p
	}


	fig <- df %>% 
	      do(
	        p = createPlot(., dnames, symbols) 
	      ) %>% 
	      subplot(nrows = 1, shareX = TRUE, shareY = T, margin = 0.004) %>% layout(annotations = annots) %>% 
	        layout(annotations = list(x = 0.5, y = 0, yshift = -36, xref = "paper", yref = "paper", xanchor = "center", yanchor = "top", showarrow = F, text = xlab), margin = margins) %>%
	      layout(
	        updatemenus = list(list(
	          y = 1.25, x = -0.06, yanchor = 'bottom', xanchor = 'left', direction = 'right', active = 0,
	          buttons = list(
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}

source("dataReaders.R")
library(tidyverse)

readSingle <- function(prefix, n, alpha, distributions, methods, POWER = FALSE) {
  	df <- read.csv(paste("data/", prefix, "_", n, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
	  df$distr <- factor(df$distr, levels=distributions)
	  df$method <- factor(df$method, levels=methods)

    if(POWER) {
      df <- df[df$effect > 0,]
    }
    else {
      #df <- df[df$effect == 0,]
    }

    if(is.na(alpha)) return(df)
    else return(df[df$alpha == alpha,])
}

prefix <- "Appendix_test-One-Factor-Unequal-Var"
alpha = 0.05

distributions = c("norm", "lnorm", "exp", "poisson", "binom", "likert5B")
dnames = c("Normal", "Log-normal", "Exponential", "Poisson", "Binomial", "Ordinal (5 levels)")
methods = c("PAR", "RNK", "INT", "NON")
palette = c("#888888", "#E69F00", "#009E73", "#E42256")

df1 <- readSingle(prefix, n = 20, alpha, distributions, methods = methods)
df <- reshapeByDesign(df1, dnames, effectvars = c("sd_ratio", "effect"))

#plot <- plotlyErrorByDesign_v3(df, xlab = "magnitude of main effect", var = "rates", xvar = "effect", max = 100, nticks=8, cbPalette = palette)

plot <- plotlyErrorByDesign_v3(df, xlab = "magnitude of main effect", var = "rates", xvar = "sd_ratio", max = 20.2, nticks=8, cbPalette = palette)