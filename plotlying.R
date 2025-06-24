
library(plotly)

plotlyError <- function(df, xlab = "magnitude of main effects", var = "rateX1X2", xvar = "effectX1", ytitle = 'Type I errors (%)', max = 100, 
	cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00"), symbols = c("asterisk", "x", "star-diamond", "star-triangle-up")){
	# aesthetics
	
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

	menuItems <- c("n = 10", "n = 20", "n = 30")
	yvarnames <- c(paste(var,"10", sep="."), paste(var,"20", sep="."), paste(var,"30", sep="."))
	
	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[1])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[2])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[3])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
	  layout(
	    legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
	    xaxis = list(title = NA, showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=60, tickfont = list(size = 11)),
	    yaxis = list(title = ytitle, font = list(size = 13), zeroline = F, showline=T, linewidth=1, mirror = F,  nticks=6, ticks="inside", tickfont = list(size = 11), range=c(0, max))
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
	          y = 1.25, x = -0.06, yanchor = 'bottom', xanchor = 'left', direction = 'right', active = 1,
	          buttons = list(
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T, F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}

plotlyErrorByEffect <- function(df, xlab = "n", max = 100, 
	cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00"), symbols = c("asterisk", "x", "star-diamond", "star-triangle-up")){
	# aesthetics
	
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})
	menuItems <- c("main effect (X1)", "main effect (X2)", "interaction effect")
	
	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(n), y = ~100*rateX1, color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(n), y = ~100*rateX2, color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(n), y = ~100*rateX1X2, color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
	  layout(
	    legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
	    xaxis = list(title = NA, showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=60, tickfont = list(size = 11)),
	    yaxis = list(title = 'Type I errors (%)', font = list(size = 13), zeroline = F, showline=T, linewidth=1, mirror = F,  nticks=6, ticks="inside", tickfont = list(size = 11), range=c(0, max))
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
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T, F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}



plotlyErrorByDesign <- function(df, xlab = "magnitude of main effects", var = "rateX1X2", xvar = "effectX1", ytitle = 'Type I errors (%)', max = 100){
	# aesthetics
	symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
	cbPalette <- c("#888888", "#E69F00", "#009E73", "#FF5E00")
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

	menuItems <- c("2x3 between", "2x4 mixed", "2x2x2 within", "3x3x3 within")
	yvarnames <- c(paste(var,"2x3",sep="."), paste(var,"2x4",sep="."), paste(var,"2x2x2",sep="."), paste(var,"3x3x3",sep="."))

	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[1])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[2])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[3])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[4])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
	  layout(
	    legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
	    xaxis = list(title = NA, showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=60, tickfont = list(size = 11)),
	    yaxis = list(title = ytitle, font = list(size = 13), zeroline = F, showline=T, linewidth=1, mirror = F,  nticks=6, ticks="inside", tickfont = list(size = 11), range=c(0, max))
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
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, T, T, T, T, F, F, F, F))),
	            list(label = menuItems[4], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}

# This is for alterative graphs for a different set of designs : 2x3, 2x4, 4x3 (writing more generic code with plotly is painful)
plotlyErrorByDesign2 <- function(df, xlab = "magnitude of main effects", var = "rateX1X2", xvar = "effectX1", min = 0, max = 100, 
	    ytitle = 'Type I errors (%)', cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00"), nticks=6){
	# aesthetics
	symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

	menuItems <- c("4x3 within", "2x3 between", "2x4 mixed")
	yvarnames <- c(paste(var,"4x3",sep="."), paste(var,"2x3",sep="."), paste(var,"2x4",sep="."))

	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[1])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[2])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~100*eval(as.symbol(yvarnames[3])), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
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
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T, F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}


# This is for alterative graphs for a different set of designs : 2x3, 2x4, 4x3 specific to Power charts (writing more generic code with plotly is painful)
plotlyPowerByDesign <- function(df, xlab = "magnitude of main effects", var = "rank", hovervar="rateX1", xvar = "effectX1", max = 100, ytitle = 'Type I errors (%)', cbPalette = c("#888888", "#E69F00", "#009E73", "#FF5E00")){
	# aesthetics
	symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
	margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)

	dnames <- levels(df$distr)
	# To be used as labels on the subplots
	annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

	menuItems <- c("4x3 within", "2x3 between", "2x4 mixed")
	yvarnames <- c(paste(var,"4x3",sep="."), paste(var,"2x3",sep="."), paste(var,"2x4",sep="."))
	hvarnames <- c(paste(hovervar,"4x3",sep="."), paste(hovervar,"2x3",sep="."), paste(hovervar,"2x4",sep="."))

	createPlot <- function(data, dnames, symbols) {
	  p <- plot_ly(data,  x = ~factor(eval(as.symbol(xvar))), y = ~eval(as.symbol(yvarnames[1])), text = ~round(100*eval(as.symbol(hvarnames[1])), digits = 2), hoverinfo ="text", color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~eval(as.symbol(yvarnames[2])), text = ~round(100*eval(as.symbol(hvarnames[2])), digits = 2), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) 
	  p <- add_trace(p, data = data, x = ~factor(eval(as.symbol(xvar))), y = ~eval(as.symbol(yvarnames[3])), text = ~round(100*eval(as.symbol(hvarnames[3])), digits = 2), color = ~method, symbol = ~method, symbols = symbols, colors = cbPalette, type = 'scatter', mode = 'lines+markers', marker = list(line = list(width = 2)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = F) %>%
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
	            list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, F, F, F, F, F, F, F, F))),
	            list(label = menuItems[2], method = 'restyle', args = list("visible", list(F, F, F, F, T, T, T, T, F, F, F, F))),
	            list(label = menuItems[3], method = 'restyle', args = list("visible", list(F, F, F, F, F, F, F, F, T, T, T, T)))
	          )))
	      ) %>% 
	      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>%
	      layout(hovermode = 'x')

	fig
}


#source("dataReaders.R")
#library(tidyverse)
#alpha = 0.05

#prefix <- "2_test_4x3_Ordinal_sample_size"

#distributions <- c("likert5", "likert5B", "likert7", "likert7B", "likert11", "likert11B")
#dnames <- c("5 - equidistant", "5 - flexible", "7 - equidistant", "7 - flexible", "11 - equidistant", "11 - flexible")
#df <- readlyData1(prefix, alpha, 0, distributions, dnames)

#plotlyErrorByEffect(df, xlab = "n", max = 100)

#prefix <- "Appendix_test_lognormal"
#distributions = c("lnorm-0.2", "lnorm-0.4", "lnorm-0.6", "lnorm-0.8", "lnorm-1.0", "lnorm-1.2")
#dnames = c("sdlog= 0.2", "sdlog= 0.4", "sdlog= 0.6", "sdlog= 0.8", "sdlog= 1.0", "sdlog= 1.2")

#df <- readData(prefix, n = 20, alpha = .05, effectType = 1, distributions)
#df <- reshapeByDesign(df, dnames, effectvars = c("effectX1","effectX2","effectX1X2"))
#plotlyErrorByDesign2(df, xlab = "magnitude of interaction effect", var = "rateX2", xvar = "effectX1", max = 69)



 #prefix <- "6_test-Effect-Size-Main"
 #distributions = c("norm", "lnorm", "exp", "poisson", "binom", "likert5B", "likert7B", "likert20B")
# dnames = c("Normal", "Log-normal", "Exponential", "Poisson", "Binomial", "Ordinal (5 levels)", "Ordinal (7 levels)", "Ordinal (20 levels)")
 #dnames = c("Normal", "Log-normal", "Exponential", "Poisson", "Binomial", "Ordinal (5 levels)")

 #df <- readData(prefix, n = 20, alpha= NA, effectType = -1, distributions = distributions)

 #df <- df %>% arrange(design,distr,effectX1X2,etaX1X2)  %>% group_by(design,distr,effectX1X2) #%>% mutate(rank = rank(etaX2))
 #df <- as.data.frame(df) %>% reshapeByDesign(dnames, effectvars = c("effectX1","effectX2","effectX1X2"),  groupvars = c("distr","method","n"))

# df <- reshapeByDesign(df, dnames, effectvars = c("effectX1","effectX2","effectX1X2"))
 #plotlyErrorByDesign3(df, xlab = "magnitude of main effect", var = "rank", hovervar = "etaX2", xvar = "effectX2", max = 4.2, ytitle = 'r (%) - ranking')
 #plotlyErrorByDesign2(df, xlab = "magnitude of main effect", var = "etaX1", xvar = "effectX1X2", min = 10, max = 103, ytitle = 'r (%)')
