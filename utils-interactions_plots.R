

# Code for plotting removable interaction effects of figure 2 using plotly
createInteractionPlot <- function(data, likert = FALSE, logscale = FALSE,  palette = c("#888888", "#FF5E00"), symbols = c("diamond", "x-thin"), offx = 0) {

  if(likert) {
    colnames(data)[3] <- "Time" # Change the name of the colun to Time to make the code consistent
  	ytitle <- 'Perceived performance'
  	type <- '-'
  	range <- c(0.3, 5.1)
  	lticktext <- list("very quick", "quick", "average", "slow", "very slow")
  	ltickvals <- list(1,2,3,4,5)
  } else {
	  ytitle <- ifelse(logscale, 'Time (min) - log scale', 'Time (min)')
	  type <- ifelse(logscale, 'log', '-')
	  range <- if(logscale) c(log10(0.25), log10(ceiling(max(data$Time)) + 0.1)) else c(0, ceiling(max(data$Time)) + 0.1)
	  lticktext <- if(logscale) list("0.25", "0.5", "1", "2", "4") else list("0", "1", "2", "3", "4")
	  ltickvals <- if(logscale) list(0.25, 0.5, 1, 2, 4) else list(0, 1, 2, 3, 4)
  }


  techA <- list(
	xref = 'paper',
  	yref = 'y',
  	x = if(logscale) 0.62 else 0.52 + offx,
  	y = if(logscale) log10((data[grepl("A", data$Technique),]$Time[1] + data[grepl("A", data$Technique),]$Time[2])/2)
  		else (data[grepl("A", data$Technique),]$Time[1] + data[grepl("A", data$Technique),]$Time[2])/2,
  	xanchor = 'left',
  	yanchor = 'top',
  	text = "Tech A",
  	font = list(family = 'Arial',
              size = 14,
              color = palette[1]),
  	showarrow = FALSE
  )

  techB <- list(
	xref = 'paper',
  	yref = 'y',
  	x = if(logscale) 0.62 else 0.52,
  	y = if(logscale) log10((data[grepl("B", data$Technique),]$Time[1] + data[grepl("B", data$Technique),]$Time[2])/2)
  		else (data[grepl("B", data$Technique),]$Time[1] + data[grepl("B", data$Technique),]$Time[2])/2,
  	xanchor = 'left',
  	yanchor = 'top',
  	text = "Tech B",
  	font = list(family = 'Arial',
              size = 14,
              color = palette[2]),
  	showarrow = FALSE
  )

  p <- plot_ly(data,  x = ~factor(Difficulty), y = ~Time, color = ~Technique, symbol = ~Technique, linetype = ~Technique, symbols = symbols, 
  	colors = palette, type = 'scatter', mode = 'lines+markers', line = list(width = 4), marker = list(line = list(width = 4)), showlegend = FALSE) %>%
  layout(
    xaxis = list(title = 'Difficulty', showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=0, tickfont = list(size = 12)),
    yaxis = list(type = type, title = ytitle, font = list(size = 13), zeroline = F, showline=T, 
    	linewidth=1, mirror = F,  nticks=4, ticks="inside", tickfont = list(size = 12), range = range, ticktext = lticktext, tickvals = ltickvals, 
    	tickangle = ifelse(likert, -30, 0))
  ) %>% layout(annotations = techA)  %>% layout(annotations = techB)  %>% 
        config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>% 
        layout(margin = list(l = 40, r = 0, b = 0, t = 40, pad = 0))

  p
}

# Code for plotting removable interaction effects of figure 2 using plotly -- Same as above but we inverse the variables
createInteractionPlotInv <- function(data, likert = FALSE, logscale = FALSE, palette = c("#888888", "#FF5E00"), symbols = c("diamond", "x-thin"), offx = 0, offy = 0) {

  if(likert) {
    colnames(data)[3] <- "Time" # Change the name of the colun to Time to make the code consistent
    ytitle <- 'Perceived performance'
    type <- '-'
    range <- c(0.3, 5.1)
    lticktext <- list("very quick", "quick", "average", "slow", "very slow")
    ltickvals <- list(1,2,3,4,5)
  } else {
    ytitle <- ifelse(logscale, 'Time (min) - log scale', 'Time (min)')
    type <- ifelse(logscale, 'log', '-')
    range <- if(logscale) c(log10(0.25), log10(ceiling(max(data$Time)) + 0.1)) else c(0, ceiling(max(data$Time)) + 0.1)
    lticktext <- if(logscale) list("0.25", "0.5", "1", "2", "4") else list("0", "1", "2", "3", "4")
    ltickvals <- if(logscale) list(0.25, 0.5, 1, 2, 4) else list(0, 1, 2, 3, 4)
  }


  Easy <- list(
  xref = 'paper',
    yref = 'y',
    x = if(logscale) 0.62 else 0.52 + offx,
    y = if(logscale) log10((data[data$Difficulty == "easy",]$Time[1] + data[data$Difficulty == "easy",]$Time[2])/2)
      else (data[data$Difficulty == "easy",]$Time[1] + data[data$Difficulty == "easy",]$Time[2])/2,
    xanchor = 'left',
    yanchor = 'top',
    text = "easy",
    font = list(family = 'Arial',
              size = 14,
              color = palette[1]),
    showarrow = FALSE
  )

  Hard <- list(
  xref = 'paper',
    yref = 'y',
    x = if(logscale) 0.62 else 0.52,
    y = if(logscale) log10((data[data$Difficulty == "hard",]$Time[1] + data[data$Difficulty == "hard",]$Time[2])/2)
      else (data[data$Difficulty == "hard",]$Time[1] + data[data$Difficulty == "hard",]$Time[2])/2 + offy,
    xanchor = 'left',
    yanchor = 'top',
    text = "hard",
    font = list(family = 'Arial',
              size = 14,
              color = palette[2]),
    showarrow = FALSE
  )

  p <- plot_ly(data,  x = ~factor(Technique), y = ~Time, color = ~Difficulty, symbol = ~Difficulty, linetype = ~Difficulty, symbols = symbols, 
    colors = palette, type = 'scatter', mode = 'lines+markers', line = list(width = 4), marker = list(line = list(width = 4)), showlegend = FALSE) %>%
  layout(
    xaxis = list(title = 'Technique', showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=0, tickfont = list(size = 12)),
    yaxis = list(type = type, title = ytitle, font = list(size = 13), zeroline = F, showline=T, 
      linewidth=1, mirror = F,  nticks=4, ticks="inside", tickfont = list(size = 12), range = range, ticktext = lticktext, tickvals = ltickvals, 
      tickangle = ifelse(likert, -30, 0))
  ) %>% layout(annotations = Easy)  %>% layout(annotations = Hard)  %>% 
        config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>% 
        layout(margin = list(l = 40, r = 0, b = 0, t = 40, pad = 0))

  p
}


createRankInteractionPlot <- function(data, rnkscale = FALSE, symbols = c("diamond", "x-thin"), palette = c("#888888", "#FF5E00")) {

  ytitle <- ifelse(rnkscale, 'rank(Time)', 'Time (sec)')
  range <- if(rnkscale) c(0, max(data$Time) + 10) else c(min(data$Time) - 2, max(data$Time) + 2)

  techA <- list(
  xref = 'paper',
    yref = 'y',
    x = 0.55,
    y = (data[data$Technique == "Tech A",]$Time[1] + data[data$Technique == "Tech A",]$Time[3])/2,
    xanchor = 'left',
    yanchor = 'top',
    text = "Tech A",
    font = list(family = 'Arial',
              size = 16,
              color = palette[1]),
    showarrow = FALSE
  )

  techB <- list(
  xref = 'paper',
    yref = 'y',
    x = 0.44,
    y = (data[data$Technique == "Tech B",]$Time[1] + data[data$Technique == "Tech B",]$Time[3])/2,
    xanchor = 'right',
    yanchor = 'bottom',
    text = "Tech B",
    font = list(family = 'Arial',
              size = 16,
              color = palette[2]),
    showarrow = FALSE
  )

  p <- plot_ly(data,  x = ~factor(Difficulty), y = ~Time, color = ~Technique, symbol = ~Technique, linetype = ~Technique, symbols = symbols, 
    colors = palette, type = 'scatter', mode = 'lines+markers', line = list(width = 4), marker = list(line = list(width = 4)), showlegend = FALSE) %>%
  layout(
    xaxis = list(title = 'Difficulty', showline=T, mirror = F, fixedrange=T, ticks="outside",tickangle=0, tickfont = list(size = 12)),
    yaxis = list(title = ytitle, font = list(size = 13), zeroline = F, showline=T, 
      linewidth=1, mirror = F,  nticks=4, ticks="inside", tickfont = list(size = 12), range = range)
      #ticktext = lticktext, tickvals = ltickvals)
  ) %>% layout(annotations = techA)  %>% layout(annotations = techB)  %>% 
        config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d",  "zoomIn2d", "zoomOut2d", "autoscale")) %>% 
        layout(margin = list(l = 60, r = 0, b = 1, t = 20, pad = 0))

  p
}
