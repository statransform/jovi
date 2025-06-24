#rm(list=ls())

#library(plotly)
#library(tidyverse)

library(reshape2)

plotlyScatter <- function(df, xlab = "eta squared (ground truth)", ylab= 'eta squared', xvar = "etaX1", yvar = paste(xvar, "_", sep =""), max = 0.4) {
  # aesthetics

  symbols <- c("asterisk", "x", "star-diamond", "star-triangle-up")
  palette <- c("#888888", "#E69F00", "#009E73", "#FF5E00")
  margins <- list(l = 50, r = 0, b = 60, t = 0, pad = 0)
  ldata = data.frame(x=c(0,5), y = c(0,5))

  dnames <- levels(df$distr)

  # To be used as labels on the subplots
  annots <- lapply(1:length(dnames), function(index){list(x = (index-0.5)/length(dnames), y = 1, text = dnames[index], xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = F)})

  menuItems <- c("4x3 within", "2x3 between", "2x4 mixed")
  xvarnames <- c(paste(xvar,"4x3",sep="."), paste(xvar,"2x3",sep="."), paste(xvar,"2x4",sep="."))
  yvarnames <- c(paste(yvar,"4x3",sep="."), paste(yvar,"2x3",sep="."), paste(yvar,"2x4",sep="."))

  createPlot <- function(data, dnames, symbols) {
    p <- plot_ly(colors = palette, symbols = symbols) %>%  add_trace(data=ldata, x = ~x, y = ~y, type = 'scatter', mode ="lines", line = list(color = "black"), showlegend = FALSE)
    p <- add_trace(p, data = data, y = ~eval(as.symbol(yvarnames[1])), x = ~eval(as.symbol(xvarnames[1])), color = ~method, symbol = ~method,  type = 'scatter', mode = 'markers', marker = list(line = list(width = 1)), legendgroup = ~method, showlegend = unique(data$distr == dnames[1]), visible = T) 
    p <- add_trace(p, data = data, y = ~eval(as.symbol(yvarnames[2])), x = ~eval(as.symbol(xvarnames[2])), color = ~method, symbol = ~method,  type = 'scatter', mode = 'markers', marker = list(line = list(width = 1)), legendgroup = ~method, showlegend = unique(data$distr == dnames[2]), visible = F)  
    p <- add_trace(p, data = data, y = ~eval(as.symbol(yvarnames[3])), x = ~eval(as.symbol(xvarnames[3])), color = ~method, symbol = ~method,  type = 'scatter', mode = 'markers', marker = list(line = list(width = 1)), legendgroup = ~method, showlegend = unique(data$distr == dnames[3]), visible = F) %>%  
    layout(
      legend = list(orientation = 'h', yanchor="bottom", xanchor="center", y = 1.2, x = .5),
      xaxis = list(title = xlab, showline=T, mirror = F, range = c(0, max)),
      yaxis = list(title = ylab, font = list(size = 13), zeroline = F, showline=T, mirror = F, range = c(0, max))
    )

    p
  }

  fig <- df %>% group_by(distr) %>% 
      do(
        p = createPlot(., dnames, symbols) 
      ) %>% 
      subplot(nrows = 1, shareX = F, shareY = F, titleY = T, titleX = T, margin = 0.04) %>% layout(annotations = annots) %>% 
        layout(dragmode = "pan", annotations = list(x = 0.5, y = 0, yshift = -36, xref = "paper", yref = "paper", xanchor = "center", yanchor = "top", showarrow = F, text =""), margin = margins) %>% style(hoverinfo = 'none') %>% 
        layout(
          updatemenus = list(list(
            y = 1.25, x = -0.06, yanchor = 'bottom', xanchor = 'left', direction = 'right', active = 0,
            buttons = list(
              list(label = menuItems[1], method = 'restyle', args = list("visible", list(T, T, T, T, T, F, F, F, F, F, F, F, F))),
              list(label = menuItems[2], method = 'restyle', args = list("visible", list(T, F, F, F, F, T, T, T, T, F, F, F, F))),
              list(label = menuItems[3], method = 'restyle', args = list("visible", list(T, F, F, F, F, F, F, F, F, T, T, T, T)))
            )))
        ) %>%
      config(displayModeBar = TRUE, scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoscale", "hoverClosestCartesian", "hoverCompareCartesian", "zoomIn2d", "zoomOut2d"))

  fig
}


readlyDataPoints <- function(prefix = "6_plot_Effect_Size", distributions=c("norm", "lnorm", "exp", "poisson", "binom", "likert5B"),
    dnames = c("Normal", "Log-normal", "Exponential", "Poisson", "Binomial", "Ordinal (5 levels)"), methods = c("PAR", "RNK", "INT", "ART"), effect = "X1") {

  df <- read.csv(paste("data/", prefix, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
  #df <- df[df$design=="2x3",]
  df <- df[df$distr %in% distributions,]
  df$distr <- factor(df$distr, levels=distributions)

  vars <- paste(tolower(methods), effect, sep ="") # e.g., parX1, rnkX1, ...

  df <- reshape2::melt(df, id.vars = c("n", "design", "distr", paste("eta", effect, sep="")), measure.vars = vars, 
      variable.name = "method", value.name = paste("eta", effect, "_", sep=""))

  df <- df %>% group_by(method) %>%  mutate(method = methods[cur_group_id()])
  df$method <- factor(df$method, levels = methods)

  df <- df %>% group_by(distr) %>%  mutate(distr=dnames[cur_group_id()])
  df$distr <- factor(df$distr, levels = dnames)

  # crearte an iteration column for each cell:
  df <- df %>% group_by(n,design,distr,method) %>% mutate(iteration = sequence(n()))

  groupvars = c("distr","method","n", "iteration")
  # Different column for each design
  df <- reshape(as.data.frame(df), idvar=groupvars, timevar = "design", direction = "wide")

  df
}

readlyDataPoints <- function(prefix = "6_plot_Effect_Size", distributions=c("norm", "lnorm", "exp", "poisson", "binom", "likert5B"),
    dnames = c("Normal", "Log-normal", "Exponential", "Poisson", "Binomial", "Ordinal (5 levels)"), methods = c("PAR", "RNK", "INT", "ART"), effect = "X1") {

  df <- read.csv(paste("data/", prefix, ".csv" , sep=""), sep=",", header=TRUE, strip.white=TRUE)
  #df <- df[df$design=="2x3",]
  df <- df[df$distr %in% distributions,]
  df$distr <- factor(df$distr, levels=distributions)

  vars <- paste(tolower(methods), effect, sep ="") # e.g., parX1, rnkX1, ...

  df <- reshape2::melt(df, id.vars = c("n", "design", "distr", paste("eta", effect, sep="")), measure.vars = vars, 
      variable.name = "method", value.name = paste("eta", effect, "_", sep=""))

  df <- df %>% group_by(method) %>%  mutate(method = methods[cur_group_id()])
  df$method <- factor(df$method, levels = methods)

  df <- df %>% group_by(distr) %>%  mutate(distr=dnames[cur_group_id()])
  df$distr <- factor(df$distr, levels = dnames)

  # crearte an iteration column for each cell:
  df <- df %>% group_by(n,design,distr,method) %>% mutate(iteration = sequence(n()))

  groupvars = c("distr","method","n", "iteration")
  # Different column for each design
  df <- reshape(as.data.frame(df), idvar=groupvars, timevar = "design", direction = "wide")

  df
}

toCohensf <- function(df){
    df[,5:10] <- sqrt(df[,5:10]/(1-df[,5:10]))
    names(df)[5:10] <- c("fX1.2x3", "fX1_.2x3", "fX1.2x4", "fX1_.2x4", "fX1.4x3", "fX1_.4x3")

    df
}

toCohensf2 <- function(df){
    df[,5:10] <- sqrt(df[,5:10]/(1-df[,5:10]))
    names(df)[5:10] <- c("fX1X2.2x3", "fX1X2_.2x3", "fX1X2.2x4", "fX1X2_.2x4", "fX1X2.4x3", "fX1X2_.4x3")

    df
}



#prefix <- "6_scatter-Effect_Size-Main"
#df <- readlyDataPoints(prefix,  distributions=c("norm", "lnorm", "likert5B"), dnames = c("Normal", "Log-normal", "Ordinal (5 levels)"))
#df_f <- df %>% toCohensf()
#fig <- plotlyScatter(df_f, xlab = "Ground truth", ylab= "Cohen's f", xvar = "fX1", max = 4.1)



