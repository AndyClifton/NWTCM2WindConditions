PlotTiBoxAndWhiskers <- function(df.obs,
                                 caption){
  require(ggplot2)
  
  df.obs$WS.bin <- BinByWS(WS = df.obs$WS,
                           dWS = 2,
                           WS.min = 2,
                           WS.max = 28,
                           sep = NA)
  
  df.obs <- na.omit(df.obs)
  
  # get median, mode, P75, P25, P10, P90, P95, P05 from the data to match Neil Kelley's data
  # see http://stackoverflow.com/questions/4765482/changing-whisker-definition-in-geom-boxplot for background to this method
  # define the summary function
  f <- function(x) {
    r <- quantile(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }
  
  # define outliers    
  o <- function(x) {
    c(quantile(x,0.05),
      quantile(x,0.95))
  }
  
  # define the mean line
  m <- function(x){
    mean(x,na.rm = TRUE)
  }
  
  p <- ggplot(data = df.obs,
              aes(x = WS.bin,
                  y = Ti)) +
    stat_summary(fun.data=f, geom="boxplot") + 
    stat_summary(fun.y = o, geom="point") +
    stat_summary(fun.y = m, geom="point", colour = "red") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_discrete() + 
    labs(title = "Turbulence Intensity",
         subtitle = paste("Wind speeds greater than 2 m/s"),
         x = "Wind speed (m/s)",
         y = "Turbulence Intensity [-]",
         caption = caption)
}