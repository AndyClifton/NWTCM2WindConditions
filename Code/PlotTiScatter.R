PlotTiScatter <- function(df.obs,
                          caption){
  require(ggplot2)
  
  p <- ggplot(data = df.obs[df.obs$WS>2,],
              aes(x = WS,
                  y = Ti)) +
    geom_point(size = 1, 
               alpha= 0.2) + 
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Turbulence Intensity by Wind Direction Sector",
         subtitle = "Wind speeds greater than 2 m/s",
         x = "Wind speed (m/s)",
         y = "Turbulence Intensity [-]",
         caption = caption)
}