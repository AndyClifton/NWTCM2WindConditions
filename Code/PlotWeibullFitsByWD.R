PlotWeibullFitsByWD <- function(df,
                                caption){
  
  # define the range of data we will use
  WS.min = 2.5
  WS.max = 25.5
  
  # first bin by wind speed 
  df$WS.bin <- BinByWS(WS = df$WS,
                       dWS = 1,
                       WS.min = WS.min,
                       WS.max = WS.max,
                       sep = NA)
  
  df <- df[((df$WS < WS.max) & (df$WS >=WS.min)),]
  df <- na.omit(df)
  
  # get bin-mean wind speed and count for each wind direction
  df.stats <- do.call(data.frame,
                      aggregate(cbind(WS.bin.WD.bin = WS) ~ WD.bin + WS.bin,
                                data = df,
                                FUN = function(x){
                                  x <- x[!is.na(x)]
                                  c(WS.mean = mean(x),
                                    count = length(x))
                                }))
  # get the weibull fit for each wind direction
  df.fit <- do.call(data.frame,
                    aggregate(cbind(wfit = WS) ~ WD.bin,
                              data = df,
                              FUN = function(x){
                                x <- x[!is.na(x)]
                                wfit = fitdistr(x,
                                                densfun = 'weibull',
                                                start = list(shape = 0.5, 
                                                             scale = mean(x)))
                                c(shape = wfit$estimate[["shape"]],
                                  scale = wfit$estimate[["scale"]],
                                  count = length(x))
                              }))
  df.fit$wfit.annotate = paste0("shape = ", format(df.fit$wfit.shape, digits = 3),
                                ",\n scale = ", format(df.fit$wfit.scale, digits = 3))
  # merge the weibull fit with the observations
  df.pred <- merge(df.stats,
                   df.fit)
  
  # predict the frequency 
  df.pred$f.pred <- dweibull(x = df.pred$WS.bin.WD.bin.WS.mean,
                             shape = df.pred$wfit.shape,
                             scale = df.pred$wfit.scale)
  # get the weights of the predicted frequency
  # df.pred <- merge(df.pred,
  #                  aggregate(cbind(f.pred.sum = f.pred) ~ WD.bin,
  #                            data = df.pred,
  #                            sum))
  
  # get the observed frequency
  df.pred$f.obs <- df.pred$WS.bin.WD.bin.count / df.pred$wfit.count
  
  # get the normalized prediction
  #df.pred$f.pred.norm <- df.pred$f.pred/df.pred$f.pred.sum
  
  # print
  p <- ggplot(data = df.pred) +
    geom_col(aes(x = as.numeric(as.character(WS.bin)),
                 y = f.obs)) +
    geom_line(aes(x = WS.bin.WD.bin.WS.mean,
                  y = f.pred),
              colour = "red") +
    geom_text(data = df.fit,
              x = 20,
              y = 0.2,
              hjust = 0,
              aes(label = wfit.annotate),
              size = 2.5,
              colour = "red") + 
    facet_wrap(~WD.bin,
               ncol = 3) +
    labs(title = "Weibull Fits to Wind Speed",
         subtitle = paste0("Wind speeds in the range ",
                           WS.min,
                           " m/s to ",
                           WS.max,
                           " m/s"),
         x = "Wind speed (m/s)",
         y = "Frequency",
         caption = caption)
  
}