FitWeibulltoWS <- function(x,
                           x.bin.edges,
                           x.bin.labels){
  
  # clean the input data
  x <- x[!is.na(x)]
  x.bin <- cut(x,
               breaks = x.bin.edges,
               labels = x.bin.labels,
               ordered_result = TRUE)
  
  x.in <- do.call(data.frame,
                  aggregate(cbind(x.bin = x) ~ x.bin,
                    data = data.frame(x = x,
                                      x.bin = x.bin),
                    FUN = function(x){c(mean = mean(x),
                                        count = length(x))}))
  
  x.in$f.obs.bin <- x.in$x.bin.count / sum(x.in$x.bin.count)
  
  # do the actual fit to the weibull
  wfit = fitdistr(x[!is.na(x)],
                  densfun = 'weibull',
                  start = list(shape = 2, 
                               scale = mean(x[!is.na(x)])))
  # get the predicted value
  x.in$f.pred.bin <- dweibull(x = x.in$x.bin.mean,
                              shape = wfit$estimate[["shape"]],
                              scale = wfit$estimate[["scale"]])
  # note that a density function does not sum to 1 (?)
  x.in$f.pred.bin.norm <- x.in$f.pred.bin/sum(x.in$f.pred.bin)
  
  return(data.frame(x.bin.label = x.in$x.bin,
                    x.bin.count = x.in$x.bin.count,
                    x.bin.mean = x.in$x.bin.mean,
                    f.obs.bin = x.in$f.obs.bin,
                    f.pred.bin = x.in$f.pred.bin.norm))
  
}