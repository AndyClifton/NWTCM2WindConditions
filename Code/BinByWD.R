BinByWD <- function(WD,
                    dWD,
                    sep = " - ",
                    useDegs = TRUE){
  # first wrap the data to -dWD to 360-dWD/2
  i.wrap <- which(WD>(360-(dWD/2)))
  if (any(i.wrap, na.rm = TRUE)){
    WD[i.wrap] <- WD[i.wrap] - 360
  }
  i.wrap <- which(WD<(-dWD/2))
  if (any(i.wrap, na.rm = TRUE)){
    WD[i.wrap] <- WD[i.wrap] + 360
  }
  
  # now cut
  cut(WD,
      breaks = seq(-dWD/2,
                   360-dWD/2,
                   dWD),
      labels = c(paste0(360-dWD/2, 
                        if(useDegs){"°"},
                        sep,
                        dWD/2,
                        if(useDegs){"°"}),
                 paste0(seq(from = dWD/2,
                            to = 360-(3*dWD/2),
                            by = dWD), 
                        if(useDegs){"°"},
                        sep,
                        seq(3*dWD/2,
                            360-(dWD/2),
                            dWD),
                        if(useDegs){"°"})),
      ordered_result = TRUE)
}