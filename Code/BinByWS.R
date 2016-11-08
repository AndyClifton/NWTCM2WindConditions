BinByWS <- function(WS,
                    dWS,
                    WS.min,
                    WS.max,
                    sep = " - "){
  
  spdseq <- seq(WS.min,WS.max,dWS)
  
  n.spd.seq <- length(spdseq)
  
  if (max(WS,na.rm = TRUE) > WS.max){    
    spd.breaks <- c(spdseq,
                    max(WS,na.rm = TRUE))
    if (is.na(sep)){
      spd.labels <- c(spdseq[1:n.spd.seq-1]+dWS/2,
                      paste0("> ", spdseq[n.spd.seq]))
    } else {
      spd.labels <- c(paste0(c(spdseq[1:n.spd.seq-1]),
                             sep,
                             c(spdseq[2:n.spd.seq])),
                      paste0(WS.max,
                             sep,
                             format(max(WS,na.rm = TRUE),
                                    digits = 3)))
    }
  } else{
    spd.breaks <- spdseq
    if (is.na(sep)){
      spd.labels <- c(spdseq[1:n.spd.seq-1] + dWS/2)
    } else {
      spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq]))    
    }
  }
  cut(x = WS,
      breaks = spd.breaks,
      labels = spd.labels,
      ordered_result = TRUE) 
}