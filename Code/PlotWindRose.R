PlotWindrose <- function(data,
                         spd,
                         dir,
                         spdres = 2,
                         dirres = 30,
                         spdmin = 2,
                         spdmax = 20,
                         spdseq = NULL,
                         palette = "YlGnBu",
                         countmax = NA,
                         debug = 0){
  requireNamespace("ggplot2")
  requireNamespace("RColorBrewer")
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (missing("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  data <- na.omit(data)
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(colors = brewer.pal(min(max(3,
                                                             n.colors.in.range),
                                                         min(9,
                                                             n.colors.in.range)),                                               
                                                     palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          format(max(data[[spd]],na.rm = TRUE),
                                 digits = 3)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  
  # Tidy up data again; this removes cases where wind speeds were less than spdmin ----
  data <- na.omit(data)
  
  # assign each wind direction to a bin
  data$dir.binned <- BinByWD(data[[dir]],
                             dWD = dirres,
                             sep = "-",
                             useDegs = FALSE)
    
  # Run debug if required ----
  if (debug>0){    
    cat(levels(data$dir.binned),"\n")
    cat(speedcuts.colors, "\n")
    cat(data[1:10,])
  }  
  
  # note that ggplot changed bar orders in 2.1
  if(package_version(packageVersion("ggplot2"))>"2.1"){
    # need to reorder wind speed groups
    data$spd.binned <- factor(data$spd.binned,
                                levels=rev(levels(data$spd.binned)))
    spd.colors <- rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar(width = 0.8) + 
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE,
                      na.translate = FALSE) +
    theme(axis.title.x = element_blank())
    
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  #print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}