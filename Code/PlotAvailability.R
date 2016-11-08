PlotAvailability <- function(df.in,
                            caption){
  
  require(ggplot2)
  require(lubridate)
  
  # get the count of data in each year and month
  df.avail <- aggregate(cbind(count = WS80) ~ n.rows + month.b + year,
                        data = df.in,
                        FUN = function(x){
                          length(x[!is.na(x)])
                        })
  # get the number of days in that month
  df.avail$n.days.per.month <- days_in_month(as.Date(paste(df.avail$year,
                                                        df.avail$month.b,
                                                        "01"),
                                                  "%Y %b %d"))
  # convert to number of samples
  df.avail$n.10min.per.month <- df.avail$n.days.per.month * 24 *6
  df.avail$f <- df.avail$count / df.avail$n.10min.per.month * 100
  
  # plot the results
  p <- ggplot(data = df.avail,
              aes(x = month.b,
                  y = f,
                  fill = n.rows)) +
    geom_bar(stat="identity") + 
    facet_wrap(~ year,
               ncol = 3) +
    guides(fill = guide_legend(reverse = TRUE))+ 
    scale_fill_continuous(name = "N. data points /\n10-min. interval")+
    labs(caption = caption,
         y = "Availability [% of max possible]") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}
