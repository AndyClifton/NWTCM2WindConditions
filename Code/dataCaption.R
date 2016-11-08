dataCaption <- function(date.start,
                        date.stop,
                        tz = 'Etc/GMT+7',
                        project.who){

nowstr <- format(with_tz(Sys.time(), 
                         tzone=tz),
                 "%d %B %Y %H:%M")

paste0("Data from ", format(date.start,"%d %B %Y"),
       " to ", format(date.stop,"%d %B %Y"),
       "\nCreated ", nowstr, 
       " by ", project.who)
}