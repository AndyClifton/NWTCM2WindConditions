GetUVfromWSWD <- function(speed = NULL,
                          dir = NULL){
  # +u is wind to the east
  # +v is wind to the north
  data.frame("u" = -speed*sin(2*pi*(dir/360)), 
             "v" = -speed*cos(2*pi*(dir/360)))
}

GetWSWDfromUV <- function(u = NULL,
                          v = NULL){
  
  data.frame("speed" = (u^2 + v^2)^(1/2), 
             "dir" = (atan(u/v)* 180/pi) + ifelse(v>0,180,ifelse(u>0,360,0)))
}