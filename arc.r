source("./linear-algebra.R")

destination <- function(p, brng, dist) {
  dist = dist/6371
  brng = radians(brng)
  lat1 = radians(p[2])
  lon1 = radians(p[1])
  lat2 = asin(sin(lat1)*cos(dist)+cos(lat1)*sin(dist)*cos(brng))
  lon2 = lon1 + atan2(sin(brng)*sin(dist)*cos(lat1), cos(dist)-sin(lat1)*sin(lat2));
  return (c(degrees(lon2), degrees(lat2)))
}

arc <- function(midLoc,distance,angle.start,angle.end,vertices){
  
  result  <- NA
  if(angle.end - angle.start > 0){
    start <- angle.start
    stop <- angle.end
  } else {
    start <- angle.end
    stop <- angle.start    
  }
  segment <- (stop-start)/vertices
  
  # 1st to 2nd last point
  i <- start
  while (i<stop) {
    des    <- destination(midLoc, i, distance/1000)
    fp     <- data.frame(lon=NA, lat=NA)
    fp$lon <- des[1]
    fp$lat <- des[2]
    i      <- i+segment
    result <- rbind(fp, result)
  }
  # add last point
  des    <- destination(midLoc, stop, distance/1000)
  fp     <- data.frame(lon=NA, lat=NA)
  fp$lon <- des[1]
  fp$lat <- des[2]
  result <- rbind(fp, result)  
  # remove NAs
  result <- subset(result, !is.na(result$lat))
  return (result)
  
}