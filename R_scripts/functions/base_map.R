# script to get base mapof the northeast US

#library(mapdata)
#library(maps)
#library(ggplot2)
#library(dplyr)

base_map = function() {
  
  # 1. turn state line map into data frame
  states <- map_data("state")
  # 2. subest new england states
  new_england <- subset(states, region %in% c("vermont", "new hampshire", "connecticut", "maine", "rhode island", "massachusetts", "new york"))
  # 3. map using ggplot
  map <- ggplot() + geom_polygon(data = new_england, aes(x=long, y = lat, group = group, alpha=0.5))  +
    coord_fixed(1.3)
  return(map)
}

# calling base_map() will produce the map
