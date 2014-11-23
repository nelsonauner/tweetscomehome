# Here, we write a function that takes in latitude, longitude and spits out the closest postal code:

require(dplyr); require(magrittr)

houseID <- read.csv('~/Home/LondonData/london2009-2014-house-prices/NPSL_London100km.csv',
                    stringsAsFactors=FALSE)
housed <- read.csv('~/Home/LondonData/london2009-2014-house-prices/Houseprice_2009_100km_London.csv')
housed$pdistrict <- housed$Postcode %>% substr(1,3) %>% factor
houseID$pdisrict <- houseID$Pcd2 %>% substr(1,3) %>% factor

crossT <- houseID %>% select(pdisrict,Latitude,Longitude)
ct <- crossT %>% group_by(pdisrict) %>% summarise(latitude = mean(Latitude),longitude = mean(Longitude))

findPostalCode <- function(long,lat,crossT) {
  # Input: longitude (scalar) and latitude (scalar)
  # crossT: should have centers and postal code. 
  long %>% c(lat) %>% print
  dist <- ct %>% with((latitude-lat)^2 + (longitude-long)^2)
  ct[which.min(dist),1] %>% as.character %>% return
}

# Test case:  findPostalCode(-.2874151,51.75313,ct) -> AL1

