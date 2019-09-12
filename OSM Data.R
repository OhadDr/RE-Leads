## OPEN STRRET MAP ##

library(osmar)
library(osmdata)
library(sf)
library(ggmap)


#src <- osmsource_api()

restaurants <- numeric()
bars <- numeric()
cafe <- numeric()
college <- numeric()
casino <- numeric() 
parking <- numeric()
nightclub <- numeric()
ice_cream <- numeric()
pharmacy <- numeric()
pub <- numeric()
theatre <- numeric()
university <- numeric()
alcohol <- numeric()
antiques <- numeric()
art <- numeric()
bakery <- numeric()
camera <- numeric()
fashion <- numeric()
second_hand <- numeric()
spices <- numeric()
tattoo <- numeric()
travel_agency <- numeric()
fuel <- numeric()
cinema <- numeric()
library <- numeric()
marketplace <- numeric()
railway_station <- numeric()
public_transport <- numeric()
subway <- numeric()
hotels <- numeric()
countmap <- 0
for (i in 1:length(data_agg$hotel_coordinates)) {
  lat <- str_extract(data_agg$hotel_coordinates[i],"(.*)[,]") %>% 
    gsub(",","",.) %>% 
    as.numeric()
  lon <- str_extract(data_agg$hotel_coordinates[i],"[,](.*)") %>% 
    gsub(",","",.) %>% 
    as.numeric()
  
  bb <- matrix(center_bbox(lon, lat, 1000, 1000),2,2)
  rownames(bb) <- c("x","y")
  colnames(bb) <- c("min","max")
  
  restaurant_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "restaurant")
  restaurants[i] <- length(osmdata_sf(restaurant_bb)$osm_points[[1]])
  bars_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "bar")
  bars[i] <- length(osmdata_sf(bars_bb)$osm_points[[1]])
  cafe_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "cafe")
  cafe[i] <- length(osmdata_sf(cafe_bb)$osm_points[[1]])
  cinema_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "cinema")
  cinema[i] <- length(osmdata_sf(cinema_bb)$osm_points[[1]])
  college_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "college")
  college[i] <- length(osmdata_sf(college_bb)$osm_points[[1]])
  casino_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "casino")
  casino[i] <- length(osmdata_sf(casino_bb)$osm_points[[1]])
  # fuel_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "fuel")
  # fuel[i] <- length(osmdata_sf(fuel_bb)$osm_points[[1]])
  library_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "library")
  library[i] <- length(osmdata_sf(library_bb)$osm_points[[1]])
  # parking_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "parking")
  # parking[i] <- length(osmdata_sf(parking_bb)$osm_points[[1]])
  marketplace_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "marketplace")
  marketplace[i] <- length(osmdata_sf(marketplace_bb)$osm_points[[1]])
  nightclub_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "nightclub")
  nightclub[i] <- length(osmdata_sf(nightclub_bb)$osm_points[[1]])
  # ice_cream_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "ice cream")
  # ice_cream[i] <- length(osmdata_sf(ice_cream_bb)$osm_points[[1]])
  pharmacy_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "pharmacy")
  pharmacy[i] <- length(osmdata_sf(pharmacy_bb)$osm_points[[1]])
  # pub_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "pub")
  # pub[i]<- length(osmdata_sf(pub_bb)$osm_points[[1]])
  theatre_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "theatre")
  theatre[i] <- length(osmdata_sf(theatre_bb)$osm_points[[1]])
  university_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "university")
  university[i] <- length(osmdata_sf(university_bb)$osm_points[[1]])
  # alcohol_shop_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "alcohol")
  # alcohol[i] <- length(osmdata_sf(alcohol_shop_bb)$osm_points[[1]])
  antiques_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "antiques")
  antiques[i] <- length(osmdata_sf(antiques_bb)$osm_points[[1]])
  art_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "art")
  art[i] <- length(osmdata_sf(art_bb)$osm_points[[1]])
  bakery_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "bakery")
  bakery[i] <- length(osmdata_sf(bakery_bb)$osm_points[[1]])
  # camera_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "camera")
  # camera[i] <- length(osmdata_sf(camera_bb)$osm_points[[1]])
  # fashion_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "fashion")
  # fashion[i] <- length(osmdata_sf(fashion_bb)$osm_points[[1]])
  # second_hand_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "second hand")
  # second_hand[i] <- length(osmdata_sf(second_hand_bb)$osm_points[[1]])
  # spices_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "spices")
  # spices[i] <- length(osmdata_sf(spices_bb)$osm_points[[1]])
  tattoo_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "tattoo")
  tattoo[i] <- length(osmdata_sf(tattoo_bb)$osm_points[[1]])
  # travel_agency_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "travel agency")
  # travel_agency[i] <- length(osmdata_sf(travel_agency_bb)$osm_points[[1]])
  subway_bb <- bb %>% opq() %>% add_osm_feature(key = "railway", value = "subway")
  subway[i] <- length(osmdata_sf(art_bb)$osm_points[[1]])
  railway_station_bb <- bb %>% opq() %>% add_osm_feature(key = "railway", value = "station")
  railway_station[i] <- length(osmdata_sf(railway_station_bb)$osm_points[[1]])
  # public_transport_bb <- bb %>% opq() %>% add_osm_feature(key = "public transport", value = "station")
  # public_transport[i] <- length(osmdata_sf(public_transport_bb)$osm_points[[1]])
  hotels_bb <- bb %>% opq() %>% add_osm_feature(key = "tourism", value = "hotel")
  hotels[i] <- length(osmdata_sf(hotels_bb)$osm_points[[1]])
  countmap <- countmap+1
  print(paste0(countmap,"/",length(data_agg$hotel_coordinates)," Completed - ",Sys.time()))
}   

params_loc_osm <- list(restaurants,
                       bars,
                       cafe,
                       college,
                       casino,
                       # parking,
                       nightclub,
                       # ice_cream,
                       pharmacy,
                       # pub,
                       theatre,
                       university,
                       # alcohol,
                       antiques,
                       art,
                       bakery,
                       # camera,
                       # fashion,
                       # second_hand,
                       # spices,
                       tattoo,
                       # travel_agency,
                       # fuel,
                       cinema,
                       library,
                       marketplace,
                       railway_station,
                       # public_transport,
                       subway,
                       hotels)
for (i in params_loc_osm){
  print(length(i))
}

osm_loc <- data.frame(restaurants,
                      bars,
                      cafe,
                      college,
                      casino,
                      # parking,
                      nightclub,
                      # ice_cream,
                      pharmacy,
                      # pub,
                      theatre,
                      university,
                      # alcohol,
                      antiques,
                      art,
                      bakery,
                      # camera,
                      # fashion,
                      # second_hand,
                      # spices,
                      tattoo,
                      # travel_agency,
                      # fuel,
                      cinema,
                      library,
                      marketplace,
                      railway_station,
                      # public_transport,
                      subway,
                      hotels)


# Adding geo-engineered variables
volume_cont <- apply(osm_loc,MARGIN = 1,FUN = sum) # sum of each row
hist(volume_cont)
boxplot(volume_cont)
osm_loc$volume <- ifelse(volume_cont <= 300, "Normal",
                         ifelse(volume_cont <= 800,"High",
                                ifelse(volume_cont <= 2000,"Very High","Massive")))

write.csv(osm_loc,"osm_dta.csv")






# # Adding Selina's locations
# 
# restaurants <- numeric()
# bars <- numeric()
# cafe <- numeric()
# college <- numeric()
# casino <- numeric()
# parking <- numeric()
# nightclub <- numeric()
# ice_cream <- numeric()
# pharmacy <- numeric()
# pub <- numeric()
# theatre <- numeric()
# university <- numeric()
# alcohol <- numeric()
# antiques <- numeric()
# art <- numeric()
# bakery <- numeric()
# camera <- numeric()
# fashion <- numeric()
# second_hand <- numeric()
# spices <- numeric()
# tattoo <- numeric()
# travel_agency <- numeric()
# fuel <- numeric()
# cinema <- numeric()
# library <- numeric()
# marketplace <- numeric()
# railway_station <- numeric()
# public_transport <- numeric()
# subway <- numeric()
# hotels <- numeric()
# countmap <- 0
# for (i in 1:length(selina_tab$hotel_coordinates)) {
#   lat <- str_extract(selina_tab$hotel_coordinates[i],"(.*)[,]") %>% 
#     gsub(",","",.) %>% 
#     as.numeric()
#   lon <- str_extract(selina_tab$hotel_coordinates[i],"[,](.*)") %>% 
#     gsub(",","",.) %>% 
#     as.numeric()
#   
#   bb <- matrix(center_bbox(lon, lat, 1000, 1000),2,2)
#   rownames(bb) <- c("x","y")
#   colnames(bb) <- c("min","max")
#   
#   restaurant_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "restaurant")
#   restaurants[i] <- length(osmdata_sf(restaurant_bb)$osm_points[[1]])
#   bars_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "bar")
#   bars[i] <- length(osmdata_sf(bars_bb)$osm_points[[1]])
#   cafe_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "cafe")
#   cafe[i] <- length(osmdata_sf(cafe_bb)$osm_points[[1]])
#   cinema_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "cinema")
#   cinema[i] <- length(osmdata_sf(cinema_bb)$osm_points[[1]])
#   college_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "college")
#   college[i] <- length(osmdata_sf(college_bb)$osm_points[[1]])
#   casino_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "casino")
#   casino[i] <- length(osmdata_sf(casino_bb)$osm_points[[1]])
#   # fuel_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "fuel")
#   # fuel[i] <- length(osmdata_sf(fuel_bb)$osm_points[[1]])
#   library_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "library")
#   library[i] <- length(osmdata_sf(library_bb)$osm_points[[1]])
#   # parking_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "parking")
#   # parking[i] <- length(osmdata_sf(parking_bb)$osm_points[[1]])
#   marketplace_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "marketplace")
#   marketplace[i] <- length(osmdata_sf(marketplace_bb)$osm_points[[1]])
#   nightclub_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "nightclub")
#   nightclub[i] <- length(osmdata_sf(nightclub_bb)$osm_points[[1]])
#   # ice_cream_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "ice cream")
#   # ice_cream[i] <- length(osmdata_sf(ice_cream_bb)$osm_points[[1]])
#   pharmacy_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "pharmacy")
#   pharmacy[i] <- length(osmdata_sf(pharmacy_bb)$osm_points[[1]])
#   # pub_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "pub")
#   # pub[i]<- length(osmdata_sf(pub_bb)$osm_points[[1]])
#   theatre_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "theatre")
#   theatre[i] <- length(osmdata_sf(theatre_bb)$osm_points[[1]])
#   university_bb <- bb %>% opq() %>% add_osm_feature(key = "amenity", value = "university")
#   university[i] <- length(osmdata_sf(university_bb)$osm_points[[1]])
#   # alcohol_shop_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "alcohol")
#   # alcohol[i] <- length(osmdata_sf(alcohol_shop_bb)$osm_points[[1]])
#   antiques_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "antiques")
#   antiques[i] <- length(osmdata_sf(antiques_bb)$osm_points[[1]])
#   art_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "art")
#   art[i] <- length(osmdata_sf(art_bb)$osm_points[[1]])
#   bakery_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "bakery")
#   bakery[i] <- length(osmdata_sf(bakery_bb)$osm_points[[1]])
#   # camera_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "camera")
#   # camera[i] <- length(osmdata_sf(camera_bb)$osm_points[[1]])
#   # fashion_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "fashion")
#   # fashion[i] <- length(osmdata_sf(fashion_bb)$osm_points[[1]])
#   # second_hand_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "second hand")
#   # second_hand[i] <- length(osmdata_sf(second_hand_bb)$osm_points[[1]])
#   # spices_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "spices")
#   # spices[i] <- length(osmdata_sf(spices_bb)$osm_points[[1]])
#   tattoo_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "tattoo")
#   tattoo[i] <- length(osmdata_sf(tattoo_bb)$osm_points[[1]])
#   # travel_agency_bb <- bb %>% opq() %>% add_osm_feature(key = "shop", value = "travel agency")
#   # travel_agency[i] <- length(osmdata_sf(travel_agency_bb)$osm_points[[1]])
#   subway_bb <- bb %>% opq() %>% add_osm_feature(key = "railway", value = "subway")
#   subway[i] <- length(osmdata_sf(art_bb)$osm_points[[1]])
#   railway_station_bb <- bb %>% opq() %>% add_osm_feature(key = "railway", value = "station")
#   railway_station[i] <- length(osmdata_sf(railway_station_bb)$osm_points[[1]])
#   # public_transport_bb <- bb %>% opq() %>% add_osm_feature(key = "public transport", value = "station")
#   # public_transport[i] <- length(osmdata_sf(public_transport_bb)$osm_points[[1]])
#   hotels_bb <- bb %>% opq() %>% add_osm_feature(key = "tourism", value = "hotel")
#   hotels[i] <- length(osmdata_sf(hotels_bb)$osm_points[[1]])
#   countmap <- countmap+1
#   print(paste0(countmap,"/",length(selina_tab$hotel_coordinates)," Completed"))
# }   
# 
# osm_loc <- data.frame(restaurants,
#                       bars,
#                       cafe,
#                       college,
#                       casino,
#                       # parking,
#                       nightclub,
#                       # ice_cream,
#                       pharmacy,
#                       # pub,
#                       theatre,
#                       university,
#                       # alcohol,
#                       antiques,
#                       art,
#                       bakery,
#                       # camera,
#                       # fashion,
#                       # second_hand,
#                       # spices,
#                       tattoo,
#                       # travel_agency,
#                       # fuel,
#                       cinema,
#                       library,
#                       marketplace,
#                       railway_station,
#                       # public_transport,
#                       subway,
#                       hotels)
# 
# # london_osm <- osm_loc
# write.csv(london_osm,"london_osm.csv")
# volume_cont <- apply(osm_loc,MARGIN = 1,FUN = sum) # sum of each row
# hist(volume_cont)
# osm_loc$volume <- ifelse(volume_cont <= 250, "Normal",
#                          ifelse(volume_cont <= 750,"High","Very High"))
# ifelse(volume_cont <= 3000,"Very High","Massive")))
# 
# 
# 
# 
# 
# 
# 
# library(httr)
# library(jsonlite)
# library(rlist)
# library(dplyr)
# library(osmdata)
# library(sf)
# library(ggmap)
# library(tidyverse)
# 
# 
# # features of osm
# available_features()
# 
# # amenities (for example)
# available_tags("railway")
# 
# # shop (for example)
# available_tags("amenity")
# available_tags("fuel")
# 
# 
# #building the query
# q <- getbb(place_name = "Boston, USA") %>%
#   opq() %>%
#   add_osm_feature(key = "amenity", value = "marketplace")
# str(q)
# 
# marketplacee <- osmdata_sf(q)
# marketplacee
# 
# 
# q <- getbb(place_name = "London") %>%
#   opq() %>%
#   add_osm_feature(key = "shop", value = "antiques")
# str(q)
# 
# antiquess <- osmdata_sf(q)$osm_points
# antiquess
# 
# #our background map
# med_map <- get_map(bbb,maptype = "toner-background")
# 
# #final map
# ggmap(med_map) +
#   geom_sf(data=osmdata_sf(q)$osm_points,
#           inherit.aes =FALSE,
#           colour="#238443",
#           fill="#004529",
#           alpha=.5,
#           size=4,
#           shape=21)+
#   labs(x="",y="")
# 
# marketplacee$osm_points
