# Cluster Analysis #
library(data.table)

# Combining Scraper & OSM data
london <- data.frame(data_agg,osm_loc)
dim(london)


# Hierarchical Clustering
clust_tab <- london[,-c(1,2,6,9,11,25:30)] # removing non-relevant columns for clustering
hc.complete <- hclust(dist(clust_tab), method="complete")
plot(hc.complete ,main = "London Clustering", xlab="", sub="",cex=.9)
cluster <- cutree(hc.complete,16)
table(cluster)
data_after_clust <- cbind(london,cluster)

# First Iteration Score file
first_iter <- fread("First Iteration Score.csv")
names(first_iter)[3] <- "Score"
first_iter <- first_iter[,-c(1,2)]
data_score <- merge(y = data_after_clust,x = first_iter,by.y = "hotel_name",by.x = "Location",all.y = T)
table(data_score$cluster,data_score$Score) # All Scored assets fall in clusters 1 & 2

# Selecting Clusters 1 & 2 data and re-Clustering for better outcomes
data_score_1 <- data_score %>% filter(cluster %in% c(1,2))
clust_tab_1 <- data_score_1[,-c(1,2,3,7,10,12,26:31,56)] # removing non-relevant columns for clustering
hc.complete <- hclust(dist(clust_tab_1), method="complete")
plot(hc.complete ,main = "London Clustering Step 2", xlab="", sub="",cex=.9)
cluster_1 <- cutree(hc.complete,30)
table(cluster_1)
data_after_clust_1 <- cbind(data_score_1,cluster_1)
tmp <- data.frame(cbind(table(data_after_clust_1$cluster_1,data_after_clust_1$Score),(matrix(table(cluster_1)))))
potential_1 <- data.frame(cluster_id=seq(1:dim(tmp)[1]),not_relevant=tmp$X1,maybe_relevant=tmp$X2,relevant=tmp$X3,cluster_size=tmp$V4) %>% 
            mutate(.,purity=(maybe_relevant+relevant)/not_relevant,potential=cluster_size-relevant-maybe_relevant-not_relevant)
potential_1 <- potential_1 %>% filter(purity>0.5,potential>5) # Clusters 2,4,5,6,7,8,9 might be interesting

# Selecting Clusters 2,4,5,6,7,8,9 data and re-Clustering for better outcomes
data_score_2 <- data_score_1 %>% filter(cluster %in% c(1,2))




# Modeling - London #
library(data.table)
library(dplyr)
library(tidyverse)

install.packages("tmap")
library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggmap)
library(osmdata)


# Visualization

q <- getbb("London") %>% opq()
#our background map
london_map <- ggmap(get_googlemap(center = q,
                                  zoom = 11, scale = 2,
                                  maptype ='terrain',
                                  color = 'color'))

#final map
ggmap(london_map,zoom=1,scale=2)

+
  geom_sf(data=osmdata_sf(q)$osm_points,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)+
  labs(x="",y="")



# First get Milan coordinates
London <- c(0.1278,51.5074)
#Extract the map from Google map at a specific zoom (this is up to you!)
mapMilan <- get_map(location=London, zoom=10,source = "osm")
#Visualize the map
ggmap(mapMilan)
# Add fill layer to nz shape
tm_shape(nc) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 



# Adding labels by initial recipe - MR
london_tab$relevant <- ifelse((london_tab$num_of_rooms <= 300 &
                                 london_tab$num_of_rooms >= 50) &
                                (london_tab$rest_bar_lounge == 1) &
                                #(london_tab$meeting_room == 1) &
                                (london_tab$guest_rating_score <= 8.5),1,0)
#(london_tab$pool == 1) no assets meet joint terms

# Merging with scraper data
dim(london_tab)
dim(london_osm)
full_table <- data.frame(london_tab,london_osm)
write.csv(full_table,"full_table_before_clust")

# Clustering
clust_tab <- full_table[,-c(1,2,6,10,11,12,13,14,16,26)]
hc.complete <- hclust(dist(clust_tab), method="complete")
plot(hc.complete ,main = "Complete Linkage", xlab="", sub="",cex=.9)
cluster <- cutree(hc.complete,3)
table(cluster)

full_table[full_table$relevant==1,]

full_after_clust <- data.frame(full_table,cluster)
full_after_clust[full_after_clust$relevant==1,]
table(full_after_clust[full_after_clust$relevant==1,]$cluster)

write.csv(full_after_clust,"full_table_after_clust")


full_relevant <- full_after_clust[full_after_clust$relevant==1,-c(2,10,11,12,13,14,26)]
full_relevant[,c(1,40)]

initial_tab <- full_after_clust %>% filter(cluster %in% c(1,2,19))
initial_tab <- initial_tab[-c(214:217),]

write.csv(initial_tab,"initial_leads_1.csv")

# # Adding unique vars
# selina_tab$relevant <- 1
# selina_tab <- data.frame(selina_tab,osm_loc)
# full_table <- rbind(full_table,selina_tab)


# Towards BAR
london_osm <- fread("london_osm.csv")
full_after_clust <- fread("full_table_after_clust")

dim(full_after_clust[full_after_clust$num_of_rooms>120,])

# Cluster analysis
first_clust <- full_after_clust %>% filter(cluster==1)
second_clust <- full_after_clust %>% filter(cluster==2)
third_clust <- full_after_clust %>% filter(cluster==3)

table(first_clust$relevant)
table(second_clust$relevant)
table(third_clust$relevant)


sort(table(first_clust$city_sector))
table(third_clust$volume)/nrow(third_clust)
table(third_clust$guest_rating_verbal)/nrow(third_clust)
table(third_clust$wifi)[2]/nrow(third_clust)
table(third_clust$gym)[2]/nrow(third_clust)
table(third_clust$meeting_room)[2]/nrow(third_clust)
table(third_clust$ac)[2]/nrow(third_clust)
table(third_clust$laundry)[2]/nrow(third_clust)
table(third_clust$rest_bar_lounge)[2]/nrow(third_clust)
table(third_clust$pool)[2]/nrow(third_clust)
table(third_clust$parking)[2]/nrow(third_clust)
table(third_clust$casino)[2]/nrow(third_clust)
table(third_clust$tattoo)[2]/nrow(third_clust)

hist(third_clust$miles_to_city_center)
mean(na.omit(third_clust$miles_to_city_center))
median(na.omit(third_clust$miles_to_city_center))
range(na.omit(third_clust$miles_to_city_center))

hist(third_clust$guest_rating_score)
mean(na.omit(third_clust$guest_rating_score))
median(na.omit(third_clust$guest_rating_score))
range(na.omit(third_clust$guest_rating_score))

hist(third_clust$num_of_rooms)
mean(na.omit(third_clust$num_of_rooms))
median(na.omit(third_clust$num_of_rooms))
range(na.omit(third_clust$num_of_rooms))

hist(third_clust$star_rating)
mean(na.omit(third_clust$star_rating))
median(na.omit(third_clust$star_rating))
range(na.omit(third_clust$star_rating))

hist(third_clust$price_for_period_USD)
mean(na.omit(third_clust$price_for_period_USD))
median(na.omit(third_clust$price_for_period_USD))
range(na.omit(third_clust$price_for_period_USD))

hist(third_clust$num_of_hotel_com_reviews)
mean(na.omit(third_clust$num_of_hotel_com_reviews))
median(na.omit(third_clust$num_of_hotel_com_reviews))
range(na.omit(third_clust$num_of_hotel_com_reviews))

hist(third_clust$trip_advisor_rating)
mean(na.omit(third_clust$trip_advisor_rating))
median(na.omit(third_clust$trip_advisor_rating))
range(na.omit(third_clust$trip_advisor_rating))

hist(third_clust$restaurants)
mean(na.omit(third_clust$restaurants))
median(na.omit(third_clust$restaurants))
range(na.omit(third_clust$restaurants))

hist(third_clust$bars)
mean(na.omit(third_clust$bars))
median(na.omit(third_clust$bars))
range(na.omit(third_clust$bars))

hist(third_clust$cafe)
mean(na.omit(third_clust$cafe))
median(na.omit(third_clust$cafe))
range(na.omit(third_clust$cafe))

hist(third_clust$nightclub)
mean(na.omit(third_clust$nightclub))
median(na.omit(third_clust$nightclub))
range(na.omit(third_clust$nightclub))

hist(third_clust$pharmacy)
mean(na.omit(third_clust$pharmacy))
median(na.omit(third_clust$pharmacy))
range(na.omit(third_clust$pharmacy))

hist(second_clust$theatre)
mean(na.omit(second_clust$theatre))
median(na.omit(second_clust$theatre))
range(na.omit(second_clust$theatre))

hist(third_clust$university)
mean(na.omit(third_clust$university))
median(na.omit(third_clust$university))
range(na.omit(third_clust$university))

hist(third_clust$antiques)
mean(na.omit(third_clust$antiques))
median(na.omit(third_clust$antiques))
range(na.omit(third_clust$antiques))

hist(third_clust$bakery)
mean(na.omit(third_clust$bakery))
median(na.omit(third_clust$bakery))
range(na.omit(third_clust$bakery))

hist(third_clust$art)
mean(na.omit(third_clust$art))
median(na.omit(third_clust$art))
range(na.omit(third_clust$art))

hist(third_clust$cinema)
mean(na.omit(third_clust$cinema))
median(na.omit(third_clust$cinema))
range(na.omit(third_clust$cinema))

hist(third_clust$library)
mean(na.omit(third_clust$library))
median(na.omit(third_clust$library))
range(na.omit(third_clust$library))

hist(third_clust$marketplace)
mean(na.omit(third_clust$marketplace))
median(na.omit(third_clust$marketplace))
range(na.omit(third_clust$marketplace))

hist(third_clust$hotels)
mean(na.omit(third_clust$hotels))
median(na.omit(third_clust$hotels))
range(na.omit(third_clust$hotels))

hist(third_clust$railway_station)
mean(na.omit(third_clust$railway_station))
median(na.omit(third_clust$railway_station))
range(na.omit(third_clust$railway_station))

hist(third_clust$subway)
mean(na.omit(third_clust$subway))
median(na.omit(third_clust$subway))
range(na.omit(third_clust$subway))

PAM <- pam(fis)