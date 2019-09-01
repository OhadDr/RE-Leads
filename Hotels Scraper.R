## Scraping Hotels.com ##

library(rvest)
library(jsonlite)
library(tidyverse)
library(stringr)
library(RSelenium)


# Sleep function for crawler
wait <- function(periods = c(1,1.5)){
  tic <- runif(1,periods[1],periods[2])
  cat(paste0(Sys.time()), "- Sleeping for ",round(tic,2), "seconds\n")
  Sys.sleep(tic)
}
wait()

# Docker commands - need to run in cmd #
# We need to make sure that our image version support our chrome browser version, therfore we need to pull the right image
# docker run --name chrome -d -p 4444:4444 -p 5900:5900 selenium/standalone-chrome-debug - creating container
# To set up client in VNC need to insert (in connection properties) 127.0.0.1:5901 - local host & external port for client, the password is "secret"
# docker ps - showing running containers only
# docker ps -a - showing all containers
# docker stop 'name of container' - stop container
# docker rm 'name of container' - remove container

# Estimated Running Time for one session:
# Scroll-down - 10 minutes
# Main Bulletin Scrape - 4 minutes 1,854 assets
# Location Details Scrape - 46 minutes


rD$server$stop() # Killing server port can now be used again 
rD <- rsDriver(remoteServerAddr = "127.0.0.1",
               port = 4445L,
               browser = "chrome",
               version = "3.141.59",# selenium version
               chromever = "76.0.3809.25") # chrome version
remDr <- rD$client
# remDr <- remoteDriver(remoteServerAddr = "127.0.0.1",port = 4445L,browser = "chrome")
# remDr$open() # opens client, meaning the chrome browser or VNC browser

# Navigating to desirable website
remDr$open()
url_hotels <- "https://www.hotels.com/search.do?resolved-location=CITY%3A549499%3AUNKNOWN%3AUNKNOWN&destination-id=549499&q-destination=London,%20England,%20United%20Kingdom&q-check-in=2019-11-18&q-check-out=2019-11-19&q-rooms=1&q-room-0-adults=2&q-room-0-children=0"
url_hotels <- paste0(url_hotels,"&locale=en_US") # avoiding hebrew results
remDr$navigate(url_hotels)

# scroll down n times, waiting for the page to load at each time, setting sleep time to avoid DDOS misclassification
for(i in 1:300){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  cat('Scrolling down',i,"\n")
  wait(periods = c(1,2))
}

# URL Hotels.com
page_source <- remDr$getPageSource()
webpage <- read_html(page_source[[1]])

# # Check-in & Check-out dates, group parameters
checkin <-str_match(url_hotels,pattern = "check-in=[0-9]*-[0-9]*-[0-9]*") %>% substr(.,10,nchar(.)) %>%  as.character()
checkout <- str_match(url_hotels,pattern = "check-out=[0-9]*-[0-9]*-[0-9]*") %>% substr(.,11,nchar(.)) %>% as.character()
q_rooms <- str_match(url_hotels,pattern = "rooms=[0-9]*") %>% str_extract(.,"[0-9*]") %>% as.numeric()
q_adults <- str_match(url_hotels,pattern = "adults=[0-9]*") %>% str_extract(.,"[0-9*]") %>% as.numeric()
q_children <- str_match(url_hotels,pattern = "children=[0-9]*") %>% str_extract(.,"[0-9*]") %>% as.numeric()


# Scraping main bulletin
hotels_wrap <- html_nodes(webpage,'.hotel-wrap')
exclude_hotels_index <- grep(pattern = "sold out",x = hotels_wrap)
hotels_wrap <- hotels_wrap[-exclude_hotels_index]

# Loop for inserting NA's where node is absent
hotel_name_html <- character()
address_html <- character()
sector_html <- character()
city_center <- numeric()
central_station <- numeric()
guest_rating_score <- numeric()
guest_rating_verbal <- character()
hotel_com_reviews_html <- numeric()
ta_rating_html <- character()
price_for_period_html <- character()
countime <- 0

for (j in 1877:length(hotels_wrap)){
  
  # Scraping fields from main bulletin
  hotel_name_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.p-name')) > 0,
                               html_nodes(hotels_wrap[j],'.p-name') %>% html_text(),
                               NA)
  
  address_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.contact')) > 0,
                            html_nodes(hotels_wrap[j],'.contact') %>% html_text(),
                            NA)
  
  sector_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.map-link.xs-welcome-rewards')) > 0,
                           html_nodes(hotels_wrap[j],'.map-link.xs-welcome-rewards') %>% html_text(),
                           NA)
  
  city_center[j] <- ifelse(length(html_nodes(hotels_wrap[1877],'.property-landmarks li')) > 0,
                           html_nodes(hotels_wrap[j],'.property-landmarks li')[1] %>% html_text() %>% 
                           gsub(" miles to City center","",.) %>% 
                           as.numeric(),
                           NA)

  guest_rating_html <- ifelse(length(html_nodes(hotels_wrap[j],'.guest-reviews-badge')) > 0,
                              html_nodes(hotels_wrap[j],'.guest-reviews-badge') %>% html_text(),
                              NA)
  guest_rating_score[j] <- substr(guest_rating_html,nchar(guest_rating_html)-3,nchar(guest_rating_html)) %>% gsub(" ","",.) %>% as.numeric()
  guest_rating_verbal[j] <- substr(guest_rating_html,1,nchar(guest_rating_html)-4) %>% gsub("l ","l",.)
  
  hotel_com_reviews_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.small-view')) > 0,
                                      html_nodes(hotels_wrap[j],'.small-view') %>% html_text() %>% 
                                        gsub(" reviews","",.) %>%
                                        gsub(",","",.) %>% 
                                        as.numeric(),
                                      NA)
  
  ta_rating_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.ta-logo')) > 0,
                              html_nodes(hotels_wrap[j],'.ta-logo') %>% html_text() %>% 
                                gsub("TripAdvisor Traveller Rating: ","",.) %>% 
                                as.numeric(),
                              NA)
  
  price_for_period_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.price')) > 0,
                                     html_nodes(hotels_wrap[j],'.price') %>% html_text() %>% 
                                       str_extract(.,pattern = '[$][0-9]*[$][0-9]*|[$][0-9]*') %>% 
                                       substr(.,2,nchar(.)) %>% 
                                       gsub(pattern = "[0-9]*[$]","",.) %>% 
                                       as.numeric(),
                                     NA)
  countime <- countime+1
  print(paste0(countime,"/",length(hotels_wrap)," Completed - ",Sys.time()))
}                       
#period_length_html <- html_nodes(webpage,'#q-nights.widget-query-nights') %>% html_text() %>% gsub(" night","",.)
params <- list(hotel_name_html,address_html,sector_html,city_center,guest_rating_score,
               guest_rating_verbal,hotel_com_reviews_html,ta_rating_html,price_for_period_html)
for (i in params){
  print(length(i))
}

all_loc_df <- data.frame(hotel_name = hotel_name_html,
                         address = address_html,
                         city_sector = sector_html,
                         miles_to_city_center = city_center,
                         guest_rating_score,
                         guest_rating_verbal,
                         num_of_hotel_com_reviews = hotel_com_reviews_html,
                         trip_advisor_rating = ta_rating_html,
                         price_for_period_USD = price_for_period_html,
                         checkin = rep(checkin,length(hotel_name_html)),
                         checkout = rep(checkout,length(hotel_name_html)),
                         q_adults = rep(q_adults,length(hotel_name_html)),
                         q_children = rep(q_children,length(hotel_name_html))
                         )


# Location Amenities
hotel_id <- html_nodes(webpage,".listings li") %>% html_attr("data-hotel-id") %>% as.numeric() %>% .[!is.na(.)]

h_name <- character()
curr_id <- numeric()
star_rating <- numeric()
h_coord <- character()
num_of_rooms <- numeric()
num_of_floors <- numeric()
rest <- numeric()
bar_lounge <- numeric()
gym <- numeric()
meeting_room <- numeric()
ac <- numeric()
wifi <- numeric()
laundry <- numeric()
pool <- numeric()
parking <- numeric()
count <- 0
for (i in 1957:length(hotel_id)){
  loc_url <- read_html(paste0("https://www.hotels.com/ho",hotel_id[i],"?pos=HCOM_US&locale=en_US"))
  
  hotel_size_list <- (html_nodes(loc_url,"div.info-box")[1] %>% html_nodes(.,"ul"))[1] %>% html_nodes(.,"li") %>% tolower()
  in_the_hotel_list <- html_nodes(loc_url,".in-the-property-module") %>% tolower()
  in_the_room_list <- html_nodes(loc_url,".in-the-room-module") %>% tolower()
  parking_list <- html_nodes(loc_url,"div.info-box")[grep("transportation",tolower(html_nodes(loc_url,"div.info-box")))] %>% tolower()
  
  name <- html_nodes(loc_url,"h1") %>% as.character() %>%
    str_extract(.,pattern = "(.*)[,]") %>% 
    gsub(pattern = "<h1>|,",replacement = "")
  h_name <- c(h_name,name)
  
  curr_id <- c(curr_id,hotel_id[i])
  
  star_rating <- if(length(html_nodes(loc_url,".vcard span.star-rating-text") %>% html_text()) > 0) {
    star_rating <- c(star_rating,html_nodes(loc_url,".vcard span.star-rating-text") %>% html_text() %>%
                       str_extract(.,'(.*)[-]') %>%
                       gsub("-","",.) %>%
                       as.numeric())
  } else {
    star_rating <- c(star_rating,NA)
  }
  
  if(length(html_nodes(loc_url,".map-widget")[1] %>% html_attr("style")) > 0) {
  coord_string <- html_nodes(loc_url,".map-widget")[1] %>% html_attr("style")
  coords_start_end <- c((coord_string %>% gregexpr(pattern ='center=',.))[[1]][1]+7,
                        (coord_string %>% gregexpr(pattern ='&zoom',.))[[1]][1]-1)
  coords <- substr(coord_string,start = coords_start_end[1],stop = coords_start_end[2])
  h_coord <- c(h_coord,coords)
  } else {
    h_coord <- c(h_coord,NA)
  }
  
  num_of_rooms <- c(num_of_rooms,str_extract(string = hotel_size_list[1],pattern = '[0-9]+'))
  num_of_floors <- c(num_of_floors,str_extract(string = hotel_size_list[2],pattern = '[0-9]+'))
  
  rest <- c(rest,ifelse(sum(grepl(pattern = "restaurant",x = in_the_hotel_list))!=0,1,0))
  
  bar_lounge <- c(bar_lounge,ifelse(sum(grepl(pattern = "bar|lounge",x = in_the_hotel_list))!=0,1,0))
  
  gym <- c(gym,ifelse(sum(grepl(pattern = "fitness|gym",x = in_the_hotel_list))!=0,1,0))
  
  meeting_room <- c(meeting_room,ifelse(sum(grepl(pattern = "business center|conference|meeting room",x = in_the_hotel_list))!=0,1,0))
  
  ac <- c(ac,ifelse(sum(grepl(pattern = "air conditioning",x = in_the_room_list))!=0,1,0))
  
  wifi <- c(wifi,ifelse(sum(grepl(pattern = "wifi",x = in_the_room_list))!=0,1,0))
  
  laundry <- c(laundry,ifelse(sum(grepl(pattern = "laundry",x = in_the_hotel_list))!=0,1,0))
  
  pool <- c(pool,ifelse(sum(grepl(pattern = "pool",x = in_the_hotel_list))!=0,1,0))
  
  parking <- c(parking,ifelse(sum(grepl(pattern = "no parking",x = parking_list))==0,1,0))
  
  count <- count+1
  print(paste0(count,"/",length(hotel_id)," Completed - ",Sys.time()," - ",h_name[count]))
}

params_loc <- list(h_name,curr_id,star_rating,h_coord,num_of_rooms,num_of_floors,rest,bar_lounge,
                   gym,meeting_room,ac,wifi,laundry,pool,parking)
for (i in params_loc){
  print(length(i))
}


loc_df <- data.frame(hotel_name = h_name,
                     hotel_id = curr_id,
                     star_rating = star_rating,
                     hotel_coordinates = h_coord,
                     num_of_rooms = num_of_rooms,
                     num_of_floors = num_of_floors,
                     rest = rest,
                     bar_lounge = bar_lounge,
                     gym = gym,
                     meeting_room = meeting_room,
                     ac = ac,
                     wifi = wifi,
                     laundry = laundry,
                     pool = pool,
                     parking = parking)
loc_df$hotel_coordinates <- as.character(loc_df$hotel_coordinates)

data_tab <- merge(x = all_loc_df,y = loc_df,by = "hotel_name")
head(data_tab)
dim(data_tab)


# 1-2.10
all_loc_1 <- all_loc_df
loc_df_1 <- loc_df
# 25-26.10
all_loc_2 <- all_loc_df
loc_df_2 <- loc_df
# 18-19.11
all_loc_3 <- all_loc_df
loc_df_3 <- loc_df
# 18-19.11
all_loc_3 <- all_loc_df
loc_df_3 <- loc_df


#london_tab <- distinct(data_tab)
#munich_tab <- distinct(data_tab)
#LA_tab <- distinct(data_tab)
#boston_tab <- distinct(data_tab)



# write.csv(london_tab,"london_tab.csv")
# write.csv(munich_tab,"munich_tab.csv")
# write.csv(LA_tab,"LA_tab.csv")
# write.csv(boston_tab,"boston_tab.csv")








# # Adding Selina's location from Hotels.com #
# 
# # Navigating to desirable website
# remDr$open()
# url_hotels <- "https://www.hotels.com/search.do?resolved-location=CITY%3A1072961%3AUNKNOWN%3AUNKNOWN&destination-id=1072961&q-destination=Porto,%20Portugal&q-check-in=2019-08-25&q-check-out=2019-08-26&q-rooms=1&q-room-0-adults=2&q-room-0-children=0&f-hotel-id=874087744"
# url_hotels <- paste0(url_hotels,"&locale=en_US") # avoiding hebrew results
# remDr$navigate(url_hotels)
# 
# # URL Hotels.com
# page_source <- remDr$getPageSource()
# webpage <- read_html(page_source[[1]])
# 
# # Scraping main bulletin
# hotels_wrap <- html_nodes(webpage,'.hotel-wrap')
# exclude_hotels_index <- grep(pattern = "sold out",x = hotels_wrap)
# hotels_wrap <- hotels_wrap[-exclude_hotels_index]
# 
# # Loop for inserting NA's where node is absent
# hotel_name_html <- character()
# address_html <- character()
# sector_html <- character()
# city_center <- numeric()
# central_station <- numeric()
# guest_rating_score <- numeric()
# guest_rating_verbal <- character()
# hotel_com_reviews_html <- numeric()
# ta_rating_html <- character()
# price_for_period_html <- character()
# countime <- 0
# 
# for (j in 1:length(hotels_wrap)){
#   
#   # Scraping fields from main bulletin
#   hotel_name_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.p-name')) > 0,
#                                html_nodes(hotels_wrap[j],'.p-name') %>% html_text(),
#                                NA)
#   
#   address_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.contact')) > 0,
#                             html_nodes(hotels_wrap[j],'.contact') %>% html_text(),
#                             NA)
#   
#   sector_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.map-link.xs-welcome-rewards')) > 0,
#                            html_nodes(hotels_wrap[j],'.map-link.xs-welcome-rewards') %>% html_text(),
#                            NA)
#   
#   city_center[j] <- html_nodes(hotels_wrap[j],'.property-landmarks li')[1] %>% html_text() %>% 
#     gsub(" miles to City center","",.) %>% 
#     as.numeric()
#   # city_center[j] <- landmark_html[seq(from = 1,to = length(landmark_html),by = 2)] %>%
#   #                             substr(1,4) %>% 
#   #                             gsub(" ","",.) %>%
#   #                             as.numeric()
#   # central_station[j] <- landmark_html[seq(from = 2,to = length(landmark_html),by = 2)] %>% 
#   #                     substr(1,4) %>% 
#   #                     gsub(" ","",.) %>%
#   #                     as.numeric()
#   
#   guest_rating_html <- ifelse(length(html_nodes(hotels_wrap[j],'.guest-reviews-badge')) > 0,
#                               html_nodes(hotels_wrap[j],'.guest-reviews-badge') %>% html_text(),
#                               NA)
#   guest_rating_score[j] <- substr(guest_rating_html,nchar(guest_rating_html)-3,nchar(guest_rating_html)) %>% gsub(" ","",.) %>% as.numeric()
#   guest_rating_verbal[j] <- substr(guest_rating_html,1,nchar(guest_rating_html)-4) %>% gsub("l ","l",.)
#   
#   hotel_com_reviews_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.small-view')) > 0,
#                                       html_nodes(hotels_wrap[j],'.small-view') %>% html_text() %>% 
#                                         gsub(" reviews","",.) %>%
#                                         gsub(",","",.) %>% 
#                                         as.numeric(),
#                                       NA)
#   
#   ta_rating_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.ta-logo')) > 0,
#                               html_nodes(hotels_wrap[j],'.ta-logo') %>% html_text() %>% 
#                                 gsub("TripAdvisor Traveller Rating: ","",.) %>% 
#                                 as.numeric(),
#                               NA)
#   
#   price_for_period_html[j] <- ifelse(length(html_nodes(hotels_wrap[j],'.price')) > 0,
#                                      html_nodes(hotels_wrap[j],'.price') %>% html_text() %>% 
#                                        str_extract(.,pattern = '[$][0-9]*[$][0-9]*|[$][0-9]*') %>% 
#                                        substr(.,2,nchar(.)) %>% 
#                                        gsub(pattern = "[0-9]*[$]","",.) %>% 
#                                        as.numeric(),
#                                      NA)
#   countime <- countime+1
#   print(paste0(countime,"/",length(hotels_wrap)," Completed"))
# }       
# 
# # Location Amenities
# hotel_id <- html_nodes(webpage,".listings li") %>% html_attr("data-hotel-id") %>% as.numeric() %>% .[!is.na(.)]
# 
# h_name <- character()
# curr_id <- numeric()
# star_rating <- numeric()
# h_coord <- character()
# num_of_rooms <- numeric()
# rest_bar_lounge <- numeric()
# gym <- numeric()
# meeting_room <- numeric()
# ac <- numeric()
# wifi <- numeric()
# laundry <- numeric()
# pool <- numeric()
# parking <- numeric()
# count <- 0
# for (i in hotel_id){
#   loc_url <- read_html(paste0("https://www.hotels.com/ho",i,"?pos=HCOM_US&locale=en_US"))
#   
#   amen_list <- html_nodes(loc_url,"#overview div")[2] %>% html_nodes(.,"li") %>% tolower()
#   
#   name <- html_nodes(loc_url,"h1") %>% as.character() %>%
#     str_extract(.,pattern = "(.*)[,]") %>% 
#     gsub(pattern = "<h1>|,",replacement = "")
#   h_name <- c(h_name,name)
#   
#   curr_id <- c(curr_id,i)
#   
#   star_rating <- if(length(html_nodes(loc_url,".vcard span.star-rating-text") %>% html_text()) > 0) {
#     star_rating <- c(star_rating,html_nodes(loc_url,".vcard span.star-rating-text") %>% html_text() %>%
#                        str_extract(.,'(.*)[-]') %>%
#                        gsub("-","",.) %>%
#                        as.numeric())
#   } else {
#     star_rating <- c(star_rating,NA)
#   }
#   
#   coord_string <- html_nodes(loc_url,".map-widget")[1] %>% html_attr("style")
#   coords_start_end <- c((coord_string %>% gregexpr(pattern ='center=',.))[[1]][1]+7,
#                         (coord_string %>% gregexpr(pattern ='&zoom',.))[[1]][1]-1)
#   coords <- substr(coord_string,start = coords_start_end[1],stop = coords_start_end[2])
#   h_coord <- c(h_coord,coords)
#   
#   num_of_rooms <- c(num_of_rooms,str_extract(string = as.character(amen_list[1]),pattern = "(>)[0-9]*") %>% substr(.,start = 2,stop = nchar(.))) %>% as.numeric()
#   
#   rest_bar_lounge <- c(rest_bar_lounge,ifelse(sum(grepl(pattern = "restaurant|bar|lounge",x = amen_list))!=0,1,0))
#   
#   gym <- c(gym,ifelse(sum(grepl(pattern = "fitness|gym",x = amen_list))!=0,1,0))
#   
#   meeting_room <- c(meeting_room,ifelse(sum(grepl(pattern = "business center|Conference",x = amen_list))!=0,1,0))
#   
#   ac <- c(ac,ifelse(sum(grepl(pattern = "air conditioning",x = amen_list))!=0,1,0))
#   
#   wifi <- c(wifi,ifelse(sum(grepl(pattern = "wifi",x = amen_list))!=0,1,0))
#   
#   laundry <- c(laundry,ifelse(sum(grepl(pattern = "laundry",x = amen_list))!=0,1,0))
#   
#   pool <- c(pool,ifelse(sum(grepl(pattern = "pool",x = amen_list))!=0,1,0))
#   
#   parking <- c(parking,ifelse(sum(grepl(pattern = "parking",x = amen_list))!=0,1,0))
#   
#   count <- count+1
#   print(paste0(count,"/",length(hotel_id)," Completed"))
# }
# 
# all_loc_df <- data.frame(hotel_name = hotel_name_html,
#                          address = address_html,
#                          city_sector = sector_html,
#                          miles_to_city_center = city_center,
#                          guest_rating_score,
#                          guest_rating_verbal,
#                          num_of_hotel_com_reviews = hotel_com_reviews_html,
#                          trip_advisor_rating = ta_rating_html,
#                          price_for_period_USD = price_for_period_html,
#                          checkin = rep(checkin,length(hotel_name_html)),
#                          checkout = rep(checkout,length(hotel_name_html)),
#                          q_adults = rep(q_adults,length(hotel_name_html)),
#                          q_children = rep(q_children,length(hotel_name_html))
# )
# 
# loc_df <- data.frame(hotel_name = h_name,
#                      hotel_id = curr_id,
#                      star_rating = star_rating,
#                      hotel_coordinates = h_coord,
#                      num_of_rooms = num_of_rooms,
#                      rest_bar_lounge = rest_bar_lounge,
#                      gym = gym,
#                      meeting_room = meeting_room,
#                      ac = ac,
#                      wifi = wifi,
#                      laundry = laundry,
#                      pool = pool,
#                      parking = parking)
# loc_df$hotel_coordinates <- as.character(loc_df$hotel_coordinates)
# 
# data_tab <- merge(x = all_loc_df,y = loc_df,by = "hotel_name")
# head(data_tab)
# 
# dim(data_tab)
# # selina_tab <- data.frame()
# selina_tab <- rbind(selina_tab,data_tab[grep("Selina",data_tab$hotel_name),])
# # selina_tab <- selina_tab[-c(1,2),]
