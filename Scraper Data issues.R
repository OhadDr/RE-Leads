# Data Issues #

# One time fix
# data_all$hotel_name <- as.character(data_all$hotel_name)
# data_all$num_of_rooms <- as.numeric(as.character(data_all$num_of_rooms))
# data_all$num_of_floors <- as.numeric(as.character(data_all$num_of_floors))
# data_all$price_for_period_USD <- as.numeric(as.character(data_all$price_for_period_USD))
# data_all$city_sector <- as.character(data_all$city_sector)
# data_all$trip_advisor_rating <- as.numeric(as.character(data_all$trip_advisor_rating))
# data_all$address <- as.character(data_all$address)

# Aggregating 4 scrapes
data_all <- rbind(data_tab_1,data_tab_2,data_tab_3,data_tab_4)

# Checking for Noisy obs
hist(data_all$miles_to_city_center)
boxplot(data_all$miles_to_city_center)
range(data_all$miles_to_city_center,na.rm = T)
hist(data_all$guest_rating_score)
boxplot(data_all$guest_rating_score)
range(data_all$guest_rating_score,na.rm = T)
hist(data_all$num_of_hotel_com_reviews)
boxplot(data_all$num_of_hotel_com_reviews)
range(data_all$num_of_hotel_com_reviews,na.rm = T)
hist(data_all$trip_advisor_rating)
boxplot(data_all$trip_advisor_rating)
range(data_all$trip_advisor_rating,na.rm = T)
hist(data_all$price_for_period_USD)
boxplot(data_all$price_for_period_USD)
range(data_all$price_for_period_USD,na.rm = T) # Less than 10$ will be marked as NA
hist(data_all$star_rating)
boxplot(data_all$star_rating)
range(data_all$star_rating,na.rm = T)
hist(data_all$num_of_rooms)
boxplot(data_all$num_of_rooms)
range(data_all$num_of_rooms,na.rm = T) # Less than 20 rooms will be removed for irrelevancy
hist(data_all$num_of_floors)
boxplot(data_all$num_of_floors)
range(data_all$num_of_floors,na.rm = T)
hist(data_all$star_rating)
boxplot(data_all$star_rating)
range(data_all$star_rating,na.rm = T)

# Manipulating Noisy obs
data_all$price_for_period_USD <- ifelse(data_all$price_for_period_USD < 10,NA,data_all$price_for_period_USD)
'%!in%' <- function(x,y)!('%in%'(x,y)) # function for 'NOT IN'
data_all <- data_all %>% filter(num_of_rooms %!in% c(1:19))


# Aggregating ADR's
data_agg <- data_all %>% 
            group_by(.,city_sector) %>%
            mutate(.,district_adr = mean(price_for_period_USD,na.rm = T)) %>% # Adding district average ADR
            group_by(.,hotel_name) %>%
            mutate(.,avg_adr = mean(price_for_period_USD,na.rm = T))
data_agg <- data_agg %>% select(.,-c(9:13)) %>% distinct()
summary(data_agg)

# Adding ADR Performance features
data_agg$below_above_adr <- ifelse(data_agg$avg_adr < data_agg$district_adr,0,1)
data_agg$avg_adr_div_dist_adr <- data_agg$avg_adr/data_agg$district_adr
data_agg$avg_adr_div_dist_adr[which(data_agg$avg_adr_div_dist_adr %in% c(NaN))] <- NA

# Adding Brand feature
Global_Hotel_brand <- c("AccorHotels","Aman","Ascott","Banyan","Tree	Barrière","Belmond","Best Western","Home inn","Radisson","Centara","Huazhu","Clayton",
                       "Dorchester Collection","Drury","Dusit Thani","Extended Stay","Four Seasons","GreenTree","Hilton","Hoshino","Hyatt","InterContinental",
                       "Interstate","InTown","Jumeirah","Kempinski","Langham","Lifestyle Holidays","Loews","Lotte","Magnuson","Mandarin","Marriott","Melia",
                       "Copthorne","MGM","Minor","NH","Oberoi","Nikko","Omni","Pan Pacific","Red Lion","Red Roof Inn","RIU","Rocco Forte","Rosewood","Scandic",
                       "Shangri-La","Shilo Inns","Dedica Anthology","Toyoko Inn","Travelodge","Treebo","Westgate","Wyndham","Whitbread")
London_Hotel_brand <- c("Mercure","Comfort inn","doubletree","club quarters","Jurys","Melia","Novotel","hilton","holiday inn","Radisson","Crowne Plaza","Grange",
                       "Park Plaza","Thistle","Mandarin","Millennium","Intercontinental")
UK_Hotel_brand <- c("Apex","Belmond","Best Western","Bloc","Britannia","Champneys","Corus","Crerar","Crest","Dorchester Collection","EasyHote","Four Pillars",
                   "Hotel du Vin","Innkeeper's Lodge","Jurys Inn","Macdonald","Malmaison","McMillan","Metro Inns","Montcalm","Premier Inn","The Principal",
                   "De Vere","Ramada","Stakis","Swallow","Thistle","Travelodge ","Village Hotel","Wetherspoons","YOTEL")
isBrandGlobal <- rep(0,dim(data_agg)[1])
brandNameGlobal <- rep("Other",dim(data_agg)[1])
isBrandUK <- rep(0,dim(data_agg)[1])
brandNameUK <- rep("Other",dim(data_agg)[1])
isBrandLondon <- rep(0,dim(data_agg)[1])
brandNameLondon <- rep("Other",dim(data_agg)[1])
asset_name <- data_agg$hotel_name

for (i in 1:length(asset_name)){
  ind <- grep(pattern = Global_Hotel_brand[i],x = asset_name,ignore.case = T)
  isBrandGlobal[ind] <- 1
  brandNameGlobal[ind] <- Global_Hotel_brand[i]
  
  ind <- grep(pattern = London_Hotel_brand[i],x = asset_name,ignore.case = T)
  isBrandLondon[ind] <- 1
  brandNameLondon[ind] <- London_Hotel_brand[i]
  
  ind <- grep(pattern = UK_Hotel_brand[i],x = asset_name,ignore.case = T)
  isBrandUK[ind] <- 1
  brandNameUK[ind] <- UK_Hotel_brand[i]
}

isBrand <- ifelse(isBrandGlobal+isBrandUK+isBrandLondon > 0,1,0)

brand_df <- data.frame(isBrand=isBrand,
                       isBrandGlobal=isBrandGlobal,
                       brandNameGlobal=brandNameGlobal,
                       isBrandUK=isBrandUK,
                       brandNameUK=brandNameUK,
                       isBrandLondon=isBrandLondon,
                       brandNameLondon=brandNameLondon)

data_agg <- data.frame(data_agg,brand_df)


# WalkScore #
library(walkscoreAPI)
count_time <- 0
walk_score <- as.numeric()
for(i in 1:length(data_agg$hotel_coordinates)){
  cords <- strsplit(data_agg$hotel_coordinates[i],",")
  lon <- unlist(cords)[2]
  lat <- unlist(cords)[1]
  walkScore <- getWS(lon,lat,"672514c43418658fd5dea42c93f4c4b2")
  walk_score[i] <- walkScore$walkscore
  count_time <- count_time+1
  print(paste0(count_time,"/",length(data_agg$hotel_coordinates)," Completed - ",Sys.time()," - ",h_name[count_time]))
}

data_agg$walk_score <- as.numeric(walk_score)


write.csv(data_agg,"data_agg.csv")


# Estimate Commercial Space - assumption that 10% of total asset space is commercial
# total_space <- num_of_rooms*40
# comm_space <- total_space*0.1