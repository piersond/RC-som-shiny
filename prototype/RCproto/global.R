library(dplyr)
library(rgdal)

#setwd("C:/GitHub/RC-som-shiny/prototype/RCproto")

# allzips <- readRDS("data/superzip.rds")
# allzips$latitude <- jitter(allzips$latitude)
# allzips$longitude <- jitter(allzips$longitude)
# allzips$college <- allzips$college * 100
# allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
# row.names(allzips) <- allzips$zipcode
# colnames(allzips)

RC_database <- readRDS("data/RC_database_current.rds")
RC_database  <- type.convert(RC_database)
RC_database$uniqueID <- paste0("ID",seq(1,nrow(RC_database),1))
RC_database <- RC_database %>% filter(!is.na(lat)) %>% filter(!is.na(long))
RC_database <- RC_database %>% rename(Dataset = google_dir)

#colnames(RC_database)
#str(RC_database)

# cleantable <- allzips %>%
#   select(
#     City = city.x,
#     State = state.x,
#     Zipcode = zipcode,
#     Rank = rank,
#     Score = centile,
#     Superzip = superzip,
#     Population = adultpop,
#     College = college,
#     Income = income,
#     Lat = latitude,
#     Long = longitude
#   )


#Reynolds creek shapefile
#file.exists('./map/watersheds_2014.shp')
rc_watersheds <- readOGR("./map/watersheds_2014.shp", layer="watersheds_2014")
rc_watersheds <- spTransform(rc_watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Reynolds creek met stations
rc_met <- read.csv("./map/ARS_climate_station_locs.csv", as.is=T)
