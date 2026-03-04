source("Setup_523D.R")

setwd("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/523D_Remote_Sensing_Model/523D_Remote_Sensing_Model")

#create geojson with my polygon
tw1_lat <- 38.1074
tw1_lon <- -121.6469
tw4_lat <- 38.1027
tw4_lon <- -121.6413

#make sure we're in WGS84
tw1_pt <- st_sfc(st_point(c(tw1_lon, tw1_lat)), crs = 4326)
tw4_pt <- st_sfc(st_point(c(tw4_lon, tw4_lat)), crs = 4326)

#500m buffer
buf_m <- 500

#make sure buffer is in meters, but then transform back to lat/lon
tw1_aoi <- st_transform(tw1_pt, 32610) |> st_buffer(buf_m) |> st_transform(4326)
tw4_aoi <- st_transform(tw4_pt, 32610) |> st_buffer(buf_m) |> st_transform(4326)

st_write(tw1_aoi, "data/TW1_AOI_500m.geojson", delete_dsn = TRUE)
st_write(tw4_aoi, "data/TW4_AOI_500m.geojson", delete_dsn = TRUE)