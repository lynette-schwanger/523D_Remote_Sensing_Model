source("Setup_523D.R")

setwd("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/523D_Remote_Sensing_Model/523D_Remote_Sensing_Model")

#load in training data
tw4_dd <- read_csv("data/raw/AMF_US-Tw4_FLUXNET_FULLSET_DD_2013-2023_5-7.csv")

#what columns do we have
names(tw4_dd)

#hmm, only wind speed, not direction. maybe hourly data has it
tw4_hh <- read_csv("data/raw/AMF_US-Tw4_FLUXNET_FULLSET_HH_2013-2023_5-7.csv")

names(tw4_hh)

#yup, hourly has it. I'll need to join these datasets. first, select only columns I need from hh
tw4_hh <- tw4_hh %>%
  select(TIMESTAMP_START, TIMESTAMP_END, WD)

#oh, but I only need one year. 2018 is ~4 years after restoration, should be prime veg time
tw4_hh_2018 <- tw4_hh %>%
  filter(TIMESTAMP_START >= 201801010000, TIMESTAMP_START <= 201812312330)

#great, let's do the same for dd
names(tw4_dd)

tw4_dd <- tw4_dd %>%
  select(TIMESTAMP, GPP_NT_VUT_REF, USTAR, USTAR_QC, NEE_VUT_REF_QC)

#only 2018
tw4_dd_2018 <- tw4_dd %>%
  filter(TIMESTAMP >= 20180101, TIMESTAMP <= 20181231)

#let's look at it
plot(tw4_dd_2018)

#a look at just the important stuff
tw4_dd_2018 %>%
  ggplot(aes(x = TIMESTAMP, y = GPP_NT_VUT_REF)) +
  geom_line() 

#ok, funky curve but it's there. 
#once I remove low QC values it might smooth out
#but make sure not to remove large sections

#let's get date columns for joining and for labels and get this curve in shape
tw4_dd_2018 <- tw4_dd_2018 %>%
  mutate(date = ymd(TIMESTAMP),
         date_mdy = format(date, "%m-%d-%Y"))

qc_thresh <- 0.8  

tw4_dd_gpp_qc <- tw4_dd_2018 %>%
  filter(!is.na(GPP_NT_VUT_REF)) %>%
  filter(!is.na(NEE_VUT_REF_QC), NEE_VUT_REF_QC >= qc_thresh) 

tw4_dd_gpp_qc %>%
  count(month = lubridate::month(date), name = "n_days_kept")
#not bad, still lots of days for each month

tw4_dd_gpp_qc %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at US-Tw4", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = "Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111")
#looking much better!

#let's clean up and save the data for model use
tw4_dd_gpp_clean <- tw4_dd_gpp_qc %>%
  select(TIMESTAMP, date, date_dmy, GPP_NT_VUT_REF, NEE_VUT_REF_QC)

#realized no site data in the dataset, let's add
tw4_dd_gpp_clean <- tw4_dd_gpp_clean %>%
  mutate(
    site = "US-Tw4",
    latitude = 38.1027,
    longitude = -121.6413
  )

#annnd reorder it
tw4_dd_gpp_clean <- tw4_dd_gpp_clean %>%
  select(site, latitude, longitude, TIMESTAMP, date, date_dmy, GPP_NT_VUT_REF, NEE_VUT_REF_QC)
  
write_csv(tw4_dd_gpp_clean, "data/tw4_gpp_clean.csv")

#ok, let's get the spectral reflectance data in here
#below is the code I used to download the data
#I ran the API request in Dr. O'Connell's script
#but copying here for reference:

#-------------------------------------------------------------------#
credentials<-read.csv("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/API_Scripts/NASAearthdata_creds.csv")

head(credentials)

## setup user name and password with the appeears package;
# set the environmental variable 
options(keyring_backend = "file")

## use credentials stored above to set username and password
# this way you won't type it into the code directly
rs_set_key(user = credentials$user, 
           password = credentials$psw)

##test that it worked; the code below should print your password to the console
rs_get_key(user = credentials$user)

head(credentials)

token <- rs_login(user = credentials$user)

dfls <- data.frame(
  task = "time_series",
  subtask = "US-Tw4_LS",
  latitude = c(38.1027),
  longitude = c(-121.6413),
  start = "2018-01-01",
  end = "2018-12-31",
  product = "HLSL30.020",
  layer = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B09", "B10", "B11", "Fmask")
)

dfs <- data.frame(
  task = "time_series",
  subtask = "US-Tw4_S2",
  latitude = c(38.1027),
  longitude = c(-121.6413),
  start = "2018-01-01",
  end = "2018-12-31",
  product = "HLSS30.020",
  layer = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12", "B09", "B10", "Fmask")
)


dfls$task <- "point"
task <- rs_build_task(dfls)

dfs$task <- "point"
task <- rs_build_task(dfs)

##below, path is a file path; we've already set our working directory, so now can just mention the data folder
##credentials$user loads in the user string from our credentials file
rs_request(
  request = task,
  user = credentials$user,
  transfer = TRUE,
  path = "data/",
  verbose = TRUE
)

##You can also find your request here: https://appeears.earthdatacloud.nasa.gov/explore
## if your download fails, it will be stored there for you to get it
#^this is what happened to me every time

#-------------------------------------------------------------------#

#got it. now read it in
tw4_ls <- read_csv("data/raw/US_Tw4_LS/point-HLSL30-020-results.csv")

tw4_s2 <- read_csv("data/raw/US_Tw4_S2/point-HLSS30-020-results.csv")

#convert all the column names so they match up with each other. oh god. 
tw4_ls_converted <- tw4_ls %>%
  transmute(
    latitude = Latitude,
    longitude = Longitude,
    date = as.Date(Date),
    sensor = Category,
    hls_tile = HLS_Tile,
    B01 = HLSL30_020_B01,
    B02 = HLSL30_020_B02,
    B03 = HLSL30_020_B03,
    B04 = HLSL30_020_B04,
    nir = HLSL30_020_B05,  
    swir1 = HLSL30_020_B06,
    swir2 = HLSL30_020_B07,
    fmask = HLSL30_020_Fmask,
    cloud = HLSL30_020_Fmask_Cloud,
    cshadow = HLSL30_020_Fmask_Cloud_shadow)

tw4_s2_converted <- tw4_s2 %>%
  transmute(
    latitude = Latitude,
    longitude = Longitude,
    date = as.Date(Date),
    sensor = Category,
    hls_tile = HLS_Tile,
    B01 = HLSS30_020_B01,
    B02 = HLSS30_020_B02,
    B03 = HLSS30_020_B03,
    B04 = HLSS30_020_B04,
    nir = HLSS30_020_B8A,  
    swir1 = HLSS30_020_B11,
    swir2 = HLSS30_020_B12,
    fmask = HLSS30_020_Fmask,
    cloud = HLSS30_020_Fmask_Cloud,
    cshadow = HLSS30_020_Fmask_Cloud_shadow)

#phew. one (well two actually) lines of code to bind them all
tw4_lss2_bind <- bind_rows(tw4_ls_converted, tw4_s2_converted) %>%
  arrange(date, sensor)

#keep only the best days when there are dupes
tw4_lss2_bind_best <- tw4_lss2_bind %>%
  mutate(is_clear = (cloud == "0b0" & cshadow == "0b0")) %>%
  group_by(date) %>%
  arrange(desc(is_clear), sensor) %>%   
  slice(1) %>%
  ungroup()

#woooweee we're getting there. let's save all this data
write_csv(tw4_lss2_bind_best, "data/US-Tw4_LSS2_clean.csv")
write_csv(tw4_dd_gpp_qc, "data/US-TW4_flux_clean.csv")


#on to spectral reflectance! 
d <- as.Date("2018-01-20")

bands <- c("B01","B02","B03","nir","swir1","swir2") 

tw4_lss2_bind_best %>%
  filter(date == d) %>%
  select(date, sensor, any_of(bands)) %>%
  pivot_longer(cols = any_of(bands), names_to = "band", values_to = "refl") %>%
  mutate(
    band = factor(band, levels = bands)
  ) %>%
  ggplot(aes(x = band, y = refl, group = sensor, color = sensor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = paste("Spectral reflectance (HLS) at US-Tw4:", d),
    x = "Band",
    y = "Surface reflectance (unitless)"
  )

