source("Setup_523D.R")

setwd("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/523D_Remote_Sensing_Model/523D_Remote_Sensing_Model")

tw1_dd <- read_csv("data/raw/AMF_US-Tw1_FLUXNET_FULLSET_DD_2011-2023_5-7.csv")

tw4_dd <- read_csv("data/raw/AMF_US-Tw4_FLUXNET_FULLSET_DD_2013-2023_5-7.csv")

tw1_dd <- tw1_dd %>%
  select(TIMESTAMP, GPP_NT_VUT_REF)

tw4_dd <- tw4_dd %>%
  select(TIMESTAMP, GPP_NT_VUT_REF)

#only my years
tw1_dd_2014_2017 <- tw1_dd %>%
  filter(TIMESTAMP >= 20140101, TIMESTAMP <= 20171231)

#only my years
tw4_dd_2014_2017 <- tw4_dd %>%
  filter(TIMESTAMP >= 20140101, TIMESTAMP <= 20171231)

#let's get date columns for joining and for labels and get this curve in shape
tw1_dd_2014_2017 <- tw1_dd_2014_2017 %>%
  mutate(date = ymd(TIMESTAMP),
         date_mdy = format(date, "%m-%d-%Y"))

tw4_dd_2014_2017 <- tw4_dd_2014_2017 %>%
  mutate(date = ymd(TIMESTAMP),
         date_mdy = format(date, "%m-%d-%Y"))

#let's look at them
tw1_dd_2014_2017 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at US-Tw1", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = "2018 daily GPP at US-Tw1. Daily gross primary productivity (GPP; g C m⁻² d⁻¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111")

tw4_dd_2014_2017 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at US-Tw4", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = "2018 daily GPP at US-Tw4. Daily gross primary productivity (GPP; g C m⁻² d⁻¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111")

#cool, looks as expected 
#need to split the data into train/test and save
#oh and add labels and coords
tw1_dd_2014_2017 <- tw1_dd_2014_2017 %>%
  mutate(
    site = "US-Tw1",
    latitude = 38.1074,
    longitude = -121.6469
  )

#annnd reorder it
tw1_dd_2014_2017 <- tw1_dd_2014_2017 %>%
  select(site, latitude, longitude, TIMESTAMP, date, date_mdy, GPP_NT_VUT_REF)

#same for tw4
tw4_dd_2014_2017 <- tw4_dd_2014_2017 %>%
  mutate(
    site = "US-Tw4",
    latitude = 38.1027,
    longitude = -121.6413
  )

#annnd reorder it
tw4_dd_2014_2017 <- tw4_dd_2014_2017 %>%
  select(site, latitude, longitude, TIMESTAMP, date, date_mdy, GPP_NT_VUT_REF)

#now split
tw1_dd_2014_2015 <- tw1_dd_2014_2017 %>%
  filter(TIMESTAMP >= 20140101, TIMESTAMP <= 20151231)

tw1_dd_2016_2017 <- tw1_dd_2014_2017 %>%
  filter(TIMESTAMP >= 20160101, TIMESTAMP <= 20171231)


tw4_dd_2014_2015 <- tw4_dd_2014_2017 %>%
  filter(TIMESTAMP >= 20140101, TIMESTAMP <= 20151231)

tw4_dd_2016_2017 <- tw4_dd_2014_2017 %>%
  filter(TIMESTAMP >= 20160101, TIMESTAMP <= 20171231)

#now join
train <- bind_rows(tw1_dd_2016_2017, tw4_dd_2014_2015)

test <- bind_rows(tw1_dd_2014_2015, tw4_dd_2016_2017)

#and save
write_csv(train, "data/train_flux_clean.csv")

write_csv(test, "data/test_flux_clean.csv")

#ok, let's get the spectral reflectance data in here
#below is the code copied from Dr. O'Connel's script

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
  start = "2014-01-01",
  end = "2017-12-31",
  product = "HLSL30.020",
  layer = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B09", "B10", "B11", "Fmask")
)

dfs <- data.frame(
  task = "time_series",
  subtask = "US-Tw4_S2",
  latitude = c(38.1027),
  longitude = c(-121.6413),
  start = "2014-01-01",
  end = "2017-12-31",
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
  path = "data/raw/",
  verbose = TRUE
)

##You can also find your request here: https://appeears.earthdatacloud.nasa.gov/explore
## if your download fails, it will be stored there for you to get it
#^this is what happened to me every time

#ok, now do the same for US-Tw1
dfls_tw1 <- data.frame(
  task = "time_series",
  subtask = "US-Tw1_LS",
  latitude = c(38.1074),
  longitude = c(-121.6469),
  start = "2014-01-01",
  end = "2017-12-31",
  product = "HLSL30.020",
  layer = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B09", "B10", "B11", "Fmask")
)

dfs_tw1 <- data.frame(
  task = "time_series",
  subtask = "US-Tw1_S2",
  latitude = c(38.1074),
  longitude = c(-121.6469),
  start = "2014-01-01",
  end = "2017-12-31",
  product = "HLSS30.020",
  layer = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12", "B09", "B10", "Fmask")
)


dfls_tw1$task <- "point"
task <- rs_build_task(dfls_tw1)

dfs_tw1$task <- "point"
task <- rs_build_task(dfs_tw1)

##below, path is a file path; we've already set our working directory, so now can just mention the data folder
##credentials$user loads in the user string from our credentials file
rs_request(
  request = task,
  user = credentials$user,
  transfer = TRUE,
  path = "data/raw/",
  verbose = TRUE
)

#-------------------------------------------------------------------#

#got it. now read it in
tw1_ls <- read_csv("data/raw/2014_2017_US_Tw1_LS/point-HLSL30-020-results.csv")

tw1_s2 <- read_csv("data/raw/2014_2017_US_Tw1_S2/point-HLSS30-020-results.csv")

tw4_ls <- read_csv("data/raw/2014_2017_US_Tw4_LS/point-HLSL30-020-results.csv")

tw4_s2 <- read_csv("data/raw/2014_2017_US_Tw4_S2/point-HLSS30-020-results.csv")

#great, now need to convert all the column names so they match up with each other. oh god. 
#line 183 in working v1
