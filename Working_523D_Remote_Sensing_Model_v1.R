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
       caption = "2018 daily GPP at US-Tw4. Daily gross primary productivity (GPP; g C m⁻² d⁻¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111")
#looking much better!
#let's fix the caption

caption_gpp <- "Figure 1: 2018 daily GPP at US-Tw4. Daily gross primary productivity (GPP; g C m-2 d-1) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall back to winter levels. Source: FLUXNET2015 US-Tw4, Dataset. https://doi.org/10.18140/FLX/1440111"

tw4_dd_gpp_qc %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at US-Tw4: Twitchell East End Wetland", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = str_wrap(caption_gpp, width = 120)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin  = margin(t = 10, r = 10, b = 20, l = 10)
  )


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

tw4_LSS2_clean <- read_csv("data/US-Tw4_LSS2_clean.csv")
tw4_flux_clean <- read_csv("data/US-TW4_flux_clean.csv")


#on to spectral reflectance! 
#one cloud-free date per season
dates <- tibble(
  date   = as.Date(c("2018-01-20", "2018-04-20", "2018-07-19", "2018-10-17")),
  season = c("Winter", "Spring", "Summer", "Fall")
)

#associate bands with proper wavelengths based on "HLS_User_Guide_v2.pdf"
#used center values
band_wavelengths <- tibble(
  band = c("B01","B02","B03","B04","nir","swir1","swir2"),
  wavelength_nm = c(440,  480,  560,  655, 865, 1610,  2200)
)

#pivot table to long
tw4_long <- tw4_LSS2_clean %>%
  inner_join(dates, by = "date") %>%           # keep only your 4 target dates + add season
  pivot_longer(
    cols = c(B01, B02, B03, B04, nir, swir1, swir2),
    names_to = "band",
    values_to = "reflectance"
  ) %>%
  left_join(band_wavelengths, by = "band") %>%
  mutate(
    season = factor(season, levels = c("Winter","Spring","Summer","Fall")),
    reflectance_pct = reflectance * 100
  )

#plot spectral reflectance
ggplot(tw4_long,
       aes(x = wavelength_nm, y = reflectance_pct,
           color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = band_wavelengths$wavelength_nm,
    labels = band_wavelengths$wavelength_nm
  ) +
  labs(
    x = "Wavelength (nm)",
    y = "% Reflectance",
    color = "Season"
  ) 

#ok! would love to spread out the wavelength scale a bit so that section is easier to see
band_order <- c("B01","B02","B03","B04","nir","swir1","swir2")

band_labels <- band_wavelengths %>%
  filter(band %in% band_order) %>%
  arrange(match(band, band_order)) %>%
  mutate(label = paste0(wavelength_nm)) %>%  
  { setNames(.$label, .$band) }

caption  <- "Seasonal spectral reflectance at US-Tw4 in 2018. Spectral reflectance is plotted across visible (440–655 nm), near-infrared (865 nm), and shortwave infrared (1610–2200 nm) bands for four representative dates. Reflectance is lowest in the visible bands and increases sharply in the NIR, with seasonal differences most pronounced at 865 nm, consistent with increased biomass during the growing season. Source: Harmonized Landsat Sentinel-2, https://www.earthdata.nasa.gov/data/catalog/lpcloud-hlsl30-2.0."
  
ggplot(tw4_long,
       aes(x = factor(band, levels = band_order),
           y = reflectance_pct,
           color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_discrete(labels = band_labels) +
  labs(
    title = "US-Tw4 Spectral Reflectance 2018",
    x = "Wavelength (nm)",   
    y = "% Reflectance",
    color = "Season",
    caption = str_wrap(caption, width = 90)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10) 
  )

#phew, worked it out. now onto the third plot
#well hmm. I probably should have done 8 dates, 2 per season, since I'll need 5-10 images
#ugh, let's start over
#TWO cloud-free dates per season
dates8 <- tibble(
  date   = as.Date(c("2018-01-25","2018-12-11",
                     "2018-03-29","2018-05-15",
                     "2018-06-19","2018-08-18",
                     "2018-09-22","2018-10-17")),
  season = c("Winter","Winter",
             "Spring","Spring",
             "Summer","Summer",
             "Fall","Fall")
)

tw4_long8 <- tw4_LSS2_clean %>%
  inner_join(dates, by = "date") %>%           # keep only your 4 target dates + add season
  pivot_longer(
    cols = c(B01, B02, B03, B04, nir, swir1, swir2),
    names_to = "band",
    values_to = "reflectance"
  ) %>%
  left_join(band_wavelengths, by = "band") %>%
  mutate(
    season = factor(season, levels = c("Winter","Spring","Summer","Fall")),
  )

tw4_season_avg <- tw4_long8 %>%
  group_by(season, band) %>%
  summarise(
    reflectance = mean(reflectance, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(reflectance_pct = reflectance * 100)

caption8 <- "Figure 2a: Seasonal spectral reflectance at US-Tw4 in 2018. Spectral reflectance is plotted across visible (440–655 nm), near-infrared (865 nm), and shortwave infrared (1610–2200 nm) bands for eight representative dates, two per season. Reflectance is lowest in the visible bands and increases sharply in the NIR, with seasonal differences most pronounced at 865 nm, consistent with increased biomass during the growing season. Source: Harmonized Landsat Sentinel-2, https://www.earthdata.nasa.gov/data/catalog/lpcloud-hlsl30-2.0."

ggplot(tw4_season_avg,
       aes(x = factor(band, levels = band_order),
           y = reflectance_pct,
           color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_discrete(labels = band_labels) +
  labs(
    title = "US-Tw4 Spectral Reflectance 2018",
    x = "Wavelength (nm)",   
    y = "% Reflectance",
    color = "Season",
    caption = str_wrap(caption8, width = 120)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10) 
  )

#annnnd it looks exactly the same, haha. makes sense
#at least this one is more defensible
#and I fixed the caption length, woooo
#would love to spread out % reflectance a bit so you can see the visible spectrum more easily

ggplot(tw4_season_avg,
       aes(x = factor(band, levels = band_order),
           y = reflectance_pct,
           color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_discrete(labels = band_labels) +
  labs(
    title = "US-Tw4 Spectral Reflectance 2018",
    x = "Wavelength (nm)",   
    y = "% Reflectance",
    color = "Season",
    caption = str_wrap(caption8, width = 120)
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10) 
  )

#just did a lil zoom in, I'll keep both plots
#think I'm going to go back and adjust the previous plot to keep the background grid though
#just take out theme_classic

ggplot(tw4_season_avg,
       aes(x = factor(band, levels = band_order),
           y = reflectance_pct,
           color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_discrete(labels = band_labels) +
  labs(
    title = "US-Tw4 Spectral Reflectance 2018",
    x = "Wavelength (nm)",   
    y = "% Reflectance",
    color = "Season",
    caption = str_wrap(caption8, width = 120)
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10) 
  )

#YAY
#GAH
#I should change the caption on the zoomed in plot

caption8zoom <- "Figure 2b: Seasonal spectral reflectance at US-Tw4 in 2018, zoomed in to highlight differences in the visible bands. Source: Harmonized Landsat Sentinel-2, https://www.earthdata.nasa.gov/data/catalog/lpcloud-hlsl30-2.0."


ggplot(tw4_season_avg,
       aes(x = factor(band, levels = band_order),
           y = reflectance_pct,
           color = season, group = season)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_discrete(labels = band_labels) +
  labs(
    title = "US-Tw4 Spectral Reflectance 2018",
    x = "Wavelength (nm)",   
    y = "% Reflectance",
    color = "Season",
    caption = str_wrap(caption8zoom, width = 120)
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  theme(
    plot.caption = element_text(hjust = 0, size = 10) 
  )

#ok, captions aligned and three plots down
#let's get one more plot in the books
#I guess it's time to get Tw1 data in here

#load in testing data
tw1_dd <- read_csv("data/raw/AMF_US-Tw1_FLUXNET_FULLSET_DD_2011-2023_5-7.csv")

tw1_dd <- tw1_dd %>%
  select(TIMESTAMP, GPP_NT_VUT_REF, USTAR, USTAR_QC, NEE_VUT_REF_QC)

#only 2018
tw1_dd_2018 <- tw1_dd %>%
  filter(TIMESTAMP >= 20180101, TIMESTAMP <= 20181231)

#let's look at it
plot(tw1_dd_2018)

#a look at just the important stuff
tw1_dd_2018 %>%
  ggplot(aes(x = TIMESTAMP, y = GPP_NT_VUT_REF)) +
  geom_line() 

#ok, funky curve but it's there. 
#once I remove low QC values it might smooth out
#but make sure not to remove large sections

#let's get date columns for joining and for labels and get this curve in shape
tw1_dd_2018 <- tw1_dd_2018 %>%
  mutate(date = ymd(TIMESTAMP),
         date_mdy = format(date, "%m-%d-%Y"))

qc_thresh <- 0.8  

tw1_dd_gpp_qc <- tw1_dd_2018 %>%
  filter(!is.na(GPP_NT_VUT_REF)) %>%
  filter(!is.na(NEE_VUT_REF_QC), NEE_VUT_REF_QC >= qc_thresh) 

tw1_dd_gpp_qc %>%
  count(month = lubridate::month(date), name = "n_days_kept")
#ooh, it ends in october. ugh. no good data past october?? :(
#ok lesson learned, should have done this earlier
#I'll just note that I didn't QC in order to not drop whole seasons
#this means I should also use tw4_dd_2018 and not tw4_dd_gpp_qc

tw4_dd_2018 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at US-Tw4: Training Site", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = "2018 daily GPP at US-Tw4. Daily gross primary productivity (GPP; g C m⁻² d⁻¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111")


tw1_dd_2018 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at Testing Site: US-Tw1 - Twitchell Wetland West Pond", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = "2018 daily GPP at US-Tw1. Daily gross primary productivity (GPP; g C m⁻² d⁻¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111")


#let's fix the caption
caption_tw4 <- "Figure 1a: 2018 daily GPP at US-Tw4. Daily gross primary productivity (GPP; g C m-2 d-1) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall back to winter levels. Source: FLUXNET2015 US-Tw4, Dataset. https://doi.org/10.18140/FLX/1440111"

tw4_dd_2018 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at Model Training Site: US-Tw4 - Twitchell East End Wetland", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = str_wrap(caption_tw4, width = 120)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin  = margin(t = 10, r = 10, b = 20, l = 10)
  )

caption_tw1 <- "Figure 1b: 2018 daily GPP at US-Tw1. Daily gross primary productivity (GPP; g C m-2 d-1) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall back to winter levels. Source: FLUXNET2015 US-Tw4, Dataset. https://doi.org/10.18140/FLX/1440108"

tw1_dd_2018 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2018 Daily GPP at Model Testing Site: US-Tw1 - Twitchell Wetland West Pond", 
       x = "Date", 
       y = "GPP (gC m-2 d-1)", 
       caption = str_wrap(caption_tw1, width = 120)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin  = margin(t = 10, r = 10, b = 20, l = 10)
  )


#let's clean up and save the data for model use
tw4_dd_2018_clean <- tw4_dd_2018 %>%
  select(TIMESTAMP, date, date_mdy, GPP_NT_VUT_REF, NEE_VUT_REF_QC)

#realized no site data in the dataset, let's add
tw4_dd_2018_clean <- tw4_dd_2018_clean %>%
  mutate(
    site = "US-Tw4",
    latitude = 38.1027,
    longitude = -121.6413
  )

#annnd reorder it
tw4_dd_2018_clean <- tw4_dd_2018_clean %>%
  select(site, latitude, longitude, TIMESTAMP, date, date_mdy, GPP_NT_VUT_REF, NEE_VUT_REF_QC)

write_csv(tw4_dd_2018_clean, "data/tw4_flux_dd_2018_clean.csv")


#do the same for tw1
tw1_dd_2018_clean <- tw1_dd_2018 %>%
  select(TIMESTAMP, date, date_mdy, GPP_NT_VUT_REF, NEE_VUT_REF_QC)

#realized no site data in the dataset, let's add
tw1_dd_2018_clean <- tw1_dd_2018_clean %>%
  mutate(
    site = "US-Tw1",
    latitude = 38.1074,
    longitude = -121.6469
  )

#annnd reorder it
tw1_dd_2018_clean <- tw1_dd_2018_clean %>%
  select(site, latitude, longitude, TIMESTAMP, date, date_mdy, GPP_NT_VUT_REF, NEE_VUT_REF_QC)

write_csv(tw1_dd_2018_clean, "data/tw1_flux_dd_2018_clean.csv")


#phew
#ok, let's get Tw1 point data in here
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

dfls_tw1 <- data.frame(
  task = "time_series",
  subtask = "US-Tw1_LS",
  latitude = c(38.1074),
  longitude = c(-121.6469),
  start = "2018-01-01",
  end = "2018-12-31",
  product = "HLSL30.020",
  layer = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B09", "B10", "B11", "Fmask")
)

dfs_tw1 <- data.frame(
  task = "time_series",
  subtask = "US-Tw1_S2",
  latitude = c(38.1074),
  longitude = c(-121.6469),
  start = "2018-01-01",
  end = "2018-12-31",
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
  path = "data/",
  verbose = TRUE
)

##You can also find your request here: https://appeears.earthdatacloud.nasa.gov/explore
## if your download fails, it will be stored there for you to get it
#^this is what happened to me every time

#-------------------------------------------------------------------#

#got it. now read it in
tw1_ls <- read_csv("data/raw/US_Tw1_LS/point-HLSL30-020-results.csv")

tw1_s2 <- read_csv("data/raw/US_Tw1_S2/point-HLSS30-020-results.csv")

#convert all the column names so they match up with each other. oh god. 
tw1_ls_converted <- tw1_ls %>%
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

tw1_s2_converted <- tw1_s2 %>%
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
tw1_lss2_bind <- bind_rows(tw1_ls_converted, tw1_s2_converted) %>%
  arrange(date, sensor)

#keep only the best days when there are dupes
tw1_lss2_bind_best <- tw1_lss2_bind %>%
  mutate(is_clear = (cloud == "0b0" & cshadow == "0b0")) %>%
  group_by(date) %>%
  arrange(desc(is_clear), sensor) %>%   
  slice(1) %>%
  ungroup()

#woooweee we're getting there. let's save all this data
write_csv(tw1_lss2_bind_best, "data/US-Tw1_LSS2_clean.csv")








#ok, going to use NDVI for my index
#compute NDVI for my 8 dates
ndvi_by_date <- tw4_LSS2_clean %>%
  mutate(date = as.Date(date)) %>%
  inner_join(dates8, by = "date") %>%   
  mutate(NDVI = (nir - B04) / (nir + B04)) %>%
  group_by(date, season) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop")

#do it for tw1 as well

#let's look
plot(ndvi_by_date)

ndvi_by_date %>%
  ggplot(aes(x = date, y = NDVI)) +
  geom_line() 

#hmmm, not exactly the curve we're looking for

gpp_by_date <- tw4_dd_gpp_qc %>%
  mutate(date = as.Date(date)) %>%
  semi_join(dates8, by = "date") %>%
  select(date, GPP_NT_VUT_REF)

gpp_by_date %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() 






