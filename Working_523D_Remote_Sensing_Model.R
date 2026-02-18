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
tw4_hh_NEE <- tw4_hh %>%
  select(TIMESTAMP_START, TIMESTAMP_END, NEE_VUT_REF, NEE_VUT_REF_QC, WD)

#oh, but I only need one year. 2018 is ~4 years after restoration, should be prime veg time
tw4_hh_NEE_2018 <- tw4_hh_NEE %>%
  filter(TIMESTAMP_START >= 201801010000, TIMESTAMP_START <= 201812312330)

#great, let's do the same for dd
tw4_NEE <- tw4_dd %>%
  select(TIMESTAMP, NEE_VUT_REF, NEE_VUT_REF_QC)

#only 2018
tw4_dd_NEE_2018 <- tw4_NEE %>%
  filter(TIMESTAMP >= 20180101, TIMESTAMP <= 20181231)

#let's look at it
plot(tw4_NEE_2018)

#a look at just the important stuff
tw4_NEE_2018 %>%
  ggplot(aes(x = TIMESTAMP, y = NEE_VUT_REF)) +
  geom_line() 

#ok, funky sine curve but it's there. 
#once I aggregate over +-four day averages the curve might be smoother
#should probably also remove low QC values
#but how to do without removing large sections
#let's get HH WD in shape first

angdiff <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

wsw_center <- 247.5
tolerance <- 30

#^thanks chat

#create date column to join with later
#p_wsw = percent of half-hours that were WSW
#n_hh = number of valid half-hours used

tw4_hh_2018_wsw_daily <- tw4_hh_NEE_2018 %>%
  mutate(
    ts = sprintf("%012.0f", TIMESTAMP_START),
    datetime = as.POSIXct(TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC"),
    date = as.Date(datetime)
  ) %>%
  filter(!is.na(WD)) %>%
  mutate(is_wsw = angdiff(WD, wsw_center) <= tolerance) %>%
  group_by(date) %>%
  summarise(
    n_hh = n(),
    p_wsw = mean(is_wsw),
    .groups = "drop"
  )

#^ugh this is not working

class(tw4_hh_NEE_2018$TIMESTAMP_START)





