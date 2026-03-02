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
  labs(title = "2014-2017 Daily GPP at US-Tw1", 
       x = "Date", 
       y = "GPP (gC m-² d-¹)", 
       caption = str_wrap(
         "Daily GPP at US-Tw1 from 2014-2017. Daily gross primary productivity (GPP; gC m-² d-¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw1 Twitchell Wetland West Pond, Dataset. https://doi.org/10.18140/FLX/1440111",
         width = 140
       )
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(t = 5.5, r = 5.5, b = 20, l = 5.5)  # more space at bottom
  )


tw4_dd_2014_2017 %>%
  ggplot(aes(x = date, y = GPP_NT_VUT_REF)) +
  geom_line() +
  labs(title = "2014-2017 Daily GPP at US-Tw4", 
       x = "Date", 
       y = "GPP (gC m-² d-¹)", 
       caption = str_wrap("Daily GPP at US-Tw4 from 2014-2017. Daily gross primary productivity (GPP; gC m-² d-¹) shows strong seasonality, remaining low through winter, increasing rapidly in spring, peaking in mid-summer, and declining through fall and winter. Source: FLUXNET2015 US-Tw4 Twitchell East End Wetland, Dataset. https://doi.org/10.18140/FLX/1440111",
       width = 140
  )
) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(t = 5.5, r = 5.5, b = 20, l = 5.5)  # more space at bottom
  )

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
tw1_lss2 <- bind_rows(tw1_ls_converted, tw1_s2_converted) %>%
  arrange(date, sensor)

tw4_lss2 <- bind_rows(tw4_ls_converted, tw4_s2_converted) %>%
  arrange(date, sensor)

#keep only the best days when there are dupes
tw1_lss2 <- tw1_lss2 %>%
  mutate(is_clear = (cloud == "0b0" & cshadow == "0b0")) %>%
  group_by(date) %>%
  arrange(desc(is_clear), sensor) %>%   
  slice(1) %>%
  ungroup()

tw4_lss2 <- tw4_lss2 %>%
  mutate(is_clear = (cloud == "0b0" & cshadow == "0b0")) %>%
  group_by(date) %>%
  arrange(desc(is_clear), sensor) %>%   
  slice(1) %>%
  ungroup()

#keep only cloud free days
tw1_lss2 <- tw1_lss2 %>%
  filter(is_clear == TRUE)

tw4_lss2 <- tw4_lss2 %>%
  filter(is_clear == TRUE)

class(tw1_lss2$date)

#split by train/test years
tw1_lss2_2014_2015 <- tw1_lss2 %>%
  filter(date >= ymd("2014-01-01"), date <= ymd("2015-12-31"))

tw1_lss2_2016_2017 <- tw1_lss2 %>%
  filter(date >= ymd("2016-01-01"), date <= ymd("2017-12-31"))

tw4_lss2_2014_2015 <- tw4_lss2 %>%
  filter(date >= ymd("2014-01-01"), date <= ymd("2015-12-31"))

tw4_lss2_2016_2017 <- tw4_lss2 %>%
  filter(date >= ymd("2016-01-01"), date <= ymd("2017-12-31"))

#now join
train_hls <- bind_rows(tw1_lss2_2016_2017, tw4_lss2_2014_2015)

test_hls <- bind_rows(tw1_lss2_2014_2015, tw4_lss2_2016_2017)


#join with flux data to finalize train/test datasets
#read in cleaned flux data
train_flux <- read_csv("data/train_flux_clean.csv")

test_flux <- read_csv("data/test_flux_clean.csv")

class(train_flux$date)

#oh but I need a site column in my HLS dataset that matches the flux dataset
train_hls <- train_hls %>%
  mutate(
    site = str_extract(sensor, "US-Tw\\d+")
  )

test_hls <- test_hls %>%
  mutate(
    site = str_extract(sensor, "US-Tw\\d+")
  )

#ok now join
train_df <- train_flux %>%
  inner_join(train_hls, by = c("site", "date"))

test_df <- test_flux %>%
  inner_join(test_hls, by = c("site", "date"))


#ok now compute NDVI
train_df <- train_df %>%
  mutate(NDVI = (nir - B04) / (nir + B04))

test_df <- test_df %>%
  mutate(NDVI = (nir - B04) / (nir + B04))

#compute NDMI
train_df <- train_df %>%
  mutate(NDMI = (nir - swir1) / (nir + swir1))

test_df <- test_df %>%
  mutate(NDMI = (nir - swir1) / (nir + swir1))

#compute EVI
train_df <- train_df %>%
  mutate(EVI = 2.5 * (nir - B04) / (nir + 6 * B04 - 7.5 * B02 + 1))

test_df <- test_df %>%
  mutate(EVI = 2.5 * (nir - B04) / (nir + 6 * B04 - 7.5 * B02 + 1))

#model time
#need to use Gaussian family for the gam becuase I have negative GPP values
gam_ndvi <- gam(
  GPP_NT_VUT_REF ~ s(NDVI, k = 30),
  data   = train_df,
  family = gaussian(),
  method = "REML")

summary(gam_ndvi)

p_train <- predict(gam_ndvi, newdata = train_df, type = "response")

rmse_train <- sqrt(mean((train_df$GPP_NT_VUT_REF - p_train)^2, na.rm = TRUE))
rmse_train

#NDVI RMSE = 2.134392

#train using NDMI
gam_ndmi <- gam(GPP_NT_VUT_REF ~ s(NDMI, k = 30),
                data = train_df, family = gaussian(), method = "REML")

p_ndmi <- predict(gam_ndmi, newdata = train_df, type = "response")
rmse_ndmi <- sqrt(mean((train_df$GPP_NT_VUT_REF - p_ndmi)^2, na.rm = TRUE))
rmse_ndmi

#NDMI RMSE = 2.471366

#train using EVI
gam_evi <- gam(GPP_NT_VUT_REF ~ s(EVI, k = 30),
               data = train_df, family = gaussian(), method = "REML")

p_evi <- predict(gam_evi, newdata = train_df, type = "response")
rmse_evi <- sqrt(mean((train_df$GPP_NT_VUT_REF - p_evi)^2, na.rm = TRUE))
rmse_evi

#EVI RMSE = 1.78255


gam_evi_ndvi <- gam(GPP_NT_VUT_REF ~ s(EVI, k=30) + s(NDVI, k=30),
                    data=train_df, family=gaussian(), method="REML")

gam_evi_ndmi <- gam(GPP_NT_VUT_REF ~ s(EVI, k=30) + s(NDMI, k=30),
                    data=train_df, family=gaussian(), method="REML")

gam_all3 <- gam(GPP_NT_VUT_REF ~ s(EVI, k=30) + s(NDVI, k=30) + s(NDMI, k=30),
                data=train_df, family=gaussian(), method="REML")

rmse_fun <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm=TRUE))
#^is it fun? IS IT

rmse_evi_ndvi <- rmse(train_df$GPP_NT_VUT_REF, predict(gam_evi_ndvi, train_df, type="response"))
rmse_evi_ndmi <- rmse(train_df$GPP_NT_VUT_REF, predict(gam_evi_ndmi, train_df, type="response"))
rmse_all3     <- rmse(train_df$GPP_NT_VUT_REF, predict(gam_all3,     train_df, type="response"))

data.frame(
  model = c("EVI", "EVI+NDVI", "EVI+NDMI", "EVI+NDVI+NDMI"),
  rmse_train = c(1.78255, rmse_evi_ndvi, rmse_evi_ndmi, rmse_all3)
) |> dplyr::arrange(rmse_train)

plot(gam_all3, pages = 1, scheme = 1, shade = TRUE)

m <- gam_all3  # <- change to your model name if needed

# baseline performance on training data
p_base <- predict(m, newdata = train_df, type = "response")
rmse_base <- rmse(train_df$GPP_NT_VUT_REF, p_base)

set.seed(1)

vars <- c("EVI", "NDVI", "NDMI")

imp <- lapply(vars, function(v){
  df_perm <- train_df
  df_perm[[v]] <- sample(df_perm[[v]])  # shuffle that column
  
  p_perm <- predict(m, newdata = df_perm, type = "response")
  rmse_perm <- rmse(train_df$GPP_NT_VUT_REF, p_perm)
  
  data.frame(variable = v, delta_rmse = rmse_perm - rmse_base)
}) |> bind_rows() |> arrange(desc(delta_rmse))

imp

ggplot(imp, aes(x = reorder(variable, delta_rmse), y = delta_rmse)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Increase in RMSE (higher = more important)",
    title = "Variable Importance (training RMSE) for GAM"
  )

#__________________________________________________________________#
#chat code I don't understand that gets me the variable importance plot
#chat based it on the R scripts provided by O'Connel

m <- gam_all3   # <-- or whatever your final GAM object is
vars <- c("EVI", "NDVI", "NDMI")

# baseline RMSE
p_base <- predict(m, newdata = train_df, type = "response")
rmse_base <- rmse(train_df$GPP_NT_VUT_REF, p_base)

set.seed(1)
B <- 50  # number of shuffles per variable

imp_rep <- expand.grid(variable = vars, rep = 1:B) |>
  dplyr::as_tibble() |>
  dplyr::rowwise() |>
  dplyr::mutate(
    delta_rmse = {
      dfp <- train_df
      dfp[[variable]] <- sample(dfp[[variable]])  # scramble one variable
      p_perm <- predict(m, newdata = dfp, type = "response")
      rmse(train_df$GPP_NT_VUT_REF, p_perm) - rmse_base
    }
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    m  = mean(delta_rmse),
    se = sd(delta_rmse)/sqrt(B),
    .groups = "drop"
  ) |>
  dplyr::arrange(m)

imp_rep

centers <- barplot(imp_rep$m,
                   horiz = TRUE,
                   names.arg = imp_rep$variable,
                   xlim = c(0, max(imp_rep$m + imp_rep$se)),
                   xlab = "Increase in RMSE",
                   main = "Variable Importance")

arrows(imp_rep$m - imp_rep$se, centers,
       imp_rep$m + imp_rep$se, centers,
       lwd = 1.5, angle = 90, code = 3, length = 0.05)


#______________________________________________________________#

#ok well it did the variable importance graph

#create model using NDMI and EVI
#oh well first actually let's plot each variable with RMSE and create plots for my final presentation

#function to plot each
plot_obs_pred <- function(obs, pred, rmse, main_title, caption_text){
  lims <- range(c(obs, pred), na.rm = TRUE)
  
  plot(pred, obs,
       xlim = lims, ylim = lims,
       xlab = expression("Predicted GPP (gC m"^-2*" d"^-1*")"),
       ylab = expression("Observed GPP (gC m"^-2*" d"^-1*")"),
       main = main_title,
       pch = 1)
  
  abline(0, 1, lty = 2)
  
  usr <- par("usr")
  text(x = usr[1] + 0.12 * diff(usr[1:2]),
       y = usr[4] - 0.10 * diff(usr[3:4]),
       labels = paste0("RMSE = ", sprintf("%.2f", rmse)))
  
  mtext(side = 1, line = 5, adj = 0, cex = 0.8, text = caption_text)
}


#NDVI MODEL
gam_ndvi <- gam(GPP_NT_VUT_REF ~ s(NDVI, k = 30),
                data = train_df, family = gaussian(), method = "REML")
p_ndvi <- predict(gam_ndvi, newdata = train_df, type = "response")
rmse_ndvi <- rmse_fun(train_df$GPP_NT_VUT_REF, p_ndvi)

plot_obs_pred(
  obs = train_df$GPP_NT_VUT_REF,
  pred = p_ndvi,
  rmse = rmse_ndvi,
  main_title = "GPP predicted from NDVI",
  caption_text = paste0("Observed vs. predicted daily GPP using NDVI computed from HLS reflectance. ",
                        "Training RMSE = ", sprintf("%.2f", rmse_ndvi), " gC m-² d-¹.")
)


#NDMI MODEL
gam_ndmi <- gam(GPP_NT_VUT_REF ~ s(NDMI, k = 30),
                data = train_df, family = gaussian(), method = "REML")
p_ndmi <- predict(gam_ndmi, newdata = train_df, type = "response")
rmse_ndmi <- rmse_fun(train_df$GPP_NT_VUT_REF, p_ndmi)

plot_obs_pred(
  obs = train_df$GPP_NT_VUT_REF,
  pred = p_ndmi,
  rmse = rmse_ndmi,
  main_title = "GPP predicted from NDMI",
  caption_text = paste0("Observed vs. predicted daily GPP using NDMI computed from HLS reflectance. ",
                        "Training RMSE = ", sprintf("%.2f", rmse_ndmi), " gC m-² d-¹.")
)

#EVI MODEL
gam_evi <- gam(GPP_NT_VUT_REF ~ s(EVI, k = 30),
               data = train_df, family = gaussian(), method = "REML")
p_evi <- predict(gam_evi, newdata = train_df, type = "response")
rmse_evi <- rmse_fun(train_df$GPP_NT_VUT_REF, p_evi)

plot_obs_pred(
  obs = train_df$GPP_NT_VUT_REF,
  pred = p_evi,
  rmse = rmse_evi,
  main_title = "GPP predicted from EVI",
  caption_text = paste0("Observed vs. predicted daily GPP using EVI computed from HLS reflectance. ",
                        "Training RMSE = ", sprintf("%.2f", rmse_evi), " gC m-² d-¹.")
)


#WOOOOOO
#then I'll show my variable importance plot I did earlier, confirming I'm going with EVI and NDVI
#plot RMSE for final model
gam_evi_ndmi <- gam(
  GPP_NT_VUT_REF ~ s(EVI, k = 30) + s(NDMI, k = 30),
  data = train_df,
  family = gaussian(),
  method = "REML"
)

p_final_train <- predict(gam_evi_ndmi, newdata = train_df, type = "response")
rmse_final_train <- rmse_fun(train_df$GPP_NT_VUT_REF, p_final_train)

plot_obs_pred(
  obs = train_df$GPP_NT_VUT_REF,
  pred = p_final_train,
  rmse = rmse_final_train,
  main_title = "GPP predicted from EVI and NDMI",
  caption_text = paste0(
    "Observed vs. predicted daily GPP using EVI and NDMI computed from HLS reflectance. ",
    "Training RMSE = ", sprintf("%.2f", rmse_final_train), " gC m-² d-¹."
  )
)


#LET'S SEE HOW TEST DATA DOES...
p_final_test <- predict(gam_evi_ndmi, newdata = test_df, type = "response")

# Test RMSE
rmse_final_test <- rmse_fun(test_df$GPP_NT_VUT_REF, p_final_test)
rmse_final_test

plot_obs_pred_2 <- function(obs, pred, rmse, main_title, caption_text){
  
  # Increase bottom margin (mar = c(bottom, left, top, right))
  par(mar = c(8, 5, 4, 2) + 0.1)
  
  lims <- range(c(obs, pred), na.rm = TRUE)
  
  plot(pred, obs,
       xlim = lims, ylim = lims,
       xlab = expression("Predicted GPP (gC m"^-2*" d"^-1*")"),
       ylab = expression("Observed GPP (gC m"^-2*" d"^-1*")"),
       main = main_title,
       pch = 1)
  
  abline(0, 1, lty = 2)
  
  usr <- par("usr")
  text(x = usr[1] + 0.12 * diff(usr[1:2]),
       y = usr[4] - 0.10 * diff(usr[3:4]),
       labels = paste0("RMSE = ", sprintf("%.2f", rmse)))
  
  mtext(side = 1, line = 5, adj = 0, cex = 0.8, text = caption_text)
}


# Plot (Observed vs Predicted) for TEST
plot_obs_pred_2(
  obs = test_df$GPP_NT_VUT_REF,
  pred = p_final_test,
  rmse = rmse_final_test,
  main_title = "GPP predicted from EVI and NDMI using Testing Data",
  caption_text = paste0(
    "Observed vs. predicted daily GPP on the test dataset using EVI and NDMI computed from HLS reflectance.\n",
    "Test RMSE = ", sprintf("%.2f", rmse_final_test), " gC m-² d-¹."
  )
)

r2_test <- 1 - sum((obs - pred)^2, na.rm = TRUE) /
  sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)

#_____________
obs  <- test_df$GPP_NT_VUT_REF
pred <- p_final_test   # <- your test predictions from predict(gam_evi_ndmi, newdata=test_df, ...)

# 2) Metrics
rmse_test <- sqrt(mean((obs - pred)^2, na.rm = TRUE))

lims <- range(c(obs, pred), na.rm = TRUE)

par(mar = c(8, 5, 4, 2) + 0.1)  # more bottom margin for caption

plot(pred, obs,
     xlim = lims, ylim = lims,
     xlab = expression("Predicted GPP (gC m"^-2*" d"^-1*")"),
     ylab = expression("Observed GPP (gC m"^-2*" d"^-1*")"),
     main = "GPP predicted from EVI and NDMI using Testing Data",
     pch = 1)

abline(0, 1, lty = 2)

# 4) Print RMSE + R^2 on plot (top-left)
usr <- par("usr")
text(x = usr[1] + 0.12 * diff(usr[1:2]),
     y = usr[4] - 0.10 * diff(usr[3:4]),
     labels = paste0("RMSE = ", sprintf("%.2f", rmse_test)))

text(x = usr[1] + 0.12 * diff(usr[1:2]),
     y = usr[4] - 0.18 * diff(usr[3:4]),
     labels = paste0("R\u00B2 = ", sprintf("%.2f", r2_test)))

# 5) Caption (forced to two lines)
caption_text <- paste0(
  "Observed vs. predicted daily GPP on the test dataset using EVI and NDMI computed from HLS reflectance.\n",
  "Test RMSE = ", sprintf("%.2f", rmse_test), " gC m-² d-¹ \n",
  "Test R\u00B2 = 0.65"
)

mtext(side = 1, line = 6, adj = 0, cex = 0.8, text = caption_text)

#__________



#booo, not as good as I hoped
#oh well, moving on
#model diagnostics?

obs <- test_df$GPP_NT_VUT_REF
pred <- p_final_test
res  <- obs - pred


# Residuals vs predicted
plot(pred, res,
     xlab = expression("Predicted GPP (gC m"^-2*" d"^-1*")"),
     ylab = expression("Residual (Observed - Predicted)"),
     main = "Test Diagnostics: Residuals vs Predicted",
     pch = 1)
abline(h = 0, lty = 2)

bias <- mean(res, na.rm = TRUE)

caption_text <- "On average, observed GPP is 0.86 gC m-² d-¹ higher than the model's predicted GPP."
  
plot(pred, res,
     xlab = expression("Predicted GPP (gC m"^-2*" d"^-1*")"),
     ylab = expression("Residual (Observed - Predicted)"),
     main = "Model Diagnostics: Residuals vs Predicted on Testing Dataset",
     pch = 1)
abline(h = 0, lty = 2)

# print bias on the plot (top-left)
usr <- par("usr")
text(x = usr[1] + 0.15 * diff(usr[1:2]),
     y = usr[4] - 0.15 * diff(usr[3:4]),
     labels = paste0("Bias (mean residual) = ", sprintf("%.2f", bias)))

mtext(side = 1, line = 4, adj = 0, cex = 0.8, text = caption_text)

