source("Setup_523D.R")

setwd("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/523D_Remote_Sensing_Model/523D_Remote_Sensing_Model")

tw1_hls <- read_csv("data/US-Tw1_LSS2_clean.csv")

tw1_flux <- read_csv("data/tw1_flux_dd_2018_clean.csv")

tw4_hls <- read_csv("data/US-Tw4_LSS2_clean.csv")

tw4_flux <- read_csv("data/tw4_flux_dd_2018_clean.csv")

#create a tibble for just my dates
dates8_tw4 <- tibble(
  date = as.Date(c("2018-01-25",
                   "2018-12-11",
                   "2018-03-29",
                   "2018-05-15",
                   "2018-06-19",
                   "2018-08-18",
                   "2018-09-22",
                   "2018-10-17")),
  season = c("Winter","Winter","Spring","Spring","Summer","Summer","Fall","Fall")
)

dates8_tw1 <- tibble(
  date = as.Date(c("2018-01-25",
                   "2018-12-11",
                   "2018-03-29",
                   "2018-05-15",
                   "2018-06-19",
                   "2018-08-18",
                   "2018-09-22",
                   "2018-10-17")),
  season = c("Winter","Winter","Spring","Spring","Summer","Summer","Fall","Fall")
)

#join data together and compute NDVI to create datasets for training and testing
tw4_ndvi_8 <- tw4_hls %>%
  filter(is_clear == TRUE) %>%
  semi_join(dates8_tw4, by = "date") %>%
  mutate(NDVI = (nir - B04) / (nir + B04)) %>%
  group_by(date) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop") %>%
  left_join(dates8_tw4, by = "date")

tw1_ndvi_8 <- tw1_hls %>%
  filter(is_clear == TRUE) %>%
  semi_join(dates8_tw1, by = "date") %>%
  mutate(NDVI = (nir - B04) / (nir + B04)) %>%
  group_by(date) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop") %>%
  left_join(dates8_tw1, by = "date")

train_tw4_8 <- tw4_flux %>%
  select(date, GPP_NT_VUT_REF) %>%        
  inner_join(tw4_ndvi_8, by = "date") %>%
  drop_na(GPP_NT_VUT_REF, NDVI)

test_tw1_8 <- tw1_flux %>%
  select(date, GPP_NT_VUT_REF) %>%
  inner_join(tw1_ndvi_8, by = "date") %>%
  drop_na(GPP_NT_VUT_REF, NDVI)

#ok, model time
mod <- lm(GPP_NT_VUT_REF ~ NDVI, data = train_tw4_8)

summary(mod) 

train_tw4_8 <- train_tw4_8 %>%
  mutate(pred_GPP = predict(mod, newdata = train_tw4_8))

plot(train_tw4_8$NDVI, train_tw4_8$GPP_NT_VUT_REF, 
     xlab= "NDVI", ylab ="GPP (gC m-2 d-1)", pch=19
)  

#hmm, not sure what I'm looking at 
#I wonder if it's ok to train on more data than just my 8 dates for raster prediction? 
#looking closer at simpleworkflow_solution I think so
#lets start over
tw4_ndvi <- tw4_hls %>%
  filter(is_clear == TRUE) %>%
  mutate(NDVI = (nir - B04) / (nir + B04)) %>%
  group_by(date) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop")

tw4_model_df <- tw4_flux %>%
  inner_join(tw4_ndvi, by = "date")

tw1_ndvi <- tw1_hls %>%
  filter(is_clear == TRUE) %>%
  mutate(NDVI = (nir - B04) / (nir + B04)) %>%
  group_by(date) %>%
  summarise(NDVI = mean(NDVI, na.rm = TRUE), .groups = "drop")

tw1_model_df <- tw1_flux %>%
  inner_join(tw1_ndvi, by = "date")

#ok, now model time fr
mod <- lm(GPP_NT_VUT_REF ~ NDVI, data = tw4_model_df)

predict_train <- predict(mod, newdata = tw4_model_df)

rmse_train <- sqrt(sum((tw4_model_df$GPP_NT_VUT_REF - predict_train)^2) / length(predict_train))
rmse_train

par(mar = c(8, 5, 4, 2) + 0.1)

plot(predict_train, tw4_model_df$GPP_NT_VUT_REF,
     main = "US-Tw4 Training Model Fit: GPP predicted from NDVI",
     xlab = "Predicted GPP (gC m-2 d-1)",
     ylab = "Observed GPP (gC m-2 d-1)")
abline(0, 1, lty = 2)

mtext(side = 3, adj = 0.05, line = -2,
      paste0("RMSE = ", round(rmse_train, 2)))

caption <- "Figure 3a: Observed vs. predicted daily GPP at US-Tw4 from a linear regression model using NDVI computed from HLS reflectance. RMSE for the training dataset is 3.13 gC m-2 d-1."

mtext(
  side = 1, adj = 0, line = 6,
  text = paste(strwrap(caption, width = 130), collapse = "\n"),
  cex = 0.8
)

#onto testing
predict_test <- predict(mod, newdata = tw1_model_df)

rmse_test <- sqrt(sum((tw1_model_df$GPP_NT_VUT_REF - predict_test)^2) / length(predict_test))
rmse_test

par(mar = c(8, 5, 4, 2) + 0.1)

plot(predict_test, tw1_model_df$GPP_NT_VUT_REF,
     main = "US-Tw1 Model Test Site: GPP predicted from NDVI",
     xlab = "Predicted GPP (gC m-2 d-1)",
     ylab = "Observed GPP (gC m-2 d-1)")
abline(0, 1, lty = 2)

mtext(side = 3, adj = 0.05, line = -2,
      paste0("RMSE = ", round(rmse_test, 2)))

caption <- "Figure 3b: Observed vs. predicted daily GPP at test site US-Tw1 from a linear regression model trained at US-Tw4 using NDVI computed from HLS reflectance. RMSE for the training dataset is 2.64 gC m-2 d-1."

mtext(
  side = 1, adj = 0, line = 6,
  text = paste(strwrap(caption, width = 130), collapse = "\n"),
  cex = 0.8
)
