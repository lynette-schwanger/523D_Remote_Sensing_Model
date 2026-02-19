## API raster gathering Method 1: appeears package
## this package works with the NASA earthdata set of datasets at https://www.earthdata.nasa.gov/
## this package takes advantage of the NASA earthdata API called AppEEARS
## see https://appeears.earthdatacloud.nasa.gov/
## APIs are intended to help programmers access data and functions already 
##  available on someone else's computer, in this case a NASA server.
##  the R appeears package is code someone else wrote to access the 
##  NASA earthdata Appeears API, but you could also code this yourself

##load libraries
library(tidyverse); library(terra); library(sf); library(appeears); #library(keyring)

library(keyring)

source("Setup_523D.R")
##set wd to project folder
setwd("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/523D_Remote_Sensing_Model/523D_Remote_Sensing_Model")


## appeears uses your NASA earthdata credentials to pull the data from NASA servers directly
## the API also allows you to subset the data before downloading through: Space, Time, and Layer (Spectral)


## prepare for NASA earth data login
## make a file for your credentials to use in the code below
## note- never hard encode things like your user name and password into scripts 
## you need to make the file below yourself. Create a plain text file that you save as "NASAearthdata_creds.csv"
## the file should contain the following lines, where you change the second line to your actual user name and password
## user name is your nasa earthdata user name, NOT YOUR EMAIL
#user, pwd
#yourUserName, YourPassword

##remember to change the path below to where you put your file
credentials<-read.csv("/Users/lynetteschwanger/Desktop/CSU/Spring_2026_Classes/ESS_523D_Environmental_Data_Science_Applications_Reproducible_Remote_Sensing_Models/API_Scripts/NASAearthdata_creds.csv")

head(credentials)

## setup user name and password with the appeears package; This creates a web cookies token that remembers your 
## credentials for certain period of time; this is similar to when you log in to 
## any website and can use pages within the website for a bit, because your authentication is remembered
## depending on your computer, it may also make a new encrypted keyring file to store your credentials on your computer  
##  recommendation:use the same password for your keyring and NASA earthdata account
##  for user name, use your NASA earthdata username, DO NOT USE YOUR EMAIL. 

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

##see the products we can gather with this package
rs_products()

## this is easier to read on the NASA website, so see also:
##  https://appeears.earthdatacloud.nasa.gov/products

## let's look at one of the products and the layers (bands) it has available
## construction of the product string is the product column and the version column at the url above 

rs_layers("HLSL30.020")

hls<-rs_layers("HLSL30.020")
names(hls)
hls$Layer
##there are two types of spatial tasks allowed by this API- point and area
##the call below sets up extraction of a point (specified by lat and long) from a time series from start to end dates
## of the product and band layers specified
## see rs_layers("myproduct") to see the names of band layers you can request
## below we're requesting two points, specified in lat long
## if you have a shapefile or any other spatial object, you can use R code to covert to lat long and pull out the coordinates to feed the function below
## note that only one start and end date is recognized by the request. Points with different start and end dates are not honored, at least in my tests 
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

##look at what we got
files<-list.files("data/point")
files
hls<-read_csv("data/point/point-HLSL30-020-results.csv")
names(hls)

##Note that HLS has both landsat 8/9 and sentinel-2 bands, above we requested only the landsat part of the collection;
## you can get the Sentinel part of the collect by resubmitting the request and asking for HLSS30.020 product
## See https://lpdaac.usgs.gov/documents/1698/HLS_User_Guide_V2.pdf for band name equivalencies among the 2 products
## as long as the bands are the same wavelengths, you can safely combine them into a single column and create timeseries
##  Thus NIR band is B05 in HLSL30 and is B08A in HLSS30, but you can put them both in the same NIR column; 
##  Same goes for SWIR1 which is B06 in L30 and B11 in S30, SWIR2 is B07 in L30 and B12 in S30
##  Note that cloud masking and pixel scaling was handled for us, and bad pixels were filled in with -9999

##On your own: modify the task above to gather a product that is useful to you
## use points that you have on hand for this point extraction, most likely from a shapefile
## create a vector lats and another vector of longs to put in df point request
## functions you may need 
## library(sf):  crs(): print the coordinate system of the shapefile; 
##               st_coordinates(): retrieve the x, y coordinates of the shapefile
##               st_transform(): convert from one coordinate system to another; In this case we need the data in uprojected lat/long coordinates
##               proj4string for unprojected lat long coordinates in WGS84 datum: "EPSG:4326", "4326", or "+proj=longlat +datum=WGS84 +no_defs" depending on how the function takes it


### Next demo: Retrieve rasters from the API, subset to bands and regions of interest ####
##polygon task
## below we're working with Landsat 8 ARD data, and we request all of the spectral bands for images in Jan 2019

##check layer names for Landsat 8
rs_layers("L08.002")

df <- data.frame(
  task = "time_series",
  subtask = "subtask",
  latitude = 42.5378,
  longitude = -72.1715,
  start = "2019-01-01",
  end = "2019-01-31",
  product = "L08.002",
  layer=c("SR_B1", "SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")
  
)

##create an ROI to use; this can be any shapefile you have on hand; we'll use the one below in this demo
# load the north carolina demo data
# included in the {sf} package
# and only retain Camden county
roi <- st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE) |>
  filter(
    NAME == "Camden"
  )
plot(roi)

# build the area based request/task
# rename the task name so data will
# be saved in the "polygon" folder
# as defined by the task name
df$task <- "polygon"
task <- rs_build_task(
  df = df,
  roi = roi,
  format = "geotiff"
)

# request the task to be executed
##below, path is a file path; we've already set our working directory, so now can just mention the data folder
##credentials$user loads in the user string from our credentials file
rs_request(
  request = task,
  user = credentials$user,
  transfer = TRUE,
  path = "data/",
  verbose = TRUE
)

##this takes a minute; while you're waiting, go to your appEEARS account page and see your requests
## https://appeears.earthdatacloud.nasa.gov/explore
## the request disappears once it's finished, but will be there while NASA works on your dataset

##look at what we got
files<-list.files("data/polygon")
files

## as we've done before, let's build raster stacks for each date, composed of cloud free pixels
## grab the QA files for cloud filtering
qafiles<-files[grep("QA_PIXEL", files)]
##get just the spectral bands
files<-files[grep("SR_B", files)]

##pull out the dates from the file names so that we can use these to id files to stack
dates<-stringr::str_split(files, "_", simplify = T)
dates
dates<-dates[,5]
##subset to just the unique dates
dates<-unique(dates)

##lets build a raster stack of one of the dates
r<-rast(paste0("data/polygon/",files[grep(dates[1], files)]))
qa<-rast(paste0("data/polygon/",qafiles[grep(dates[1], qafiles)]))

##look at the range of data; since it's between 0-1, scaling and offset has been handled by the ARD dataset for us
plot(r)
##false color infrared plot, with NIR as red channel, RED as green channel, and GREEN as blue channel
terra::plotRGB(r, r=5, g=4, b=3, stretch="hist")

##but we still need to mask out bad pixels from clouds, cloud shadows and other factors
plot(qa)
table(values(qa))
x<-table(values(qa))
qa_vals<-names(x)
qa_vals
##lets do a quick check of the qa/qc for these pixels, remember that you start from the right at bit 0
## strings that start from the right with many zeros, are good in general; the code is 0/1, with 0= not the thing; 1= yes, the thing
## for specifics on bit specifications, see https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2?hl=en#bands
## at the URL above, expand the QA_PIXEL table entry to see bit specifications
qa_bits<-R.utils::intToBin(qa_vals)
qa_bits
##  21824= not water, not clouds, 21952 and 21888 = water;  30048 is snow/ice; we can keep these; 
##  Check my work and add other values here as needed
#  inverse =T means to keep the pixels that ARE in the mask values, without that we say what to remove
r<-mask(r,qa, maskvalues=c(21824, 21952, 21888, 30048), inverse=T)

terra::plotRGB(r, r=5, g=4, b=3, stretch="hist")

## you can take these code lines to make a function for processing the appEEARS output for your dataset of choice

## on your own, convert the code skeletons above into a sensible workflow for your dataset; 
##   try one of the harmonized Landsat-Sentinel-2 datasets, 
##   or download the surface temp bands of the Landsat data of your choice
## we demo'd above extraction of raster data at points of interest (eg- code script basic_raster_extraction.r)
## we also demo'd above downloading, stacking and masking raster bands, which can be used for site-wide prediction



