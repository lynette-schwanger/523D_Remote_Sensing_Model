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

#stack tw1 rasters
all_files <- list.files(
  "data/raw/HLS_tw1_2018_rasters",
  pattern = "_aid0001_10N\\.tif$",
  full.names = TRUE
)

# extract DOY (YYYYDDD) and band from filenames
doy_id  <- sub(".*_doy([0-9]{7})_aid.*", "\\1", basename(all_files))
band_id <- sub(".*_(B[0-9A-Z]+)_doy.*", "\\1", basename(all_files))

# split files by date
files_by_doy <- split(all_files, doy_id)

# stack each date, enforce band order, and name layers
stacks_by_doy <- lapply(names(files_by_doy), function(d) {
  f <- files_by_doy[[d]]
  b <- sub(".*_(B[0-9A-Z]+)_doy.*", "\\1", basename(f))
  ord <- order(b)
  r <- rast(f[ord])
  names(r) <- b[ord]
  r
})
names(stacks_by_doy) <- names(files_by_doy)

# example: the stack for doy 2018020
stacks_by_doy[["2018020"]]

#export
out_dir <- "data/HLS_tw1_2018_stacks"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (d in names(stacks_by_doy)) {
  r <- stacks_by_doy[[d]]
  out_file <- file.path(out_dir, paste0("HLSS30_tw1_stack_doy", d, ".tif"))
  writeRaster(r, out_file, overwrite = TRUE)
}




#rasters for tw4 
all_files_tw4 <- list.files(
  "data/raw/HLS_tw4_2018_rasters",
  pattern = "_aid0001_10N\\.tif$",
  full.names = TRUE
)


# extract DOY (YYYYDDD) and band from filenames
doy_id  <- sub(".*_doy([0-9]{7})_aid.*", "\\1", basename(all_files_tw4))
band_id <- sub(".*_(B[0-9A-Z]+)_doy.*", "\\1", basename(all_files_tw4))

# split files by date
files_by_doy <- split(all_files_tw4, doy_id)

# stack each date, enforce band order, and name layers
stacks_by_doy <- lapply(names(files_by_doy), function(d) {
  f <- files_by_doy[[d]]
  b <- sub(".*_(B[0-9A-Z]+)_doy.*", "\\1", basename(f))
  ord <- order(b)
  r <- rast(f[ord])
  names(r) <- b[ord]
  r
})
names(stacks_by_doy) <- names(files_by_doy)

# example: the stack for doy 2018020
stacks_by_doy[["2018020"]]

#export
out_dir <- "data/HLS_tw4_2018_stacks"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (d in names(stacks_by_doy)) {
  r <- stacks_by_doy[[d]]
  out_file <- file.path(out_dir, paste0("HLSS30_tw4_stack_doy", d, ".tif"))
  writeRaster(r, out_file, overwrite = TRUE)
}


names(stacks_by_doy[["2018020"]]) 

#ok shoot, I need to reorder and rename
#ugh could definitely create a function to do this but oh welllllll
#start with tw1

in_dir  <- "data/HLS_tw1_2018_stacks"         # folder with your existing stacks
out_dir <- "data/HLS_tw1_2018_stacks_renamed" # new folder for renamed stacks
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)


keep <- c("B01","B02","B03","B04","B8A","B11","B12")
new_names <- c("B01","B02","B03","B04","nir","swir1","swir2")

for (f in tif_files) {
  r <- rast(f)
  
  # reorder and rename
  r2 <- r[[keep]]
  names(r2) <- new_names
  
# write new files with new name
  out_file <- file.path(out_dir, sub("\\.tif$", "_renamed.tif", basename(f)))
  writeRaster(r2, out_file, overwrite = TRUE)
}

r <- rast("data/HLS_tw1_2018_stacks_renamed/HLSS30_tw1_stack_doy2018020_renamed.tif")

names(r)

in_dir  <- "data/HLS_tw4_2018_stacks"         # folder with your existing stacks
out_dir <- "data/HLS_tw4_2018_stacks_renamed" # new folder for renamed stacks
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

tif_files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)


keep <- c("B01","B02","B03","B04","B8A","B11","B12")
new_names <- c("B01","B02","B03","B04","nir","swir1","swir2")

for (f in tif_files) {
  r <- rast(f)
  
  #reorder and rename
  r2 <- r[[keep]]
  names(r2) <- new_names
  
  #write with "_renamed" added before .tif
  out_file <- file.path(out_dir, sub("\\.tif$", "_renamed.tif", basename(f)))
  writeRaster(r2, out_file, overwrite = TRUE)
}


r <- rast("data/HLS_tw4_2018_stacks_renamed/HLSS30_tw4_stack_doy2018020_renamed.tif")

names(r)

plot(r)
#^cool

#YAYAYAYAY
#ok, now I need to compute NDMI and EVI for the rasters
#compute NDMI and EVI, here are the formulas I used: 
(NDMI = (nir - swir1) / (nir + swir1))

#compute EVI
(EVI = 2.5 * (nir - B04) / (nir + 6 * B04 - 7.5 * B02 + 1))

