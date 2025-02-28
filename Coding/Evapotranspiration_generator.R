#Extraction massively of nc files

#Loading libraries
library(raster)
library(terra)

#Parameters of geoprocessing
year_Eva= 2025
month_Eva= 1
output_folder <- paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Evapotranspiration/", year_Eva, "/ET_", year_Eva, sprintf("%02d", month_Eva), "/")

# Loading GIS and reference data
clip_base84 <- vect("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/base_clip_nc.shp")
area <- vect("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/Area.shp")

#Loading nc files
eva_ncs <- paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Evapotranspiration/Evapotranspiration_data/NC_", year_Eva, sprintf("%02d", month_Eva), "/")
nc_files <- list.files(path = eva_ncs)

#Robot starts
print(paste("Computing evapotranspiration sample for month", sprintf("%02d", month_Eva), "and year", (year_Eva)))
validator <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(validator) <- c("day", "minR", "maxR")

for (i in 1:length(nc_files)){

  # Read the NetCDF file
  nc_data <- rast(paste0(eva_ncs, nc_files[i]))
  
  #Geoprocessing
  eva_84 <- crop(nc_data[[1]], clip_base84, mask=TRUE)
  eva_pol <- project(eva_84, crs(area))
  template <- rast(ext(eva_pol), res=30)
  resampled_eva <- resample(eva_pol, template, method = "cubic")
  eva_pol_fin <- crop(resampled_eva, area, mask=TRUE)
  
  #validation
  eva_pol_fin[eva_pol_fin < 0] <- 0
  plot(eva_pol_fin)
  writeRaster(eva_pol_fin, paste0(output_folder, "ET_", sub(".*_(\\d{8})\\d{4}\\.nc$", "\\1", nc_files[i]), ".tif"), overwrite = TRUE)
  validator <- rbind(validator, list(day= sub(".*_(\\d{8})\\d{4}\\.nc$", "\\1", nc_files[i]), minR= min(values(eva_pol_fin), na.rm = TRUE), maxR= max(values(eva_pol_fin), na.rm = TRUE)))
  print(paste(i, "done for", sub(".*_(\\d{8})\\d{4}\\.nc$", "\\1", nc_files[i])))
}

validator



