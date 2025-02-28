library(raster)
library(terra)
library(sf)
library(dplyr)  
library(automap)
library(gstat)

#Parameters of geoprocessing
year_Ro= 2025
month_Ro= 1
diario= 31
output_folder <- paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Runoff/", year_Ro, "/RO_", year_Ro, sprintf("%02d", month_Ro), "/")
datos_poland= read.csv(paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Raw_data/Runoff_rawdata/RO_", year_Ro, sprintf("%02d", month_Ro), ".csv"))

# Loading GIS and reference data
zerosRaster <- "C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/zeros.tif"
clip_base <- vect("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/base_clip.shp")
area <- vect("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/Area.shp")

#Projecting to the Polish reference system
points_sf <- st_as_sf(datos_poland, coords = c("longitude", "latitude"), crs = 4326)
ro_sf <- st_transform(points_sf, crs = 2180)
ro_sf <- ro_sf %>%
  mutate(
    LON = st_coordinates(.)[,1],  # Extract X (longitude in meters)
    LAT = st_coordinates(.)[,2]   # Extract Y (latitude in meters)
  )
ro <- st_drop_geometry(ro_sf)[, c("surface_runoff_sum", "LON", "LAT", "date")]

#Robot starts
print(paste("Computing runoff surface for month", sprintf("%02d", month_Ro), "and year", (year_Ro)))

for (i in 1:diario){
  
  #Getting the day of analysis
  datos <- ro %>% filter(date == paste0(year_Ro, sprintf("%02d", month_Ro), sprintf("%02d",i)))

  # Checking if there any daily precipitation
  if(all(na.omit(datos[1]) == 0)) {
    file.copy(from = zerosRaster, to = paste0(output_folder, "RO_", year_Ro, sprintf("%02d", month_Ro), sprintf("%02d", i), ".tif"))
    print(paste(i, "zeros"))
  } else {
    
    if(length(na.omit(datos[1][datos[1] > 0]))>2){
      #Create spatial dataframe
      data <- na.omit(data.frame(
        x = datos$LON,  # Longitude
        y = datos$LAT,  # Latitude
        z = datos$surface_runoff_sum  # Variable of interest
      ))
      coordinates(data) <- ~x + y
      grd <- expand.grid(x = seq(285000, 340000, by = 250), y = seq(370000, 410000, by = 250))
      coordinates(grd) <- ~x + y
      gridded(grd) <- TRUE
      
      #Perform ordinary kriging
      variogram_model <- autofitVariogram(z ~ 1, data, fix.values = c(0,NA,NA), model = c("Exp"))
      kriging_result <- gstat::krige(
        formula = z ~ 1,        # Model formula
        locations = data,       # Input data
        newdata = grd,         # Grid for interpolation
        model = variogram_model$var_model  # Variogram model
      )
      
      #Geoprocessing
      raster_krig <- rast(kriging_result["var1.pred"])
      crs(raster_krig) <- crs(area)
      raster_clipped <- mask(raster_krig, clip_base)
      template <- rast(ext(raster_clipped), res=30)
      resampled_r <- resample(raster_clipped, template, method = "cubic")
      raster_clipped_fin_fin <- crop(resampled_r, area, mask=TRUE)
      
      #validation
      raster_clipped_fin_fin[raster_clipped_fin_fin < 0] <- 0
      plot(raster_clipped_fin_fin)
      writeRaster(raster_clipped_fin_fin, paste0(output_folder, "RO_", year_Ro, sprintf("%02d", month_Ro), sprintf("%02d", i), ".tif"), overwrite = TRUE)
      print(paste(i, "no xero"))      
    } else {
      file.copy(from = zerosRaster, to = paste0(output_folder, "RO_", year_Ro, sprintf("%02d", month_Ro), sprintf("%02d", i), ".tif"))
      print(paste(i, "zeros and only value"))      
    }
  }
}
