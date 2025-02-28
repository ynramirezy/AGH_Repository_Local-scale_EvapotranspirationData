#Precipitation interpolation surface generator using geostatistics

#Loading libraries
library(automap)
library(gstat)
library(sp)
library(raster)
library(terra)

#Parameters of geoprocessing
year_Preci= 2025
month_Preci= 1
output_folder <- paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/", year_Preci, "/P_", year_Preci, sprintf("%02d", month_Preci), "/")
datos_poland= read.csv(paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Precipitation_data/P_", year_Preci, sprintf("%02d", month_Preci), ".txt"), sep="\t")

# Loading GIS and reference data
zerosRaster <- "C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/zeros.tif"
area <- vect("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/Area.shp")
clip_base <- vect("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/base_clip.shp")

# Loading precipitation data
stations=c(352150300, 350150500, 351180435, 350160520, 351160415, 351160418, 350170530, 352160330, 352140310, 350150510, 351180455, 351160424, 351150400)
datos <- datos_poland[datos_poland$id %in% stations, ]
datos[, c("station", "NAME")]

#Robot starts
print(paste("Computing precipitation surface for month", sprintf("%02d", month_Preci), "and year", (year_Preci)))
validator <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(validator) <- c("day", "minR", "minD", "maxR", "maxD", "check_low", "check_up")

for (i in 1:days_in_month(ymd(paste(year_Preci, sprintf("%02d", month_Preci), "01", sep="-")))){
  
  # Checking if there any daily precipitation
  if(all(na.omit(datos[i+3]) == 0)) {
    file.copy(from = zerosRaster, to = paste0(output_folder, "P_", year_Preci, sprintf("%02d", month_Preci), sprintf("%02d", i), ".tif"))
    print(paste(i, "zeros"))
  } else {
    
    if(length(na.omit(datos[,3+i][datos[,3+i] > 0]))>2){
      #Create spatial dataframe
      data <- na.omit(data.frame(
        x = datos$LON,  # Longitude
        y = datos$LAT,  # Latitude
        z = datos[,3+i]  # Variable of interest
      ))
      coordinates(data) <- ~x + y
      grd <- expand.grid(x = seq(200000, 470000, by = 250), y = seq(280000, 550000, by = 250))
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
      writeRaster(raster_clipped_fin_fin, paste0(output_folder, "P_", year_Preci, sprintf("%02d", month_Preci), sprintf("%02d", i), ".tif"), overwrite = TRUE)
      validator <- rbind(validator, list(day= i, minR= min(values(raster_clipped_fin_fin), na.rm = TRUE), minD= min(na.omit(datos[,3+i][datos[,3+i] > 0])), maxR= max(values(raster_clipped_fin_fin), na.rm = TRUE), maxD= max(na.omit(datos[,3+i])), check_low= ifelse(min(na.omit(datos[,3+i][datos[,3+i] > 0])) < min(values(raster_clipped_fin_fin), na.rm = TRUE), "OK", "Not OK"), check_up= ifelse(max(na.omit(datos[,3+i])) > max(values(raster_clipped_fin_fin), na.rm = TRUE), "OK", "Not OK")))
      print(paste(i, "no xero"))      
    } else {
      file.copy(from = zerosRaster, to = paste0(output_folder, "P_", year_Preci, sprintf("%02d", month_Preci), sprintf("%02d", i), ".tif"))
      print(paste(i, "zeros and only value"))      
    }
  }
}

validator
