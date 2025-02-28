#Loading libraries
library(automap)
library(gstat)
library(sp)
library(raster)
library(terra)

test= rast("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Precipitation/2023/P_202309/P_20230918.tif")
plot(evapo) 


# Load the raster datasets (update file paths accordingly)
runoff <- rast("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Runoff/2023/RO_202309/RO_20230918.tif")  # Runoff raster
precip <- rast("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Precipitation/2023/P_202309/P_20230918.tif")  # Precipitation raster
evapo <- rast("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Evapotranspiration/2023/ET_202309/ET_20230918.tif")  # Evapotranspiration raster

target_raster <- runoff  # Use runoff as the reference
precip <- resample(precip, target_raster, method = "cubic")
evapo <- resample(evapo, target_raster, method = "cubic")

# Stack rasters to align them
stacked_rasters <- c(runoff, precip, evapo)

# Convert raster stack to a data frame
df <- as.data.frame(stacked_rasters, xy = TRUE, na.rm = TRUE)

# Rename columns for clarity
colnames(df) <- c("x", "y", "runoff", "precip", "evapo")

# Perform OLS regression: runoff ~ precipitation + evapotranspiration
ols_model <- lm(runoff ~ precip + evapo, data = df)

# Print summary of the model
summary(ols_model)

# Predict runoff using the model and create a new raster
df$predicted_runoff <- predict(ols_model, newdata = df)

# Convert the predicted values back to a raster
predicted_raster <- terra::rast(df[, c("x", "y", "predicted_runoff")], crs = crs(runoff))

# Save the predicted runoff raster
writeRaster(predicted_raster, "predicted_runoff.tif", overwrite = TRUE)