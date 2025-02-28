#Downloading massively Evapotranspiration nc data from EUMESAT

#Loading libraries
library(httr)
library(terra)
library(lubridate)

#Parameters of geoprocessing
year_Eva= 2025
month_Eva= 1
output_folder <- paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Evapotranspiration/Evapotranspiration_data/NC_", year_Eva, sprintf("%02d", month_Eva), "/")
username <- "yyara"
password <- "MiamiHorror9*"

for (i in 1:days_in_month(ymd(paste(year_Eva, month_Eva, "01", sep="-")))) {

  #Evapotranspiration web scraping
  url <- paste0("https://datalsasaf.lsasvcs.ipma.pt/PRODUCTS/MSG/MDMET/NETCDF/", year_Eva, "/", sprintf("%02d", month_Eva), "/", sprintf("%02d", i), "/NETCDF4_LSASAF_MSG_DMET_MSG-Disk_", year_Eva, sprintf("%02d", month_Eva), sprintf("%02d", i), "0000.nc")
  output_file <- paste0(output_folder, strsplit(url, "/")[[1]][11])
  response <- GET(url, authenticate(username, password), write_disk(output_file, overwrite = TRUE))
  if (status_code(response) == 200) {
    print(paste(i, "File downloaded successfully!"))
  } else {
    print(paste("Failed to download. Status code:", status_code(response)))
  }
}

#Validator
nc <- rast(output_file)
plot(nc[[1]])