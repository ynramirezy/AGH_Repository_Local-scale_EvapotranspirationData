#Downlading formated precipitation monthly data

#Libraries
library(climate)

#Setting parameters
#year: numeric
#month: numeric [1,2,3,4,5,6,7,8,9,10,11,12], ex, sep is 9
#output file path: string, path plus file name, ex, "C:/Users/usuario/sep_precipitation.txt"

year_Preci= 2025
month_Preci= 1
output= paste0("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Precipitation_data/P_", year_Preci, sprintf("%02d", month_Preci), ".txt")

monthlyPreciData(year_Preci, month_Preci, output)

monthlyPreciData<- function(year, month, output) {
  ##Downloading synoptic meteorogical data for 2023
  m = meteo_imgw(interval = "daily", rank = "synop", year = year, coords = FALSE)
  
  ##Setting the records for September and for the daily precipitation
  m_month= m[m$mm==month,]
  colnames(m_month)
  preci_month= m_month[c("id", "station", "mm", "day", "rr_daily")]
  
  ##Getting stations and days values
  stations= length(unique(preci_month$station))
  days_rr= dim(preci_month[preci_month$station == unique(preci_month$station)[1],])[1]
  
  ##Creating the raining day names
  shp_names_rr= array()
  for(k in 1:days_rr) {
    shp_names_rr[k]= paste0(paste("rr", sprintf("%02d", month), sep="_"), sprintf("%02d", preci_month[preci_month$station == unique(preci_month$station)[1],]$day[k]))
  }
  
  ##Creating the dataframe of the precipitation
  preci_poland <- data.frame(matrix(ncol = days_rr + 3, nrow = stations))
  x <- c(colnames(preci_month)[1], colnames(preci_month)[2], colnames(preci_month)[3], shp_names_rr)
  colnames(preci_poland) <- x
  
  ##Populating preci_poland
  preci_poland$station= unique(preci_month$station)
  preci_poland$id= unique(preci_month$id)
  preci_poland$mm= month
  for(i in 1:stations){
    for(j in 1:days_rr){
      preci_poland[i,j+3]= preci_month[preci_month$station == unique(preci_month$station)[i],]$rr[j]
    }
  }
  
  ##Attaching the coordinates
  coords_preci= read.csv("C:/Users/chess/OneDrive/Desktop/AGH/RunOff/NewAttempt/Auxiliar_data/Coords_new.txt", sep="\t")
  precixy_poland <- merge(preci_poland, coords_preci, by = "id")
  
  #Saving results
  write.table(precixy_poland, output, sep="\t", row.names=FALSE, col.names=TRUE)
}

