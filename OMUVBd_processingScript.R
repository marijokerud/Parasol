### PARASOL Processing the UVB data (OMUVBd) across Europe ♥

library(ncdf4)
library(sp)
library(raster)
library(plyr)
library(lubridate)
library(scales)


#Download UV-B data from the OMUVBd data product from http://disc.sci.gsfc.nasa.gov/Aura/data-holdings/OMI/omuvbd_v003.shtml

### CREATE RASTER AND SAVE AS ASCII-FILES
setwd() # Set working directory
fileNames <- list.files() # Reads what files are in the directory
ncdfFile <- fileNames
filePath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_asc" #New folder to save ascii files in

makeRaster <- function(ncdfFile, filepath){
  nc <- nc_open(ncdfFile)
  getVar <- ncvar_get(nc)
  nc_close(nc)
  createRaster <- raster(getVar)
  transposeRaster <- t(createRaster)
  flipRaster <- flip(transposeRaster, direction = "y")
  bb <- extent(-24.5, 45.5, 24.5, 71.5)
  extent(flipRaster) <- bb
  
  dir.create(paste(filePath), showWarnings = FALSE) # create a new directory to store the files if needed
  fileToWrite <- paste(filePath, "/", substr(ncdfFile, start =1, stop = 28), ".asc", sep = "") 
  writeRaster(flipRaster, filename = fileToWrite, fileformat = asc, overwrite=TRUE)	# write an ascii file
  
}

sapply(fileNames, makeRaster, filepath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_asc" )



### EXTRACT COORDINATES AND SAVE AS CSV-FILES
setwd() # Set working directory
fileNames <- list.files() # Reads what files are in the directory
ascFile <- fileNames
filePath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/Public/Day"


## LOAD ARBORETA COORDINATES ##
sites <-read.csv("O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/Europe/Data/Arboreta/Arboreta-coordinates.csv", header = TRUE, sep=";")
head(sites)
arboreta <- sites$site
coor <- cbind(sites$long, sites$lat)

extractDay <- function(ascFile, filepath){
  Raster <- raster(ascFile)
  crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  # set geographical setting, datum WGS84
  proj4string(Raster) <-crs.geo #Add projection (datum WGS84)
  
  values <- extract(Raster, coor, method="simple", df=TRUE, na.rm=TRUE)
  
  dir.create(paste(filePath), showWarnings = FALSE) # create a new directory to store the files if needed
  fileToWrite <- paste(filePath, "/", substr(ascFile, start =20, stop = 28), ".csv", sep = "") 
  write.csv2(values, file = fileToWrite)	# Write a csv file
}

sapply(fileNames, extractDay, filepath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/Public/Day" )


##PUT FILES TOGETHER
setwd() # Set working directory
fileNames <- list.files() # Reads what files are in the directory
filePath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/Public/Day"


makeDateData <- function(x){
  yr <- substr(x, start =1, stop = 4)
  mnth <- substr(x, start = 6, stop = 7)    
  dy <- substr(x, start = 8, stop = 9)
  myDate <- paste(yr, mnth, dy, sep = "-")
  myDate <- as.Date(myDate)
  
  uv <- read.csv2( file = x)
  uv$ar <- 0
  colnames(uv) <- c("", "", "uvb", "arb")
  result <- data.frame(date = myDate,
                       uvb = uv$uvb,
                       arb = as.character(arboreta))
  return(result) 
}

uvbData <- do.call(rbind.data.frame, lapply(fileNames, makeDateData))
head(uvbData)
library(reshape)
uvbData2 <- melt(uvbData, id=c("date", "arb"))
head(uvbData2)
uvbData3<- cast(uvbData2, date~ arb)
head(uvbData3)

setwd() # Set working directory
write.csv2(uvbData3, file = "uvbDataDailyArboreta.csv")


