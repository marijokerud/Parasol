### PARASOL Calculating long-term UVB (2005-2015) across Europe ♥
library(ncdf4)
library(sp)
library(raster)


### CALCULATE MONTHLY MEAN ###
setwd() # Set working directory
fileNames <- list.files() # Reads what files are in the directory
filePath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_monthly_mean"

#mnthToCalc= "2014m05"

calcMnthMean <- function(mnthToCalc, fileNames, filepath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_asc"){
  
  want <- grep(pattern = mnthToCalc, x = fileNames) # identifies the year + month you want
  wantMnth <- fileNames[want] # gets the year/ month you want
  mnth <- stack(wantMnth) # build a stack
  crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  # set geographical setting, datum WGS84
  proj4string(mnth) <-crs.geo #Add projection (datum WGS84)
  
  mnthMean <- calc(mnth, fun = mean, na.rm = TRUE) # Calc the monthly means

  dir.create(paste(filePath), showWarnings = FALSE) # create a new directory to store the files if needed
  fileToWrite <- paste(filePath, "/", mnthToCalc, ".asc", sep = "") 
  writeRaster(mnthMean, filename = fileToWrite, fileformat = asc, overwrite=TRUE)	# write an asci file
  
}


#Run one
calcMnthMean(mnthToCalc = "2015m05", fileNames, filePath )

## RUN A BATCH ##
#2005
filesToCalc <- c("2005m01", "2005m02", "2005m03", "2005m04", "2005m05", "2005m06", "2005m07", "2005m08", "2005m09", "2005m10", "2005m11", "2005m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2006
filesToCalc <- c("2006m01", "2006m02", "2006m03", "2006m04", "2006m05", "2006m06", "2006m07", "2006m08", "2006m09", "2006m10", "2006m11", "2006m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2007
filesToCalc <- c("2007m01", "2007m02", "2007m03", "2007m04", "2007m05", "2007m06", "2007m07", "2007m08", "2007m09", "2007m10", "2007m11", "2007m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2008
filesToCalc <- c("2008m01", "2008m02", "2008m03", "2008m04", "2008m05", "2008m06", "2008m07", "2008m08", "2008m09", "2008m10", "2008m11", "2008m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2009
filesToCalc <- c("2009m01", "2009m02", "2009m03", "2009m04", "2009m05", "2009m06", "2009m07", "2009m08", "2009m09", "2009m10", "2009m11", "2009m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2010
filesToCalc <- c("2010m01", "2010m02", "2010m03", "2010m04", "2010m05", "2010m06", "2010m07", "2010m08", "2010m09", "2010m10", "2010m11", "2010m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2011
filesToCalc <- c("2011m01", "2011m02", "2011m03", "2011m04", "2011m05", "2011m06", "2011m07", "2011m08", "2011m09", "2011m10", "2011m11", "2011m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2012
filesToCalc <- c("2012m01", "2012m02", "2012m03", "2012m04", "2012m05", "2012m06", "2012m07", "2012m08", "2012m09", "2012m10", "2012m11", "2012m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2013
filesToCalc <- c("2013m01", "2013m02", "2013m03", "2013m04", "2013m05", "2013m06", "2013m07", "2013m08", "2013m09", "2013m10", "2013m11", "2013m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2014
filesToCalc <- c("2014m01", "2014m02", "2014m03", "2014m04", "2014m05", "2014m06", "2014m07", "2014m08", "2014m09", "2014m10", "2014m11", "2014m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )

#2015
filesToCalc <- c("2015m01", "2015m02", "2015m03", "2015m04", "2015m05", "2015m06", "2015m07", "2015m08", "2015m09", "2015m10", "2015m11", "2015m12")
sapply(filesToCalc, calcMnthMean, fileNames = fileNames )


### CALCULATE MONTHLY LONG-TERM ###
setwd() # Set working directory
fileNames <- list.files() # Reads what files are in the directory
setwd() # Set working directory
filePath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_monthly_mean_climatology"

calcMnthClim <- function(mnthToCalc1, fileNames, filepath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_monthly_mean"){
  
  want <- grep(pattern = mnthToCalc1, x = fileNames) # identifies the year + month you want
  wantMnth <- fileNames[want] # gets the year/ month you want
  mnth <-stack(wantMnth) # build a stack
  crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  # Set geographical setting, datum WGS84
  proj4string(mnth) <-crs.geo #Add projection (datum WGS84)
  
  mnthMean1 <- calc(mnth, fun = mean, na.rm = TRUE) # Calc the monthly mean
  
  dir.create(paste(filePath), showWarnings = FALSE) # Create a new directory to store the files if needed
  fileToWrite4 <- paste(filePath, "/", mnthToCalc1, ".asc", sep = "") 
  writeRaster(mnthMean1, filename = fileToWrite4, fileformat = asc, overwrite=TRUE)	# write an asci file
  
}

#RUN
filesToCalc1 <- c("m01", "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m09", "m10", "m11", "m12")
sapply(filesToCalc1, calcMnthClim, fileNames = fileNames )


### CALCULATE ANNUAL LONG-TERM ###
setwd() # Set working directory
jan <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m01.asc")
feb <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m02.asc")
mar <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m03.asc")
apr <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m04.asc")
may <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m05.asc")
jun <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m06.asc")
jul <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m07.asc")
aug <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m08.asc")
sep <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m09.asc")
oct <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m10.asc")
nov <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m11.asc")
des <- raster("O:\\1. Universitetet i Bergen\\Ph.D\\Stats\\R-projects\\UVB\\OMUVBd\\OMUVBd_monthly_mean_climatology\\m12.asc")

year <- stack(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, des)
proj4string(year) <-crs.geo #Add projection (datum WGS84)

yearClim <- calc(year, fun = mean, na.rm = TRUE)
writeRaster(yearClim, filename = "yearClim.asc", fileformat = asc, overwrite=TRUE )


## LOAD ARBORETA COORDINATES ##
sites <-read.csv("O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/Public/Arboreta-coordinates.csv", header = TRUE, sep=";")
head(sites)
site.names <- sites$site
coor <- cbind(sites$long, sites$lat)


## EXTRACT VALUES FOR EACH ARBORETA ##
setwd() # Set working directory
fileNames <- list.files() # Reads what files are in the directory
rasterFile <- fileNames
filePath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_toi_result"

#Run one#
fileNames = "yearClim.asc"


getMM <- function(rasterFile, filepath = "O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/UVB/OMUVBd/OMUVBd_toi") {
  
  openRaster <- raster(rasterFile)
  crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  # set geographical setting, datum WGS84
  proj4string(openRaster) <-crs.geo #Add projection (datum WGS84)
  
  monthlyMeans <- extract(openRaster, coor, method="simple", df=TRUE, na.rm=TRUE)
  row.names(monthlyMeans) <- site.names
  colnames(monthlyMeans) <- c("ID", "UVB")
  
  dir.create(paste(filePath), showWarnings = FALSE) # create a new directory to store the files if needed
  fileToWrite <- paste(filePath, "/", rasterFile, ".csv", sep = "") 
  write.csv2(monthlyMeans, file = fileToWrite)	# Write a csv file
 
}


sapply(fileNames, getMM, filePath)
