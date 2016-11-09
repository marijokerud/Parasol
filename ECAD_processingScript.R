### PARASOL Estimation of flowering dates across Europe ♥

#Download the 0.50 deg. regular grid, 1995-2016 from http://www.ecad.eu/download/ensembles/downloadchunks.php

file.choose()
install.packages("ncdf4")
install.packages("sp")
install.packages("raster")
install.packages("lubridate")
library(ncdf4)
library(sp)
library(raster)
library(lubridate)

setwd() # Set working directory

### netCDF ###
nc <- nc_open("tg_0.25deg_reg_1995-2015_v13.1.nc")
print(nc)
names(nc$var)
names(nc$dim)
str(nc)
str(nc$var$tg)
str(nc$dim)

dailyTemp <- ncvar_get(nc)  #I don't need to specify with varid= because there is only one variable
d <- as.Date(nc$dim$time$vals, origin = "1950-01-01")
temp2014<-dailyTemp[, , d > as.Date("2014-01-01") & d < as.Date("2014-07-01")]

temp2014[, , 1]

image(temp2014[, , 150])
nc_close(nc)

##LOAD ARBORETA COORDINATES##
sites <-read.csv("O:/1. Universitetet i Bergen/Ph.D/Stats/R-projects/Europe/Data/Arboreta/Arboreta-coordinates.csv", header = TRUE, sep=";")
head(sites)
site.names <- sites$site
coor <- cbind(sites$long, sites$lat)  # set x (longitude) and y (latitude)

europe <- stack("tg_0.25deg_reg_1995-2015_v13.1.nc")
plot(europe)
arbEurope<- extract(europe, coor, method='simple', fun=NULL, na.rm=TRUE, layer = 6941, nl=180)
arbEurope<- t(arbEurope)
row.names(arbEurope) <-substr(start =2, stop = 11, rownames(arbEurope))
colnames(arbEurope)<- site.names
write.csv2(arbEurope, file = "arbEur.csv")

arbEur <- read.csv("arbEur.csv", header=TRUE, sep = ";", dec= ",")
colnames(arbEur)[1]<- "date"
arbEur$date<- as.Date(arbEur$date, format = "%Y-%m-%d")
class(arbEur$date)
head(arbEur)

dateArbEur <- arbEur$date

peakFlower <- function(x, limit = 288, date = dateArbEur ){

baseline <- mean(x[1:59])
if(baseline < 4 ) baseline <- 4

x[x < baseline] <- 0             #Sets values below 4 degrees to 0
zero<-which(x == 0)       #Finds the last row value with temp = 0

if(length(zero) > 0){
  s.col<-tail(zero, n=1)    #Save last last row value with temp = 0
  x<- x[-c(1:s.col)]       #Create a new vector with temp after last day of below 4 degrees
} else{
  s.col <- 1
}

x <- cumsum(x)            #Cumulative sum for each row 
flower<-which(x > limit)  #Finds first row value above 288 degree/days
e.col<- head(flower, n=1) #Save first row value above 288 degree/days 
colPOI<- s.col+e.col      #Identify row at peak flowering
flowerdate<-date[colPOI]       #Identify date at peak flowering
return(as.character(flowerdate))
}


# Remove date from arbEur
arbEur1 <- arbEur
arbEur1$date <- NULL

# Running one garden
peakFlower(x = arbEur1$Geneva)
peakFlower(x = arbEur1[,12])


# Loop across columns using apply
FD<- apply(arbEur1, 2, FUN = peakFlower)
FD
class(FD)
write.csv2(FD, file = "flowDateEst.csv")


