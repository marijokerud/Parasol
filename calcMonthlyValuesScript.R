### PARASOL UV-B periods across Europe ♥

library(ncdf4)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(plyr)
library(lubridate)
library(scales)
library(ggplot2)

setwd() # Set working directory
dailyArb <- read.csv2("uvbDataDailyArboreta.csv", header= TRUE, row.names= 1) 
head(dailyArb)
class(dailyArb$date)
dailyArb$date <- as.Date(dailyArb$date, format = "%Y-%m-%d")

flow <- read.csv("flowDateEst.csv", header = TRUE, sep = ";")
flow$date <- as.Date(flow$flowDate, format = "%d/%m/%Y")

#### FUNCTION TO EXTRACT THE MONTHLY VALUES FOR A GIVEN GARDEN AND A GIVEN TIME PERIOD

findMonthValues <- function(garden, nDays, flow, dailyArb){
	
	# garden = character object, name of garden
	# nDays  = numeric, number of days before sampling period you want to consider
	# flow = flowering table (see flowDate.csv)
	# dailyArb = daily UVB values for a given month
		
	# Find the flowering date for the given garden
	endDate <- flow$date[flow$loc == garden]

	# Match that up with the dates in the dailyArb table and find the end and then the start point
	
	dateRange <- seq.Date(from = endDate, length.out = nDays, by = -1)
	startDate <- min(dateRange) 
	
	# Find the mean UVB for that period
	meanUVB <- mean(dailyArb[dailyArb$date >= startDate & dailyArb$date <= endDate, garden ], na.rm = TRUE)
	
	##### Find the anomalies for each month
	
	# Split the dates up so you can select the ones you want
	dailyArb$dm <- format(dailyArb$date, format = "%d-%m")
	dailyArb$yr <- format(dailyArb$date, format = "%Y")
	wantRange <- format(dateRange, format = "%d-%m")

	# select only the months you want
	monthlyData <- dailyArb[dailyArb$dm %in% wantRange, c("date", "dm", "yr", garden)]

	# find the means
	monthMean <- aggregate(monthlyData[garden], by = monthlyData["yr"], mean, na.rm= TRUE )

	# get the standardised anomalies
	sda <- scale(monthMean[garden])[,1]
	monthMean$sda <- sda	
	colnames(monthMean) <- c("yr", "mean", "sda")
	
	return(list(garden  = garden,
				meanUVB = meanUVB,
				anomTable = monthMean
				))
}

#One week
findMonthValues(garden = "Bergen", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 7, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 7, flow = flow, dailyArb = dailyArb)

#Two weeks
findMonthValues(garden = "Bergen", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 14, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 14, flow = flow, dailyArb = dailyArb)

#Three weeks
findMonthValues(garden = "Bergen", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 21, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 21, flow = flow, dailyArb = dailyArb)

#Four weeks
findMonthValues(garden = "Bergen", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 28, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 28, flow = flow, dailyArb = dailyArb)

#Five weeks
findMonthValues(garden = "Bergen", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 35, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 35, flow = flow, dailyArb = dailyArb)

#Six weeks
findMonthValues(garden = "Bergen", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 42, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 42, flow = flow, dailyArb = dailyArb)

#Seven weeks
findMonthValues(garden = "Bergen", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 49, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 49, flow = flow, dailyArb = dailyArb)

#Eight weeks
findMonthValues(garden = "Bergen", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 56, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 56, flow = flow, dailyArb = dailyArb)

#Nine weeks
findMonthValues(garden = "Bergen", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 63, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 63, flow = flow, dailyArb = dailyArb)

#Ten weeks
findMonthValues(garden = "Bergen", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 70, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 70, flow = flow, dailyArb = dailyArb)

#Eleven weeks
findMonthValues(garden = "Bergen", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 77, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 77, flow = flow, dailyArb = dailyArb)

#Twelve weeks
findMonthValues(garden = "Bergen", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana", nDays = 84, flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba", nDays = 84, flow = flow, dailyArb = dailyArb)






################### G R O W I N G  S E A S O N ######################################

setwd() # Set working directory
dailyArb <- read.csv2("uvbDataDailyArboreta.csv", header= TRUE, row.names= 1)
head(dailyArb)
class(dailyArb$date)
dailyArb$date <- as.Date(dailyArb$date, format = "%Y-%m-%d")

flow <- read.csv("flowDateGS.csv", header = TRUE)
flow$Sdate <- as.Date(flow$sDate, format = "%d/%m/%Y")
flow$Edate <- as.Date(flow$eDate, format = "%d/%m/%Y")

#### FUNCTION TO EXTRACT THE MONTHLY VALUES FOR A GIVEN GARDEN AND A GIVEN TIME PERIOD


#### FUNCTION TO EXTRACT THE MONTHLY VALUES FOR A GIVEN GARDEN AND A GIVEN TIME PERIOD

findMonthValues <- function(garden, flow, dailyArb){
  
  # garden = character object, name of garden
  # nDays  = numeric, number of days before sampling period you want to consider
  # flow = flowering table (see flowDate.csv)
  # dailyArb = daily UVB values for a given month
  
  # Find the flowering date for the given garden
  startDate <- flow$Sdate[flow$loc == garden] 
  endDate <- flow$Edate[flow$loc == garden]
  
  # Match that up with the dates in the dailyArb table and find the end and then the start point
  
  dateRange <- seq.Date(from = startDate, to = endDate, by = "day")
  
  # Find the mean UVB for that period
  meanUVB <- mean(dailyArb[dailyArb$date >= startDate & dailyArb$date <= endDate,  garden ], na.rm = TRUE)
  
  ##### Find the anomalies for each month
  
  # Split the dates up so you can select the ones you want
  dailyArb$dm <- format(dailyArb$date, format = "%d-%m")
  dailyArb$yr <- format(dailyArb$date, format = "%Y")
  wantRange <- format(dateRange, format = "%d-%m")
  
  # select only the months you want
  monthlyData <- dailyArb[dailyArb$dm %in% wantRange, c("date", "dm", "yr",  garden)]
  
  # find the means
  monthMean <- aggregate(monthlyData[garden], by = monthlyData["yr"], mean, na.rm= TRUE )
  
  # get the standardised anomalies
  sda <- scale(monthMean[garden])[,1]
  monthMean$sda <- sda	
  colnames(monthMean) <- c("yr", "mean", "sda")
  
  return(list(garden  = garden,
              meanUVB = meanUVB,
              anomTable = monthMean
  ))
}

#Growing season
findMonthValues(garden = "Bergen", flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Stockholm",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Benmore",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Edinburgh",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Dublin",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Bedgebury",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Munich",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Salzburg",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Geneva",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Ljubljana",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Podgorica",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Pontevedra",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Tirana",  flow = flow, dailyArb = dailyArb)
findMonthValues(garden = "Cordoba",  flow = flow, dailyArb = dailyArb)


