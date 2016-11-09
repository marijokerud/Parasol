### PARASOL Supporting information figures ♥

install.packages("coda")
install.packages("rjags")
library(coda)
library(rjags)
library(ggplot2)
library(grid)
library(cowplot)



############################################### REDUCED DATASET ###############################################
###############################################     ONE WEEK    ###############################################
setwd() # Set working directory
sylvestris <- read.csv("tmahEurMeanNEWred.csv", header=TRUE, sep = ";") #New dataset without Benmore and Edinburgh
head(sylvestris)
summary(sylvestris)

ratio <- as.numeric(sylvestris$ratio)

uv1 <- as.numeric(sylvestris$w1)
uv1=scale(uv1)[,1]


alpha.mean1 <- mean(c(paraSamplesONEwo[[1]][,1], paraSamplesONEwo[[2]][,1], paraSamplesONEwo[[3]][,1]))
beta.mean1 <- mean(c(paraSamplesONEwo[[1]][,2], paraSamplesONEwo[[2]][,2], paraSamplesONEwo[[3]][,2]))

alpha1 <- c(paraSamplesONEwo[[1]][,1], paraSamplesONEwo[[2]][,1], paraSamplesONEwo[[3]][,1])
beta1 <- c(paraSamplesONEwo[[1]][,2], paraSamplesONEwo[[2]][,2], paraSamplesONEwo[[3]][,2])


predUV1 <- seq(min(uv1-0.0005), max(uv1+0.05), 0.05)
predictions <- array(dim = c(length(predUV1), length(alpha1)))
for(i in 1:length(predUV1)){
  predictions[i,] <- alpha1 + beta1 * predUV1[i]
}

UPB1 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope1<- alpha.mean1 + beta.mean1 * predUV1
LPB1 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB1
slope1
LPB1

predONE<- list(UPB1, slope1, LPB1, predUV1)
predONE<-as.data.frame(predONE)
colnames(predONE)<- c("UPB", "slope", "LPB", "predUV")


ONE<-
  ggplot() +
  ggtitle("Mean of one week") +
  geom_point(data=sylvestris,aes(x=uv1, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(slope1)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(UPB1)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(LPB1)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv1, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())  


############################################### TWO WEEKS ###############################################

uv2 <- as.numeric(sylvestris$w2)
uv2=scale(uv2)[,1]


alpha.mean2 <- mean(c(paraSamplesTWOwo[[1]][,1], paraSamplesTWOwo[[2]][,1], paraSamplesTWOwo[[3]][,1]))
beta.mean2 <- mean(c(paraSamplesTWOwo[[1]][,2], paraSamplesTWOwo[[2]][,2], paraSamplesTWOwo[[3]][,2]))

alpha2 <- c(paraSamplesTWOwo[[1]][,1], paraSamplesTWOwo[[2]][,1], paraSamplesTWOwo[[3]][,1])
beta2 <- c(paraSamplesTWOwo[[1]][,2], paraSamplesTWOwo[[2]][,2], paraSamplesTWOwo[[3]][,2])


predUV2 <- seq(min(uv2-0.0005), max(uv2+0.05), 0.05)
predictions <- array(dim = c(length(predUV2), length(alpha2)))
for(i in 1:length(predUV2)){
  predictions[i,] <- alpha2 + beta2 * predUV2[i]
}

UPB2 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope2<- alpha.mean2 + beta.mean2 * predUV2
LPB2 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB2
slope2
LPB2

predTWO<- list(UPB2, slope2, LPB2, predUV2)
predTWO<-as.data.frame(predTWO)
colnames(predTWO)<- c("UPB", "slope", "LPB", "predUV")


TWO<-
  ggplot() +
  ggtitle("Mean of two weeks") +
  geom_point(data=sylvestris,aes(x=uv2, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTWO,aes(x=predUV2, y=exp(slope2)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWO,aes(x=predUV2, y=exp(UPB2)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWO,aes(x=predUV2, y=exp(LPB2)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv2, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### THREE WEEKS ###############################################

uv3 <- as.numeric(sylvestris$w3)
uv3=scale(uv3)[,1]


alpha.mean3 <- mean(c(paraSamplesTHREEwo[[1]][,1], paraSamplesTHREEwo[[2]][,1], paraSamplesTHREEwo[[3]][,1]))
beta.mean3 <- mean(c(paraSamplesTHREEwo[[1]][,2], paraSamplesTHREEwo[[2]][,2], paraSamplesTHREEwo[[3]][,2]))

alpha3 <- c(paraSamplesTHREEwo[[1]][,1], paraSamplesTHREEwo[[2]][,1], paraSamplesTHREEwo[[3]][,1])
beta3 <- c(paraSamplesTHREEwo[[1]][,2], paraSamplesTHREEwo[[2]][,2], paraSamplesTHREEwo[[3]][,2])


predUV3 <- seq(min(uv3-0.0005), max(uv3+0.05), 0.05)
predictions <- array(dim = c(length(predUV3), length(alpha3)))
for(i in 1:length(predUV3)){
  predictions[i,] <- alpha3 + beta3 * predUV3[i]
}

UPB3 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope3<- alpha.mean3 + beta.mean3 * predUV3
LPB3 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB3
slope3
LPB3

predTHREE<- list(UPB3, slope3, LPB3, predUV3)
predTHREE<-as.data.frame(predTHREE)
colnames(predTHREE)<- c("UPB", "slope", "LPB", "predUV")


THREE<-
  ggplot() +
  ggtitle("Mean of three weeks") +
  geom_point(data=sylvestris,aes(x=uv3, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTHREE,aes(x=predUV3, y=exp(slope3)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTHREE,aes(x=predUV3, y=exp(UPB3)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTHREE,aes(x=predUV3, y=exp(LPB3)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv3, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### FOUR WEEKS ###############################################

uv4 <- as.numeric(sylvestris$w4)
uv4=scale(uv4)[,1]


alpha.mean4 <- mean(c(paraSamplesFOURwo[[1]][,1], paraSamplesFOURwo[[2]][,1], paraSamplesFOURwo[[3]][,1]))
beta.mean4 <- mean(c(paraSamplesFOURwo[[1]][,2], paraSamplesFOURwo[[2]][,2], paraSamplesFOURwo[[3]][,2]))

alpha4 <- c(paraSamplesFOURwo[[1]][,1], paraSamplesFOURwo[[2]][,1], paraSamplesFOURwo[[3]][,1])
beta4 <- c(paraSamplesFOURwo[[1]][,2], paraSamplesFOURwo[[2]][,2], paraSamplesFOURwo[[3]][,2])


predUV4 <- seq(min(uv4-0.0005), max(uv4+0.05), 0.05)
predictions <- array(dim = c(length(predUV4), length(alpha4)))
for(i in 1:length(predUV4)){
  predictions[i,] <- alpha4 + beta4 * predUV4[i]
}

UPB4 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope4<- alpha.mean4 + beta.mean4 * predUV4
LPB4 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB4
slope4
LPB4

predFOUR<- list(UPB4, slope4, LPB4, predUV4)
predFOUR<-as.data.frame(predFOUR)
colnames(predFOUR)<- c("UPB", "slope", "LPB", "predUV")


FOUR<-
  ggplot() +
  ggtitle("Mean of four weeks") +
  geom_point(data=sylvestris,aes(x=uv4, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predFOUR,aes(x=predUV4, y=exp(slope4)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFOUR,aes(x=predUV4, y=exp(UPB4)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFOUR,aes(x=predUV4, y=exp(LPB4)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv4, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### FIVE WEEKS ###############################################

uv5 <- as.numeric(sylvestris$w5)
uv5=scale(uv5)[,1]


alpha.mean5 <- mean(c(paraSamplesFIVEwo[[1]][,1], paraSamplesFIVEwo[[2]][,1], paraSamplesFIVEwo[[3]][,1]))
beta.mean5 <- mean(c(paraSamplesFIVEwo[[1]][,2], paraSamplesFIVEwo[[2]][,2], paraSamplesFIVEwo[[3]][,2]))

alpha5 <- c(paraSamplesFIVEwo[[1]][,1], paraSamplesFIVEwo[[2]][,1], paraSamplesFIVEwo[[3]][,1])
beta5 <- c(paraSamplesFIVEwo[[1]][,2], paraSamplesFIVEwo[[2]][,2], paraSamplesFIVEwo[[3]][,2])


predUV5 <- seq(min(uv5-0.0005), max(uv5+0.05), 0.05)
predictions <- array(dim = c(length(predUV5), length(alpha5)))
for(i in 1:length(predUV5)){
  predictions[i,] <- alpha5 + beta5 * predUV5[i]
}

UPB5 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope5<- alpha.mean5 + beta.mean5 * predUV5
LPB5 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB5
slope5
LPB5

predFIVE<- list(UPB5, slope5, LPB5, predUV5)
predFIVE<-as.data.frame(predFIVE)
colnames(predFIVE)<- c("UPB", "slope", "LPB", "predUV")


FIVE<-
  ggplot() +
  ggtitle("Mean of five weeks") +
  geom_point(data=sylvestris,aes(x=uv5, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predFIVE,aes(x=predUV5, y=exp(slope5)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFIVE,aes(x=predUV5, y=exp(UPB5)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFIVE,aes(x=predUV5, y=exp(LPB5)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv5, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())  

############################################### SIX WEEKS ###############################################

uv6 <- as.numeric(sylvestris$w6)
uv6=scale(uv6)[,1]


alpha.mean6 <- mean(c(paraSamplesSIXwo[[1]][,1], paraSamplesSIXwo[[2]][,1], paraSamplesSIXwo[[3]][,1]))
beta.mean6 <- mean(c(paraSamplesSIXwo[[1]][,2], paraSamplesSIXwo[[2]][,2], paraSamplesSIXwo[[3]][,2]))

alpha6 <- c(paraSamplesSIXwo[[1]][,1], paraSamplesSIXwo[[2]][,1], paraSamplesSIXwo[[3]][,1])
beta6 <- c(paraSamplesSIXwo[[1]][,2], paraSamplesSIXwo[[2]][,2], paraSamplesSIXwo[[3]][,2])


predUV6 <- seq(min(uv6-0.0005), max(uv6+0.05), 0.05)
predictions <- array(dim = c(length(predUV6), length(alpha6)))
for(i in 1:length(predUV6)){
  predictions[i,] <- alpha6 + beta6 * predUV6[i]
}

UPB6 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope6<- alpha.mean6 + beta.mean6 * predUV6
LPB6 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB6
slope6
LPB6

predSIX<- list(UPB6, slope6, LPB6, predUV6)
predSIX<-as.data.frame(predSIX)
colnames(predSIX)<- c("UPB", "slope", "LPB", "predUV")


SIX<-
  ggplot() +
  ggtitle("Mean of six weeks") +
  geom_point(data=sylvestris,aes(x=uv6, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predSIX,aes(x=predUV6, y=exp(slope6)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSIX,aes(x=predUV6, y=exp(UPB6)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSIX,aes(x=predUV6, y=exp(LPB6)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv6, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### SEVEN WEEKS ###############################################

uv7 <- as.numeric(sylvestris$w7)
uv7=scale(uv7)[,1]


alpha.mean7 <- mean(c(paraSamplesSEVENwo[[1]][,1], paraSamplesSEVENwo[[2]][,1], paraSamplesSEVENwo[[3]][,1]))
beta.mean7 <- mean(c(paraSamplesSEVENwo[[1]][,2], paraSamplesSEVENwo[[2]][,2], paraSamplesSEVENwo[[3]][,2]))

alpha7 <- c(paraSamplesSEVENwo[[1]][,1], paraSamplesSEVENwo[[2]][,1], paraSamplesSEVENwo[[3]][,1])
beta7 <- c(paraSamplesSEVENwo[[1]][,2], paraSamplesSEVENwo[[2]][,2], paraSamplesSEVENwo[[3]][,2])


predUV7 <- seq(min(uv7-0.0005), max(uv7+0.05), 0.05)
predictions <- array(dim = c(length(predUV7), length(alpha7)))
for(i in 1:length(predUV7)){
  predictions[i,] <- alpha7 + beta7 * predUV7[i]
}

UPB7 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope7<- alpha.mean7 + beta.mean7 * predUV7
LPB7 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB7
slope7
LPB7

predSEVEN<- list(UPB7, slope7, LPB7, predUV7)
predSEVEN<-as.data.frame(predSEVEN)
colnames(predSEVEN)<- c("UPB", "slope", "LPB", "predUV")


SEVEN<-
  ggplot() +
  ggtitle("Mean of seven weeks") +
  geom_point(data=sylvestris,aes(x=uv7, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predSEVEN,aes(x=predUV7, y=exp(slope7)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSEVEN,aes(x=predUV7, y=exp(UPB7)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSEVEN,aes(x=predUV7, y=exp(LPB7)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv7, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### EIGHT WEEKS ###############################################

uv8 <- as.numeric(sylvestris$w8)
uv8=scale(uv8)[,1]


alpha.mean8 <- mean(c(paraSamplesEIGHTwo[[1]][,1], paraSamplesEIGHTwo[[2]][,1], paraSamplesEIGHTwo[[3]][,1]))
beta.mean8 <- mean(c(paraSamplesEIGHTwo[[1]][,2], paraSamplesEIGHTwo[[2]][,2], paraSamplesEIGHTwo[[3]][,2]))

alpha8 <- c(paraSamplesEIGHTwo[[1]][,1], paraSamplesEIGHTwo[[2]][,1], paraSamplesEIGHTwo[[3]][,1])
beta8 <- c(paraSamplesEIGHTwo[[1]][,2], paraSamplesEIGHTwo[[2]][,2], paraSamplesEIGHTwo[[3]][,2])


predUV8 <- seq(min(uv8-0.0005), max(uv8+0.05), 0.05)
predictions <- array(dim = c(length(predUV8), length(alpha8)))
for(i in 1:length(predUV8)){
  predictions[i,] <- alpha8 + beta8 * predUV8[i]
}

UPB8 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope8<- alpha.mean8 + beta.mean8 * predUV8
LPB8 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB8
slope8
LPB8

predEIGHT<- list(UPB8, slope8, LPB8, predUV8)
predEIGHT<-as.data.frame(predEIGHT)
colnames(predEIGHT)<- c("UPB", "slope", "LPB", "predUV")


EIGHT<-
  ggplot() +
  ggtitle("Mean of eight weeks") +
  geom_point(data=sylvestris,aes(x=uv8, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predEIGHT,aes(x=predUV8, y=exp(slope8)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predEIGHT,aes(x=predUV8, y=exp(UPB8)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predEIGHT,aes(x=predUV8, y=exp(LPB8)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv8, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### NINE WEEKS ###############################################

uv9 <- as.numeric(sylvestris$w9)
uv9=scale(uv9)[,1]


alpha.mean9 <- mean(c(paraSamplesNINEwo[[1]][,1], paraSamplesNINEwo[[2]][,1], paraSamplesNINEwo[[3]][,1]))
beta.mean9 <- mean(c(paraSamplesNINEwo[[1]][,2], paraSamplesNINEwo[[2]][,2], paraSamplesNINEwo[[3]][,2]))

alpha9 <- c(paraSamplesNINEwo[[1]][,1], paraSamplesNINEwo[[2]][,1], paraSamplesNINEwo[[3]][,1])
beta9 <- c(paraSamplesNINEwo[[1]][,2], paraSamplesNINEwo[[2]][,2], paraSamplesNINEwo[[3]][,2])


predUV9 <- seq(min(uv9-0.0005), max(uv9+0.05), 0.05)
predictions <- array(dim = c(length(predUV9), length(alpha9)))
for(i in 1:length(predUV9)){
  predictions[i,] <- alpha9 + beta9 * predUV9[i]
}

UPB9 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope9<- alpha.mean9 + beta.mean9 * predUV9
LPB9 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB9
slope9
LPB9

predNINE<- list(UPB9, slope9, LPB9, predUV9)
predNINE<-as.data.frame(predNINE)
colnames(predNINE)<- c("UPB", "slope", "LPB", "predUV")


NINE<-
  ggplot() +
  ggtitle("Mean of nine weeks") +
  geom_point(data=sylvestris,aes(x=uv9, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predNINE,aes(x=predUV9, y=exp(slope9)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predNINE,aes(x=predUV9, y=exp(UPB9)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predNINE,aes(x=predUV9, y=exp(LPB9)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv9, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### TEN WEEKS ###############################################

uv10 <- as.numeric(sylvestris$w10)
uv10=scale(uv10)[,1]


alpha.mean10 <- mean(c(paraSamplesTENwo[[1]][,1], paraSamplesTENwo[[2]][,1], paraSamplesTENwo[[3]][,1]))
beta.mean10 <- mean(c(paraSamplesTENwo[[1]][,2], paraSamplesTENwo[[2]][,2], paraSamplesTENwo[[3]][,2]))

alpha10 <- c(paraSamplesTENwo[[1]][,1], paraSamplesTENwo[[2]][,1], paraSamplesTENwo[[3]][,1])
beta10 <- c(paraSamplesTENwo[[1]][,2], paraSamplesTENwo[[2]][,2], paraSamplesTENwo[[3]][,2])


predUV10 <- seq(min(uv10-0.0005), max(uv10+0.05), 0.05)
predictions <- array(dim = c(length(predUV10), length(alpha10)))
for(i in 1:length(predUV10)){
  predictions[i,] <- alpha10 + beta10 * predUV10[i]
}

UPB10 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope10<- alpha.mean10 + beta.mean10 * predUV10
LPB10 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB10
slope10
LPB10

predTEN<- list(UPB10, slope10, LPB10, predUV10)
predTEN<-as.data.frame(predTEN)
colnames(predTEN)<- c("UPB", "slope", "LPB", "predUV")


TEN<-
  ggplot() +
  ggtitle("Mean of ten weeks") +
  geom_point(data=sylvestris,aes(x=uv10, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTEN,aes(x=predUV10, y=exp(slope10)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTEN,aes(x=predUV10, y=exp(UPB10)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTEN,aes(x=predUV10, y=exp(LPB10)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv10, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### ELEVEN WEEKS ###############################################

uv11 <- as.numeric(sylvestris$w11)
uv11=scale(uv11)[,1]


alpha.mean11 <- mean(c(paraSamplesELEVENwo[[1]][,1], paraSamplesELEVENwo[[2]][,1], paraSamplesELEVENwo[[3]][,1]))
beta.mean11 <- mean(c(paraSamplesELEVENwo[[1]][,2], paraSamplesELEVENwo[[2]][,2], paraSamplesELEVENwo[[3]][,2]))

alpha11 <- c(paraSamplesELEVENwo[[1]][,1], paraSamplesELEVENwo[[2]][,1], paraSamplesELEVENwo[[3]][,1])
beta11 <- c(paraSamplesELEVENwo[[1]][,2], paraSamplesELEVENwo[[2]][,2], paraSamplesELEVENwo[[3]][,2])


predUV11 <- seq(min(uv11-0.0005), max(uv11+0.05), 0.05)
predictions <- array(dim = c(length(predUV11), length(alpha11)))
for(i in 1:length(predUV11)){
  predictions[i,] <- alpha11 + beta11 * predUV11[i]
}

UPB11 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope11<- alpha.mean11 + beta.mean11 * predUV11
LPB11 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB11
slope11
LPB11

predELEVEN<- list(UPB11, slope11, LPB11, predUV11)
predELEVEN<-as.data.frame(predELEVEN)
colnames(predELEVEN)<- c("UPB", "slope", "LPB", "predUV")


ELEVEN<-
  ggplot() +
  ggtitle("Mean of eleven weeks") +
  geom_point(data=sylvestris,aes(x=uv11, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predELEVEN,aes(x=predUV11, y=exp(slope11)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predELEVEN,aes(x=predUV11, y=exp(UPB11)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predELEVEN,aes(x=predUV11, y=exp(LPB11)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv11, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### TWELVE WEEKS ###############################################

uv12 <- as.numeric(sylvestris$w12)
uv12=scale(uv12)[,1]


alpha.mean12 <- mean(c(paraSamplesTWELVEwo[[1]][,1], paraSamplesTWELVEwo[[2]][,1], paraSamplesTWELVEwo[[3]][,1]))
beta.mean12 <- mean(c(paraSamplesTWELVEwo[[1]][,2], paraSamplesTWELVEwo[[2]][,2], paraSamplesTWELVEwo[[3]][,2]))

alpha12 <- c(paraSamplesTWELVEwo[[1]][,1], paraSamplesTWELVEwo[[2]][,1], paraSamplesTWELVEwo[[3]][,1])
beta12 <- c(paraSamplesTWELVEwo[[1]][,2], paraSamplesTWELVEwo[[2]][,2], paraSamplesTWELVEwo[[3]][,2])


predUV12 <- seq(min(uv12-0.0005), max(uv12+0.05), 0.05)
predictions <- array(dim = c(length(predUV12), length(alpha12)))
for(i in 1:length(predUV12)){
  predictions[i,] <- alpha12 + beta12 * predUV12[i]
}

UPB12 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope12<- alpha.mean12 + beta.mean12 * predUV12
LPB12 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB12
slope12
LPB12

predTWELVE<- list(UPB12, slope12, LPB12, predUV12)
predTWELVE<-as.data.frame(predTWELVE)
colnames(predTWELVE)<- c("UPB", "slope", "LPB", "predUV")


TWELVE<-
  ggplot() +
  ggtitle("Mean of twelve weeks") +
  geom_point(data=sylvestris,aes(x=uv12, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(slope12)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(UPB12)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(LPB12)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv12, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 

############################################### GS WEEKS ###############################################

uvGS <- as.numeric(sylvestris$gs_mean)
uvGS=scale(uvGS)[,1]


alpha.meanGS <- mean(c(paraSamplesGSwo[[1]][,1], paraSamplesGSwo[[2]][,1], paraSamplesGSwo[[3]][,1]))
beta.meanGS <- mean(c(paraSamplesGSwo[[1]][,2], paraSamplesGSwo[[2]][,2], paraSamplesGSwo[[3]][,2]))

alphaGS <- c(paraSamplesGSwo[[1]][,1], paraSamplesGSwo[[2]][,1], paraSamplesGSwo[[3]][,1])
betaGS <- c(paraSamplesGSwo[[1]][,2], paraSamplesGSwo[[2]][,2], paraSamplesGSwo[[3]][,2])


predUVGS <- seq(min(uvGS-0.0005), max(uvGS+0.05), 0.05)
predictions <- array(dim = c(length(predUVGS), length(alphaGS)))
for(i in 1:length(predUVGS)){
  predictions[i,] <- alphaGS + betaGS * predUVGS[i]
}

UPBGS <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slopeGS<- alpha.meanGS + beta.meanGS * predUVGS
LPBGS <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPBGS
slopeGS
LPBGS

predGS<- list(UPBGS, slopeGS, LPBGS, predUVGS)
predGS<-as.data.frame(predGS)
colnames(predGS)<- c("UPB", "slope", "LPB", "predUV")


GS<-
  ggplot() +
  ggtitle("Mean of growing season") +
  geom_point(data=sylvestris,aes(x=uvGS, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predGS,aes(x=predUVGS, y=exp(slopeGS)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predGS,aes(x=predUVGS, y=exp(UPBGS)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predGS,aes(x=predUVGS, y=exp(LPBGS)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uvGS, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### CLIM WEEKS ###############################################

uvCLIM <- as.numeric(sylvestris$year_mean)
uvCLIM=scale(uvCLIM)[,1]


alpha.meanCLIM <- mean(c(paraSamplesCLIMwo[[1]][,1], paraSamplesCLIMwo[[2]][,1], paraSamplesCLIMwo[[3]][,1]))
beta.meanCLIM <- mean(c(paraSamplesCLIMwo[[1]][,2], paraSamplesCLIMwo[[2]][,2], paraSamplesCLIMwo[[3]][,2]))

alphaCLIM <- c(paraSamplesCLIMwo[[1]][,1], paraSamplesCLIMwo[[2]][,1], paraSamplesCLIMwo[[3]][,1])
betaCLIM <- c(paraSamplesCLIMwo[[1]][,2], paraSamplesCLIMwo[[2]][,2], paraSamplesCLIMwo[[3]][,2])


predUVCLIM <- seq(min(uvCLIM-0.0005), max(uvCLIM+0.05), 0.05)
predictions <- array(dim = c(length(predUVCLIM), length(alphaCLIM)))
for(i in 1:length(predUVCLIM)){
  predictions[i,] <- alphaCLIM + betaCLIM * predUVCLIM[i]
}

UPBCLIM <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slopeCLIM<- alpha.meanCLIM + beta.meanCLIM * predUVCLIM
LPBCLIM <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPBCLIM
slopeCLIM
LPBCLIM

predCLIM<- list(UPBCLIM, slopeCLIM, LPBCLIM, predUVCLIM)
predCLIM<-as.data.frame(predCLIM)
colnames(predCLIM)<- c("UPB", "slope", "LPB", "predUV")


CLIM<-
  ggplot() +
  ggtitle("Annual mean 2005-2015") +
  geom_point(data=sylvestris,aes(x=uvCLIM, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(slopeCLIM)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(UPBCLIM)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(LPBCLIM)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uvCLIM, y=ratio/200, label = sylvestris$arb),size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### LAT WEEKS ###############################################

uvLAT <- as.numeric(sylvestris$lat)
uvLAT=scale(uvLAT)[,1]


alpha.meanLAT <- mean(c(paraSamplesLATwo[[1]][,1], paraSamplesLATwo[[2]][,1], paraSamplesLATwo[[3]][,1]))
beta.meanLAT <- mean(c(paraSamplesLATwo[[1]][,2], paraSamplesLATwo[[2]][,2], paraSamplesLATwo[[3]][,2]))

alphaLAT <- c(paraSamplesLATwo[[1]][,1], paraSamplesLATwo[[2]][,1], paraSamplesLATwo[[3]][,1])
betaLAT <- c(paraSamplesLATwo[[1]][,2], paraSamplesLATwo[[2]][,2], paraSamplesLATwo[[3]][,2])


predUVLAT <- seq(min(uvLAT-0.0005), max(uvLAT+0.05), 0.05)
predictions <- array(dim = c(length(predUVLAT), length(alphaLAT)))
for(i in 1:length(predUVLAT)){
  predictions[i,] <- alphaLAT + betaLAT * predUVLAT[i]
}

UPBLAT <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slopeLAT<- alpha.meanLAT + beta.meanLAT * predUVLAT
LPBLAT <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPBLAT
slopeLAT
LPBLAT

predLAT<- list(UPBLAT, slopeLAT, LPBLAT, predUVLAT)
predLAT<-as.data.frame(predLAT)
colnames(predLAT)<- c("UPB", "slope", "LPB", "predUV")


LAT<-
  ggplot() +
  ggtitle("Latitude") +
  geom_point(data=sylvestris,aes(x=uvLAT, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predLAT,aes(x=predUVLAT, y=exp(slopeLAT)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predLAT,aes(x=predUVLAT, y=exp(UPBLAT)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predLAT,aes(x=predUVLAT, y=exp(LPBLAT)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uvLAT, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 




#Paper III
source("http://peterhaschke.com/Code/multiplot.R")
pdf("Figure2_ESM.pdf", width = 8.9, height = 11.7, useDingbats=FALSE)
multiplot(ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, ELEVEN, TWELVE, GS, CLIM, LAT, cols=3)
dev.off()





############################################### FULL DATASET ###############################################
###############################################  ONE WEEK   ###############################################
setwd() # Set working directory
sylvestris <- read.csv("EurMeanPCA.csv", header=TRUE, sep = ";")
head(sylvestris)
summary(sylvestris)

ratio <- as.numeric(sylvestris$ratio)

uv1 <- as.numeric(sylvestris$w1)
uv1=scale(uv1)[,1]


alpha.mean1 <- mean(c(paraSamplesONEw[[1]][,1], paraSamplesONEw[[2]][,1], paraSamplesONEw[[3]][,1]))
beta.mean1 <- mean(c(paraSamplesONEw[[1]][,2], paraSamplesONEw[[2]][,2], paraSamplesONEw[[3]][,2]))

alpha1 <- c(paraSamplesONEw[[1]][,1], paraSamplesONEw[[2]][,1], paraSamplesONEw[[3]][,1])
beta1 <- c(paraSamplesONEw[[1]][,2], paraSamplesONEw[[2]][,2], paraSamplesONEw[[3]][,2])


predUV1 <- seq(min(uv1-0.0005), max(uv1+0.05), 0.05)
predictions <- array(dim = c(length(predUV1), length(alpha1)))
for(i in 1:length(predUV1)){
  predictions[i,] <- alpha1 + beta1 * predUV1[i]
}

UPB1 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope1<- alpha.mean1 + beta.mean1 * predUV1
LPB1 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB1
slope1
LPB1

predONE<- list(UPB1, slope1, LPB1, predUV1)
predONE<-as.data.frame(predONE)
colnames(predONE)<- c("UPB", "slope", "LPB", "predUV")


ONE<-
  ggplot() +
  ggtitle("Mean of one week") +
  geom_point(data=sylvestris,aes(x=uv1, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(slope1)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(UPB1)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(LPB1)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv1, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())  


############################################### TWO WEEKS ###############################################

uv2 <- as.numeric(sylvestris$w2)
uv2=scale(uv2)[,1]


alpha.mean2 <- mean(c(paraSamplesTWOw[[1]][,1], paraSamplesTWOw[[2]][,1], paraSamplesTWOw[[3]][,1]))
beta.mean2 <- mean(c(paraSamplesTWOw[[1]][,2], paraSamplesTWOw[[2]][,2], paraSamplesTWOw[[3]][,2]))

alpha2 <- c(paraSamplesTWOw[[1]][,1], paraSamplesTWOw[[2]][,1], paraSamplesTWOw[[3]][,1])
beta2 <- c(paraSamplesTWOw[[1]][,2], paraSamplesTWOw[[2]][,2], paraSamplesTWOw[[3]][,2])


predUV2 <- seq(min(uv2-0.0005), max(uv2+0.05), 0.05)
predictions <- array(dim = c(length(predUV2), length(alpha2)))
for(i in 1:length(predUV2)){
  predictions[i,] <- alpha2 + beta2 * predUV2[i]
}

UPB2 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope2<- alpha.mean2 + beta.mean2 * predUV2
LPB2 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB2
slope2
LPB2

predTWO<- list(UPB2, slope2, LPB2, predUV2)
predTWO<-as.data.frame(predTWO)
colnames(predTWO)<- c("UPB", "slope", "LPB", "predUV")


TWO<-
  ggplot() +
  ggtitle("Mean of two weeks") +
  geom_point(data=sylvestris,aes(x=uv2, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTWO,aes(x=predUV2, y=exp(slope2)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWO,aes(x=predUV2, y=exp(UPB2)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWO,aes(x=predUV2, y=exp(LPB2)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv2, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### THREE WEEKS ###############################################

uv3 <- as.numeric(sylvestris$w3)
uv3=scale(uv3)[,1]


alpha.mean3 <- mean(c(paraSamplesTHREEw[[1]][,1], paraSamplesTHREEw[[2]][,1], paraSamplesTHREEw[[3]][,1]))
beta.mean3 <- mean(c(paraSamplesTHREEw[[1]][,2], paraSamplesTHREEw[[2]][,2], paraSamplesTHREEw[[3]][,2]))

alpha3 <- c(paraSamplesTHREEw[[1]][,1], paraSamplesTHREEw[[2]][,1], paraSamplesTHREEw[[3]][,1])
beta3 <- c(paraSamplesTHREEw[[1]][,2], paraSamplesTHREEw[[2]][,2], paraSamplesTHREEw[[3]][,2])


predUV3 <- seq(min(uv3-0.0005), max(uv3+0.05), 0.05)
predictions <- array(dim = c(length(predUV3), length(alpha3)))
for(i in 1:length(predUV3)){
  predictions[i,] <- alpha3 + beta3 * predUV3[i]
}

UPB3 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope3<- alpha.mean3 + beta.mean3 * predUV3
LPB3 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB3
slope3
LPB3

predTHREE<- list(UPB3, slope3, LPB3, predUV3)
predTHREE<-as.data.frame(predTHREE)
colnames(predTHREE)<- c("UPB", "slope", "LPB", "predUV")


THREE<-
  ggplot() +
  ggtitle("Mean of three weeks") +
  geom_point(data=sylvestris,aes(x=uv3, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTHREE,aes(x=predUV3, y=exp(slope3)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTHREE,aes(x=predUV3, y=exp(UPB3)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTHREE,aes(x=predUV3, y=exp(LPB3)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv3, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### FOUR WEEKS ###############################################

uv4 <- as.numeric(sylvestris$w4)
uv4=scale(uv4)[,1]


alpha.mean4 <- mean(c(paraSamplesFOURw[[1]][,1], paraSamplesFOURw[[2]][,1], paraSamplesFOURw[[3]][,1]))
beta.mean4 <- mean(c(paraSamplesFOURw[[1]][,2], paraSamplesFOURw[[2]][,2], paraSamplesFOURw[[3]][,2]))

alpha4 <- c(paraSamplesFOURw[[1]][,1], paraSamplesFOURw[[2]][,1], paraSamplesFOURw[[3]][,1])
beta4 <- c(paraSamplesFOURw[[1]][,2], paraSamplesFOURw[[2]][,2], paraSamplesFOURw[[3]][,2])


predUV4 <- seq(min(uv4-0.0005), max(uv4+0.05), 0.05)
predictions <- array(dim = c(length(predUV4), length(alpha4)))
for(i in 1:length(predUV4)){
  predictions[i,] <- alpha4 + beta4 * predUV4[i]
}

UPB4 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope4<- alpha.mean4 + beta.mean4 * predUV4
LPB4 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB4
slope4
LPB4

predFOUR<- list(UPB4, slope4, LPB4, predUV4)
predFOUR<-as.data.frame(predFOUR)
colnames(predFOUR)<- c("UPB", "slope", "LPB", "predUV")


FOUR<-
  ggplot() +
  ggtitle("Mean of four weeks") +
  geom_point(data=sylvestris,aes(x=uv4, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predFOUR,aes(x=predUV4, y=exp(slope4)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFOUR,aes(x=predUV4, y=exp(UPB4)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFOUR,aes(x=predUV4, y=exp(LPB4)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv4, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### FIVE WEEKS ###############################################

uv5 <- as.numeric(sylvestris$w5)
uv5=scale(uv5)[,1]


alpha.mean5 <- mean(c(paraSamplesFIVEw[[1]][,1], paraSamplesFIVEw[[2]][,1], paraSamplesFIVEw[[3]][,1]))
beta.mean5 <- mean(c(paraSamplesFIVEw[[1]][,2], paraSamplesFIVEw[[2]][,2], paraSamplesFIVEw[[3]][,2]))

alpha5 <- c(paraSamplesFIVEw[[1]][,1], paraSamplesFIVEw[[2]][,1], paraSamplesFIVEw[[3]][,1])
beta5 <- c(paraSamplesFIVEw[[1]][,2], paraSamplesFIVEw[[2]][,2], paraSamplesFIVEw[[3]][,2])


predUV5 <- seq(min(uv5-0.0005), max(uv5+0.05), 0.05)
predictions <- array(dim = c(length(predUV5), length(alpha5)))
for(i in 1:length(predUV5)){
  predictions[i,] <- alpha5 + beta5 * predUV5[i]
}

UPB5 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope5<- alpha.mean5 + beta.mean5 * predUV5
LPB5 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB5
slope5
LPB5

predFIVE<- list(UPB5, slope5, LPB5, predUV5)
predFIVE<-as.data.frame(predFIVE)
colnames(predFIVE)<- c("UPB", "slope", "LPB", "predUV")


FIVE<-
  ggplot() +
  ggtitle("Mean of five weeks") +
  geom_point(data=sylvestris,aes(x=uv5, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predFIVE,aes(x=predUV5, y=exp(slope5)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFIVE,aes(x=predUV5, y=exp(UPB5)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predFIVE,aes(x=predUV5, y=exp(LPB5)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv5, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())  

############################################### SIX WEEKS ###############################################

uv6 <- as.numeric(sylvestris$w6)
uv6=scale(uv6)[,1]


alpha.mean6 <- mean(c(paraSamplesSIXw[[1]][,1], paraSamplesSIXw[[2]][,1], paraSamplesSIXw[[3]][,1]))
beta.mean6 <- mean(c(paraSamplesSIXw[[1]][,2], paraSamplesSIXw[[2]][,2], paraSamplesSIXw[[3]][,2]))

alpha6 <- c(paraSamplesSIXw[[1]][,1], paraSamplesSIXw[[2]][,1], paraSamplesSIXw[[3]][,1])
beta6 <- c(paraSamplesSIXw[[1]][,2], paraSamplesSIXw[[2]][,2], paraSamplesSIXw[[3]][,2])


predUV6 <- seq(min(uv6-0.0005), max(uv6+0.05), 0.05)
predictions <- array(dim = c(length(predUV6), length(alpha6)))
for(i in 1:length(predUV6)){
  predictions[i,] <- alpha6 + beta6 * predUV6[i]
}

UPB6 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope6<- alpha.mean6 + beta.mean6 * predUV6
LPB6 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB6
slope6
LPB6

predSIX<- list(UPB6, slope6, LPB6, predUV6)
predSIX<-as.data.frame(predSIX)
colnames(predSIX)<- c("UPB", "slope", "LPB", "predUV")


SIX<-
  ggplot() +
  ggtitle("Mean of six weeks") +
  geom_point(data=sylvestris,aes(x=uv6, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predSIX,aes(x=predUV6, y=exp(slope6)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSIX,aes(x=predUV6, y=exp(UPB6)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSIX,aes(x=predUV6, y=exp(LPB6)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv6, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### SEVEN WEEKS ###############################################

uv7 <- as.numeric(sylvestris$w7)
uv7=scale(uv7)[,1]


alpha.mean7 <- mean(c(paraSamplesSEVENw[[1]][,1], paraSamplesSEVENw[[2]][,1], paraSamplesSEVENw[[3]][,1]))
beta.mean7 <- mean(c(paraSamplesSEVENw[[1]][,2], paraSamplesSEVENw[[2]][,2], paraSamplesSEVENw[[3]][,2]))

alpha7 <- c(paraSamplesSEVENw[[1]][,1], paraSamplesSEVENw[[2]][,1], paraSamplesSEVENw[[3]][,1])
beta7 <- c(paraSamplesSEVENw[[1]][,2], paraSamplesSEVENw[[2]][,2], paraSamplesSEVENw[[3]][,2])


predUV7 <- seq(min(uv7-0.0005), max(uv7+0.05), 0.05)
predictions <- array(dim = c(length(predUV7), length(alpha7)))
for(i in 1:length(predUV7)){
  predictions[i,] <- alpha7 + beta7 * predUV7[i]
}

UPB7 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope7<- alpha.mean7 + beta.mean7 * predUV7
LPB7 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB7
slope7
LPB7

predSEVEN<- list(UPB7, slope7, LPB7, predUV7)
predSEVEN<-as.data.frame(predSEVEN)
colnames(predSEVEN)<- c("UPB", "slope", "LPB", "predUV")


SEVEN<-
  ggplot() +
  ggtitle("Mean of seven weeks") +
  geom_point(data=sylvestris,aes(x=uv7, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predSEVEN,aes(x=predUV7, y=exp(slope7)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSEVEN,aes(x=predUV7, y=exp(UPB7)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predSEVEN,aes(x=predUV7, y=exp(LPB7)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv7, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### EIGHT WEEKS ###############################################

uv8 <- as.numeric(sylvestris$w8)
uv8=scale(uv8)[,1]


alpha.mean8 <- mean(c(paraSamplesEIGHTw[[1]][,1], paraSamplesEIGHTw[[2]][,1], paraSamplesEIGHTw[[3]][,1]))
beta.mean8 <- mean(c(paraSamplesEIGHTw[[1]][,2], paraSamplesEIGHTw[[2]][,2], paraSamplesEIGHTw[[3]][,2]))

alpha8 <- c(paraSamplesEIGHTw[[1]][,1], paraSamplesEIGHTw[[2]][,1], paraSamplesEIGHTw[[3]][,1])
beta8 <- c(paraSamplesEIGHTw[[1]][,2], paraSamplesEIGHTw[[2]][,2], paraSamplesEIGHTw[[3]][,2])


predUV8 <- seq(min(uv8-0.0005), max(uv8+0.05), 0.05)
predictions <- array(dim = c(length(predUV8), length(alpha8)))
for(i in 1:length(predUV8)){
  predictions[i,] <- alpha8 + beta8 * predUV8[i]
}

UPB8 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope8<- alpha.mean8 + beta.mean8 * predUV8
LPB8 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB8
slope8
LPB8

predEIGHT<- list(UPB8, slope8, LPB8, predUV8)
predEIGHT<-as.data.frame(predEIGHT)
colnames(predEIGHT)<- c("UPB", "slope", "LPB", "predUV")


EIGHT<-
  ggplot() +
  ggtitle("Mean of eight weeks") +
  geom_point(data=sylvestris,aes(x=uv8, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predEIGHT,aes(x=predUV8, y=exp(slope8)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predEIGHT,aes(x=predUV8, y=exp(UPB8)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predEIGHT,aes(x=predUV8, y=exp(LPB8)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv8, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### NINE WEEKS ###############################################

uv9 <- as.numeric(sylvestris$w9)
uv9=scale(uv9)[,1]


alpha.mean9 <- mean(c(paraSamplesNINEw[[1]][,1], paraSamplesNINEw[[2]][,1], paraSamplesNINEw[[3]][,1]))
beta.mean9 <- mean(c(paraSamplesNINEw[[1]][,2], paraSamplesNINEw[[2]][,2], paraSamplesNINEw[[3]][,2]))

alpha9 <- c(paraSamplesNINEw[[1]][,1], paraSamplesNINEw[[2]][,1], paraSamplesNINEw[[3]][,1])
beta9 <- c(paraSamplesNINEw[[1]][,2], paraSamplesNINEw[[2]][,2], paraSamplesNINEw[[3]][,2])


predUV9 <- seq(min(uv9-0.0005), max(uv9+0.05), 0.05)
predictions <- array(dim = c(length(predUV9), length(alpha9)))
for(i in 1:length(predUV9)){
  predictions[i,] <- alpha9 + beta9 * predUV9[i]
}

UPB9 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope9<- alpha.mean9 + beta.mean9 * predUV9
LPB9 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB9
slope9
LPB9

predNINE<- list(UPB9, slope9, LPB9, predUV9)
predNINE<-as.data.frame(predNINE)
colnames(predNINE)<- c("UPB", "slope", "LPB", "predUV")


NINE<-
  ggplot() +
  ggtitle("Mean of nine weeks") +
  geom_point(data=sylvestris,aes(x=uv9, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predNINE,aes(x=predUV9, y=exp(slope9)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predNINE,aes(x=predUV9, y=exp(UPB9)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predNINE,aes(x=predUV9, y=exp(LPB9)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv9, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### TEN WEEKS ###############################################

uv10 <- as.numeric(sylvestris$w10)
uv10=scale(uv10)[,1]


alpha.mean10 <- mean(c(paraSamplesTENw[[1]][,1], paraSamplesTENw[[2]][,1], paraSamplesTENw[[3]][,1]))
beta.mean10 <- mean(c(paraSamplesTENw[[1]][,2], paraSamplesTENw[[2]][,2], paraSamplesTENw[[3]][,2]))

alpha10 <- c(paraSamplesTENw[[1]][,1], paraSamplesTENw[[2]][,1], paraSamplesTENw[[3]][,1])
beta10 <- c(paraSamplesTENw[[1]][,2], paraSamplesTENw[[2]][,2], paraSamplesTENw[[3]][,2])


predUV10 <- seq(min(uv10-0.0005), max(uv10+0.05), 0.05)
predictions <- array(dim = c(length(predUV10), length(alpha10)))
for(i in 1:length(predUV10)){
  predictions[i,] <- alpha10 + beta10 * predUV10[i]
}

UPB10 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope10<- alpha.mean10 + beta.mean10 * predUV10
LPB10 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB10
slope10
LPB10

predTEN<- list(UPB10, slope10, LPB10, predUV10)
predTEN<-as.data.frame(predTEN)
colnames(predTEN)<- c("UPB", "slope", "LPB", "predUV")


TEN<-
  ggplot() +
  ggtitle("Mean of ten weeks") +
  geom_point(data=sylvestris,aes(x=uv10, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTEN,aes(x=predUV10, y=exp(slope10)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTEN,aes(x=predUV10, y=exp(UPB10)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTEN,aes(x=predUV10, y=exp(LPB10)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv10, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### ELEVEN WEEKS ###############################################

uv11 <- as.numeric(sylvestris$w11)
uv11=scale(uv11)[,1]


alpha.mean11 <- mean(c(paraSamplesELEVENw[[1]][,1], paraSamplesELEVENw[[2]][,1], paraSamplesELEVENw[[3]][,1]))
beta.mean11 <- mean(c(paraSamplesELEVENw[[1]][,2], paraSamplesELEVENw[[2]][,2], paraSamplesELEVENw[[3]][,2]))

alpha11 <- c(paraSamplesELEVENw[[1]][,1], paraSamplesELEVENw[[2]][,1], paraSamplesELEVENw[[3]][,1])
beta11 <- c(paraSamplesELEVENw[[1]][,2], paraSamplesELEVENw[[2]][,2], paraSamplesELEVENw[[3]][,2])


predUV11 <- seq(min(uv11-0.0005), max(uv11+0.05), 0.05)
predictions <- array(dim = c(length(predUV11), length(alpha11)))
for(i in 1:length(predUV11)){
  predictions[i,] <- alpha11 + beta11 * predUV11[i]
}

UPB11 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope11<- alpha.mean11 + beta.mean11 * predUV11
LPB11 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB11
slope11
LPB11

predELEVEN<- list(UPB11, slope11, LPB11, predUV11)
predELEVEN<-as.data.frame(predELEVEN)
colnames(predELEVEN)<- c("UPB", "slope", "LPB", "predUV")


ELEVEN<-
  ggplot() +
  ggtitle("Mean of eleven weeks") +
  geom_point(data=sylvestris,aes(x=uv11, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predELEVEN,aes(x=predUV11, y=exp(slope11)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predELEVEN,aes(x=predUV11, y=exp(UPB11)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predELEVEN,aes(x=predUV11, y=exp(LPB11)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv11, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### TWELVE WEEKS ###############################################

uv12 <- as.numeric(sylvestris$w12)
uv12=scale(uv12)[,1]


alpha.mean12 <- mean(c(paraSamplesTWELVEw[[1]][,1], paraSamplesTWELVEw[[2]][,1], paraSamplesTWELVEw[[3]][,1]))
beta.mean12 <- mean(c(paraSamplesTWELVEw[[1]][,2], paraSamplesTWELVEw[[2]][,2], paraSamplesTWELVEw[[3]][,2]))

alpha12 <- c(paraSamplesTWELVEw[[1]][,1], paraSamplesTWELVEw[[2]][,1], paraSamplesTWELVEw[[3]][,1])
beta12 <- c(paraSamplesTWELVEw[[1]][,2], paraSamplesTWELVEw[[2]][,2], paraSamplesTWELVEw[[3]][,2])


predUV12 <- seq(min(uv12-0.0005), max(uv12+0.05), 0.05)
predictions <- array(dim = c(length(predUV12), length(alpha12)))
for(i in 1:length(predUV12)){
  predictions[i,] <- alpha12 + beta12 * predUV12[i]
}

UPB12 <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slope12<- alpha.mean12 + beta.mean12 * predUV12
LPB12 <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPB12
slope12
LPB12

predTWELVE<- list(UPB12, slope12, LPB12, predUV12)
predTWELVE<-as.data.frame(predTWELVE)
colnames(predTWELVE)<- c("UPB", "slope", "LPB", "predUV")


TWELVE<-
  ggplot() +
  ggtitle("Mean of twelve weeks") +
  geom_point(data=sylvestris,aes(x=uv12, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(slope12)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(UPB12)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(LPB12)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uv12, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 

############################################### GS WEEKS ###############################################

uvGS <- as.numeric(sylvestris$gs_mean)
uvGS=scale(uvGS)[,1]


alpha.meanGS <- mean(c(paraSamplesGSw[[1]][,1], paraSamplesGSw[[2]][,1], paraSamplesGSw[[3]][,1]))
beta.meanGS <- mean(c(paraSamplesGSw[[1]][,2], paraSamplesGSw[[2]][,2], paraSamplesGSw[[3]][,2]))

alphaGS <- c(paraSamplesGSw[[1]][,1], paraSamplesGSw[[2]][,1], paraSamplesGSw[[3]][,1])
betaGS <- c(paraSamplesGSw[[1]][,2], paraSamplesGSw[[2]][,2], paraSamplesGSw[[3]][,2])


predUVGS <- seq(min(uvGS-0.0005), max(uvGS+0.05), 0.05)
predictions <- array(dim = c(length(predUVGS), length(alphaGS)))
for(i in 1:length(predUVGS)){
  predictions[i,] <- alphaGS + betaGS * predUVGS[i]
}

UPBGS <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slopeGS<- alpha.meanGS + beta.meanGS * predUVGS
LPBGS <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPBGS
slopeGS
LPBGS

predGS<- list(UPBGS, slopeGS, LPBGS, predUVGS)
predGS<-as.data.frame(predGS)
colnames(predGS)<- c("UPB", "slope", "LPB", "predUV")


GS<-
  ggplot() +
  ggtitle("Mean of growing season") +
  geom_point(data=sylvestris,aes(x=uvGS, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predGS,aes(x=predUVGS, y=exp(slopeGS)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predGS,aes(x=predUVGS, y=exp(UPBGS)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predGS,aes(x=predUVGS, y=exp(LPBGS)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uvGS, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 


############################################### CLIM WEEKS ###############################################

uvCLIM <- as.numeric(sylvestris$year_mean)
uvCLIM=scale(uvCLIM)[,1]


alpha.meanCLIM <- mean(c(paraSamplesCLIMw[[1]][,1], paraSamplesCLIMw[[2]][,1], paraSamplesCLIMw[[3]][,1]))
beta.meanCLIM <- mean(c(paraSamplesCLIMw[[1]][,2], paraSamplesCLIMw[[2]][,2], paraSamplesCLIMw[[3]][,2]))

alphaCLIM <- c(paraSamplesCLIMw[[1]][,1], paraSamplesCLIMw[[2]][,1], paraSamplesCLIMw[[3]][,1])
betaCLIM <- c(paraSamplesCLIMw[[1]][,2], paraSamplesCLIMw[[2]][,2], paraSamplesCLIMw[[3]][,2])


predUVCLIM <- seq(min(uvCLIM-0.0005), max(uvCLIM+0.05), 0.05)
predictions <- array(dim = c(length(predUVCLIM), length(alphaCLIM)))
for(i in 1:length(predUVCLIM)){
  predictions[i,] <- alphaCLIM + betaCLIM * predUVCLIM[i]
}

UPBCLIM <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slopeCLIM<- alpha.meanCLIM + beta.meanCLIM * predUVCLIM
LPBCLIM <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPBCLIM
slopeCLIM
LPBCLIM

predCLIM<- list(UPBCLIM, slopeCLIM, LPBCLIM, predUVCLIM)
predCLIM<-as.data.frame(predCLIM)
colnames(predCLIM)<- c("UPB", "slope", "LPB", "predUV")


CLIM<-
  ggplot() +
  ggtitle("Annual mean 2005-2015") +
  geom_point(data=sylvestris,aes(x=uvCLIM, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(slopeCLIM)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(UPBCLIM)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(LPBCLIM)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uvCLIM, y=ratio/200, label = sylvestris$arb),size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### LAT WEEKS ###############################################

uvLAT <- as.numeric(sylvestris$lat)
uvLAT=scale(uvLAT)[,1]


alpha.meanLAT <- mean(c(paraSamplesLATw[[1]][,1], paraSamplesLATw[[2]][,1], paraSamplesLATw[[3]][,1]))
beta.meanLAT <- mean(c(paraSamplesLATw[[1]][,2], paraSamplesLATw[[2]][,2], paraSamplesLATw[[3]][,2]))

alphaLAT <- c(paraSamplesLATw[[1]][,1], paraSamplesLATw[[2]][,1], paraSamplesLATw[[3]][,1])
betaLAT <- c(paraSamplesLATw[[1]][,2], paraSamplesLATw[[2]][,2], paraSamplesLATw[[3]][,2])


predUVLAT <- seq(min(uvLAT-0.0005), max(uvLAT+0.05), 0.05)
predictions <- array(dim = c(length(predUVLAT), length(alphaLAT)))
for(i in 1:length(predUVLAT)){
  predictions[i,] <- alphaLAT + betaLAT * predUVLAT[i]
}

UPBLAT <- apply(predictions, 1, quantile, probs = 0.975) # Upper bound
slopeLAT<- alpha.meanLAT + beta.meanLAT * predUVLAT
LPBLAT <- apply(predictions, 1, quantile, probs = 0.015) # Lower bound
UPBLAT
slopeLAT
LPBLAT

predLAT<- list(UPBLAT, slopeLAT, LPBLAT, predUVLAT)
predLAT<-as.data.frame(predLAT)
colnames(predLAT)<- c("UPB", "slope", "LPB", "predUV")


LAT<-
  ggplot() +
  ggtitle("Latitude") +
  geom_point(data=sylvestris,aes(x=uvLAT, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_line(data=predLAT,aes(x=predUVLAT, y=exp(slopeLAT)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predLAT,aes(x=predUVLAT, y=exp(UPBLAT)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predLAT,aes(x=predUVLAT, y=exp(LPBLAT)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestris, aes(x=uvLAT, y=ratio/200, label = sylvestris$arb), size= 3, vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(plot.title = element_text(size = 11), 
        axis.title.y = element_text(color='black', size=10,vjust=1),
        axis.text.y = element_text(color='black', size=8, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=10,vjust=1),
        axis.text.x = element_text(color='black', size=8, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 






#Paper III
source("http://peterhaschke.com/Code/multiplot.R")
pdf("Figure1_ESM.pdf", width = 8.9, height = 11.7, useDingbats=FALSE)
multiplot(ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, ELEVEN, TWELVE, GS, CLIM, LAT, cols=3)
dev.off()


