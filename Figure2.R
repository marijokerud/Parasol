### PARASOL UV-B Figure 2 ♥

library(ggplot2)
library(grid)
library(cowplot)


setwd() # Set working directory
sylvestris <- read.csv("EurMeanPCA.csv", header=TRUE, sep = ";") #Full dataset
sylvestrisRED <- read.csv("EurMeanPCAred.csv", header=TRUE, sep = ";") #New dataset without Benmore and Edinburgh
sylvestrisBE <- read.csv("EurMeanPCA-BE.csv", header=TRUE, sep = ";") #New dataset with only Benmore and Edinburgh
head(sylvestrisBE)
summary(sylvestris)

ratio <- as.numeric(sylvestrisRED$ratio)
ratioFULL<- as.numeric(sylvestrisBE$ratio)
n <- nrow(sylvestrisRED) 
pCount <- as.numeric(sylvestrisRED$nGrains)

sylvestris$year_mean <- as.numeric(sylvestris$year_mean)
sylvestris$year_mean=scale(sylvestris$year_mean)[,1]


############################################### ONE WEEK ###############################################
uv1 <- as.numeric(sylvestrisRED$w1)
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

one<-
  ggplot() +
  geom_point(data=sylvestrisRED,aes(x=uv1, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_point(data=sylvestrisBE,aes(x=w1, y=ratio/200), color="red", alpha=1, size=1.2, show.legend=FALSE) + #Add Benmore and Edinburgh
  geom_line(data=predONE,aes(x=predUV1, y=exp(slope1)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(UPB1)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predONE,aes(x=predUV1, y=exp(LPB1)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestrisRED, aes(x=uv1, y=ratio/200, label = sylvestrisRED$arb), vjust= 1.5) +
  geom_text(data=sylvestrisBE, aes(x=w1, y=ratio/200, label = sylvestrisBE$arb), vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=1),
        axis.text.y = element_text(color='black', size=12, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=14,vjust=1),
        axis.text.x = element_text(color='black', size=12, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank()) 



############################################### TWO WEEK ###############################################
uv2 <- as.numeric(sylvestrisRED$w2)
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
LPB2 <- apply(predictions, 1, quantile, probs = 0.025) # Lower bound
UPB2
slope2
LPB2

predTwo<- list(UPB2, slope2, LPB2, predUV2)
predTwo<-as.data.frame(predTwo)
colnames(predTwo)<- c("UPB", "slope", "LPB", "predUV")


#Two weeks
two<-
  ggplot() +
  geom_point(data=sylvestrisRED,aes(x=uv2, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_point(data=sylvestrisBE,aes(x=w2, y=ratio/200), color="red", alpha=1, size=1.2, show.legend=FALSE) + #Add Benmore and Edinburgh
  geom_line(data=predTwo,aes(x=predUV2, y=exp(slope2)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTwo,aes(x=predUV2, y=exp(UPB2)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTwo,aes(x=predUV2, y=exp(LPB2)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestrisRED, aes(x=uv2, y=ratio/200, label = sylvestrisRED$arb), vjust= 1.5) +
  geom_text(data=sylvestrisBE, aes(x=w2, y=ratio/200, label = sylvestrisBE$arb), vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=1),
        axis.text.y = element_text(color='black', size=12, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=14,vjust=1),
        axis.text.x = element_text(color='black', size=12, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())




############################################### TWELVE WEEK ###############################################
uv12 <- as.numeric(sylvestrisRED$w12)
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


twelve<-
  ggplot() +
  geom_point(data=sylvestrisRED,aes(x=uv12, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_point(data=sylvestrisBE,aes(x=w12, y=ratio/200), color="red", alpha=1, size=1.2, show.legend=FALSE) + #Add Benmore and Edinburgh
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(slope12)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(UPB12)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predTWELVE,aes(x=predUV12, y=exp(LPB12)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestrisRED, aes(x=uv12, y=ratio/200, label = sylvestrisRED$arb), vjust= 1.5) +
  geom_text(data=sylvestrisBE, aes(x=w12, y=ratio/200, label = sylvestrisBE$arb), vjust= 1.5) +

  scale_x_continuous(breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=1),
        axis.text.y = element_text(color='black', size=12, angle=0, hjust=-0.1),
        axis.title.x = element_text(color='black', size=14,vjust=1),
        axis.text.x = element_text(color='black', size=12, angle=0, vjust=0.5),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


############################################### ANNUAL CLIM ###############################################
uvCLIM <- as.numeric(sylvestrisRED$year_mean)
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
LPBCLIM <- apply(predictions, 1, quantile, probs = 0.025) # Lower bound
UPBCLIM
slopeCLIM
LPBCLIM

predCLIM<- list(UPBCLIM, slopeCLIM, LPBCLIM, predUVCLIM)
predCLIM<-as.data.frame(predCLIM)
colnames(predCLIM)<- c("UPB", "slope", "LPB", "predUV")


clim<-
  ggplot() +
  geom_point(data=sylvestrisRED,aes(x=uvCLIM, y=ratio/200), color="black", alpha=1, size=1.2, show.legend=FALSE) +
  geom_point(data=sylvestrisBE,aes(x=year_mean, y=ratio/200), color="red", alpha=1, size=1.2, show.legend=FALSE) + #Add Benmore and Edinburgh
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(slopeCLIM)), color="black", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(UPBCLIM)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_line(data=predCLIM,aes(x=predUVCLIM, y=exp(LPBCLIM)), color="grey50", alpha=1, size=1, show.legend=FALSE) +
  geom_text(data=sylvestrisRED, aes(x=uvCLIM, y=ratio/200, label = sylvestrisRED$arb), vjust= 1.5) +
  geom_text(data=sylvestrisBE, aes(x=year_mean, y=ratio/200, label = sylvestrisBE$arb), vjust= 1.5) +
  
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_bw() +                                                          #Set normal plot colour
  xlab("Standardised UV-B") +                                           #Set x-axis title
  ylab("pCA ratio") +                                                   #Set x-axis title
  theme(axis.title.y = element_text(color='black', size=14),
        axis.text.y = element_text(color='black', size=12),
        axis.title.x = element_text(color='black', size=14),
        axis.text.x = element_text(color='black', size=12),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.minor.x= element_blank(),
        panel.grid.minor.y= element_blank())


Figure2<- 
  ggdraw() +
  draw_plot(one, 0, .5, .5, .5) +
  draw_plot(two, .5, .5, .5, .5) +
  
  draw_plot(twelve, 0, 0, .5, .5) +
  draw_plot(clim, .5, 0, .5, .5) +
  
  draw_plot_label(c("(a)", "(b)", "(c)", "(d)"), c(0, .5, 0, .5), c(1, 1, .5, .5), hjust = -0.2,  size = 15)


#Paper III
pdf("Figure2.pdf", width = 8.9, height = 8.9, useDingbats=FALSE)
plot_grid(Figure2)
dev.off()


