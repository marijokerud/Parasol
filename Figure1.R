### PARASOL UV-B Figure 1 ♥
library(ggplot2)
library(grid)
library(cowplot)

setwd() # Set working directory
weeks2 <- read.csv("weeks2.csv", header=TRUE, sep = ";") 
head(weeks2)
summary(weeks2)

var2<- tail(weeks2, 8)
var2$data<- as.factor(var2$data)
weeks2<- head(weeks2, 24)
weeks2$data<- as.factor(weeks2$data)

cbPalette <- c("black","black","black","black","black","black","black","black","black","black")
cbPalette1<- c("black", "white", "black","black","black", "black", "white","white","white","white","white") 

#Slope
slope<-
  ggplot() +
  geom_point(data=weeks2,aes(x=no, y=beta, color=data, shape=data, fill=data), alpha=1, size=1.5) +
  geom_smooth(data=weeks2,aes(x=no, y=beta, color=data, linetype= data), alpha=.5, size=0.8,
              method = "gam", formula= y~x, se = FALSE) +
  geom_vline(xintercept = 13) +
  geom_point(data=var2,aes(x=no, y=beta, color=data, shape=data, fill=data), alpha=1, size=2) +
  scale_shape_manual(values=c(21, 24, 21, 22, 23, 24, 21, 21, 22, 23)) +
  scale_color_manual(values = c(cbPalette)) +          #Set colour
  scale_fill_manual(values = c(cbPalette1)) +           #Set colour
    
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  theme_bw() +  
  xlab("") +                                                                 #Set x-axis title
  ylab(expression(beta)) +                                                   #Set y-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=0),
        axis.text.y = element_text(color='black', size=12, angle=0),
        axis.title.x = element_text(color='black', size=14,hjust=.3),
        axis.text.x = element_text(color='black', size=12, angle=0),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.major.x= element_blank(),
        panel.grid.major.y= element_blank())

#CRI without Benmore and Edinburgh
criWO<-
  ggplot() +
  geom_point(data=weeks2,aes(x=no, y=cri, color=data, shape=data, fill=data), alpha=1, size=1.5) +
  geom_smooth(data=weeks2,aes(x=no, y=cri, color=data, linetype= data), alpha=.5, size=0.8,
              method = "loess", formula= y~x, span = 1, se = FALSE) +
  geom_vline(xintercept = 13) +
  geom_point(data=var2,aes(x=no, y=cri, color=data, shape=data, fill=data), alpha=1, size=2) +
  scale_shape_manual(values=c(21, 24, 21, 22, 23, 24, 21, 21, 22, 23)) +
  scale_color_manual(values = c(cbPalette)) +          #Set colour
  scale_fill_manual(values = c(cbPalette1)) +           #Set colour
  
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  theme_bw() +  
  xlab("") +                                                                 #Set x-axis title
  ylab("% CRI above 0") +                                                   #Set y-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=0),
        axis.text.y = element_text(color='black', size=12, angle=0),
        axis.title.x = element_text(color='black', size=14,hjust=.3),
        axis.text.x = element_text(color='black', size=12, angle=0),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.major.x= element_blank(),
        panel.grid.major.y= element_blank())


#DIC DATA
weeks <- read.csv("weeks.csv", header=TRUE, sep = ";") 
weeks$data<- as.factor(weeks$data)
head(weeks)
summary(weeks)

weeksW<- head(weeks, 16)
varW<- tail(weeksW, 4)
weeksW<- head(weeks, 12)

weeksWO<- tail(weeks, 16)
varWO<- tail(weeksWO, 4)
weeksWO<- head(weeksWO, 12)


#DIC without Benmore and Edinburgh
dicWO<-
  ggplot() +
  geom_point(data=weeksWO,aes(x=no, y=dic, shape=data), alpha=1, size=2) +
  geom_vline(xintercept = 13) +
  geom_point(data=varWO,aes(x=no, y=dic, shape=data), alpha=1, size=2.5) +
  geom_smooth(data=weeksWO,aes(x=no, y=dic), colour="black", alpha=.5, size=0.8,
              method = "loess", formula= y~ log(x), span = 0.42, se = FALSE) +
  scale_shape_manual(values=c(16, 15, 15, 18, 17)) +
  
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  theme_bw() +                                                                      #Set normal plot colour
  xlab("") +                                                                        #Set x-axis title
  ylab("DIC") +                                     #Set x-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=0),
        axis.text.y = element_text(color='black', size=12, hjust=-0.1),
        axis.title.x = element_text(color='black', size=14,hjust=.3),
        axis.text.x = element_text(color='black', size=12, vjust=0),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.major.x= element_blank(),
        panel.grid.major.y= element_blank())



#DIC with Benmore and Edinburgh
dicW<-
  ggplot() +
  geom_point(data=weeksW,aes(x=no, y=dic, shape=data), alpha=1, size=2) +
  geom_vline(xintercept = 13) +
  geom_point(data=varW,aes(x=no, y=dic, shape=data), alpha=1, size=2.5) +
  geom_smooth(data=weeksW,aes(x=no, y=dic), linetype= "dashed", colour="black", alpha=.5, size=0.8,
              method = "gam", formula= y~ log(x), se = FALSE) +
  scale_shape_manual(values=c(2, 1, 0, 0, 5)) +
  
  scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  theme_bw() +                                                                      #Set normal plot colour
  xlab("") +                                                                        #Set x-axis title
  ylab("DIC") +                                                   #Set x-axis title
  theme(axis.title.y = element_text(color='black', size=14,vjust=0),
        axis.text.y = element_text(color='black', size=12, hjust=0),
        axis.title.x = element_text(color='black', size=14,hjust=.3),
        axis.text.x = element_text(color='black', size=12, vjust=0),
        axis.ticks = element_blank(),
        legend.position ="none",
        panel.grid.major.x= element_blank(),
        panel.grid.major.y= element_blank())



plot_grid(slope, criWO, dicWO, dicW, labels = c("a)", "b)", "c)", "d)"), nrow = 4)

#Paper III
pdf("Figure1.pdf", width = 8.9, height = 12.6, useDingbats=FALSE)
plot_grid(slope, criWO, dicWO, dicW, labels = c("(a)", "(b)", "(c)", "(d)"), hjust = -0.15,
          label_size = 15, nrow = 4)
dev.off()

