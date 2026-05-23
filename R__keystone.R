rm(list = ls(all.names = TRUE))
setwd("C:/Users/mattd/Desktop/Data")

library(readxl)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(date)
library(reshape2)


key<- read_excel("joey.xlsx", sheet = 1)
spec<- read_excel("joey.xlsx", sheet = 2)
key$ci<- as.numeric(key$ci)
key$ci<- abs(key$ci)


key<-merge(key, spec, by= "species")


ggplot(key, aes(x=species, y=log(abs(ci)), col= Ecosystem))+
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=4)+
 # ylim(.0, 2)+
  labs(x='Taxa', y= "Community Impact") +
  theme(axis.text.x=element_text(size= 12, angle = 90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


key$ciap<-as.numeric(key$ciap)

ggplot(key, aes(x=species, y=log(abs(ciap)), col= Ecosystem))+
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=4)+
  # ylim(.0, 2)+
  theme(axis.text.x=element_text(size= 12, angle = 90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

ggplot(key, aes(x=species, y=ci, col= Ecosystem))+
  geom_jitter(shape=16, size= 3, position=position_jitter(0.2)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=4)+
  theme(axis.text.x=element_text(size= 12, angle = 90),
        axis.text.y=element_text(size= 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))

ggplot(key, aes(x=Ecosystem, y=log(ci), col= Ecosystem))+
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=4) +
  labs(x='Ecosystem', y= "Community Impact") +
  theme(axis.text.x=element_text(size= 12, angle = 90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

specvar<-aggregate(ci~species + Ecosystem, key, var)

ggplot(specvar, aes(x=species, y=abs(log(ci)), col= Ecosystem))+
  geom_jitter(shape=16, size= 3, position=position_jitter(0.2)) +
  geom_boxplot(outlier.colour="black", outlier.shape=8, outlier.size=4)+
  labs(x='Species', y= "Context Dependency (Var)")+
  theme(axis.text.x=element_text(size= 12, angle = 90),
        axis.text.y=element_text(size= 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 1))


