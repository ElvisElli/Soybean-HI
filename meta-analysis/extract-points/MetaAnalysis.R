## extract data from .jpeg or png files using the juicr package 
rm(list=ls())

## load package
library(juicr)
library(rstudioapi)
library(readxl)
library(readr)
library(dplyr)
library(ggpubr)#equation in the plot
library(broom)
library(tidyr)

##plot theme
source("../plot_theme.R")

setwd(dirname(getActiveDocumentContext()$path))

#GUI_juicr("duvick2004.jpg")

GUI_juicr("fig.JPG")

duvick <- read.csv("duvick2004_juicr_extracted_points.csv") %>% 
  mutate(study="Duvick et al. (2004)")

ggplot(data=duvick,aes(x=YOR,y=LA))+
  geom_point()+
  geom_smooth(method = "lm",se=F,colour="blue")+
  stat_regline_equation(label.y = 4,label.x = 1940,colour="blue")+
  temp+
  ggtitle("Duvick et al (2004)")+
  scale_x_continuous(limits = c(1920,2010),breaks = c(1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))

ggsave("Duvick.et.al.2004_US.tiff",width=15,height=13,units ="cm",dpi=600,compression="lzw")