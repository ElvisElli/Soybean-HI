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
setwd(dirname(getActiveDocumentContext()$path))

#GUI_juicr("duvick2004.jpg")

GUI_juicr("5.JPG")

