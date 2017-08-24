rm(list=ls(all=TRUE)) # wipes previous workspace

# Packages used by this code
library(zoo)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(ggrepel)
library(Quandl)
library(jsonlite)
library(data.table)
library(ggalt)
library(grid)
library(gridExtra)
library(readxl)

# Function to get the CANSIM Table for the Data
getCANSIM<-function(x) {
  url<-paste("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/0",x,"-eng.zip",sep="")
  url
  temp<-tempfile()
  download.file(url,temp)
  unzip(temp,paste("0",x,"-eng.csv",sep=""))
  data<-read.csv(paste("0",x,"-eng.csv",sep=""),stringsAsFactors=FALSE)
  data$Value<-as.numeric(as.character(data$Value)) # Convert factors to numeric
  data<-data.table(data)
  return(data)
}

# Default theme
mytheme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 6, color = "gray40"),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)
mythemebar<-mytheme+theme(
  panel.grid.major.x = element_blank(),
  axis.text.x = element_text(size=12,hjust=0.5,face="bold",colour="black")
)

# Useful lists
provinces<-c("Canada","Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
             "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
             "Alberta","British Columbia")
provinces2<-c("CAN","NL","PE","NS",
             "NB","QC","ON","MB","SK",
             "AB","BC")
provnames<-data.table(GEO=provinces,short=provinces2)
provnames$short <- factor(provnames$short, levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YK","NT","NU")) # Lock in factor level order
