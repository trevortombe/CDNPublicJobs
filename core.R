rm(list=ls(all=TRUE)) # wipes previous workspace

# Common Packages
packages<-c("rtweet","curl","mapproj","scales","zoo","dplyr",
            "RColorBrewer","tidyverse","ggalt","gridExtra","ggridges",
            "ggpubr","testit","ggseas","readxl","grid","gghighlight",
            "stringr","extrafont","directlabels","cansim",
            "ggplot2","ggthemes","tidyr","patchwork","jsonlite",
            "data.table","ggrepel","sp","spatstat","rmarkdown")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Your preferred color scheme (https://www.color-hex.com/color-palette/33490)
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
colbar<-c("#D35E60","#7293CB","#84BA5B","#E1974C","#808585","#9067A7","#AB6857","#CCC210")
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col)
}

# Useful lists
provinces<-c("Canada","Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
             "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
             "Alberta","British Columbia","Yukon","Northwest Territories","Nunavut")
tenprov<-c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
           "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
           "Alberta","British Columbia")
provinces2<-c("CAN","NL","PE","NS",
              "NB","QC","ON","MB","SK",
              "AB","BC","YT","NT","NU")
provsort<-c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL")
provnames<-data.frame(GEO=provinces,short=provinces2)
provnames$short <- factor(provnames$short, levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")) # Lock in factor level order
provorder<-tibble(GEO=c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
                  order=as.numeric(seq(1,10)))

# For the new StatCan Data Tables
getTABLE<-function(x) {
  url<-paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",x,"-eng.zip")
  temp<-tempfile()
  download.file(url,temp)
  if (has_warning(unzip(temp,paste0(x,".csv")))) { # Avoids html landing page
    download.file(url,temp)
  }
  unzip(temp,paste0(x,".csv"))
  rawdata<-fread(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  data<-rawdata %>%
    dplyr::rename(Value=VALUE) %>%
    select(-UOM_ID,-SCALAR_ID) %>%
    dplyr::rename_all(list(~make.names(.))) # this replaces the spaces with dots in the column names
  if (class(data$Ref_Date)=="character" & !grepl("/",data[1,"Ref_Date"])){
    data<-data %>%
      mutate(Ref_Date=as.yearmon(Ref_Date))
  }
  if ("GEO" %in% colnames(data)){
    data <- data %>%
      left_join(provnames,by="GEO")
  }
  if ("North.American.Industry.Classification.System..NAICS." %in% colnames(data)){
    data <- data %>%
      rename(NAICS=North.American.Industry.Classification.System..NAICS.) %>%
      mutate(NAICScode=str_match(NAICS,"\\[(.*?)\\]")[,2],
             NAICS=ifelse(regexpr(" \\[",NAICS)>1,
                          substr(NAICS,1,regexpr(" \\[",NAICS)-1),NAICS))
  }
  if (any(grepl("North.American.Product.Classification.System..NAPCS.",colnames(data)))){
    colnames(data)[grepl("North.American.Product.Classification.System..NAPCS.",colnames(data))]<-"NAPCS"
    data <- data %>%
      mutate(NAPCS=ifelse(regexpr(" \\[",NAPCS)>1,
                          substr(NAPCS,1,regexpr(" \\[",NAPCS)-1),NAPCS))
  }
  sourcetable<-gsub("(\\d{2})(\\d{2})(\\d{4})$","\\1-\\2-\\3",x)
  comment(data)<-paste("Statistics Canada data table",sourcetable)
  return(data)
}
getTABLEraw<-function(x) {
  url<-paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",x,"-eng.zip")
  temp<-tempfile()
  download.file(url,temp)
  if (has_warning(unzip(temp,paste0(x,".csv")))) { # Avoids html landing page
    download.file(url,temp)
  }
  unzip(temp,paste0(x,".csv"))
  rawdata<-read.csv(paste0(x,".csv"),stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  return(rawdata)
}
loadTABLE<-function(x) {
  rawdata<-fread(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  data<-rawdata %>%
    dplyr::rename(Value=VALUE) %>%
    select(-UOM_ID,-SCALAR_ID) %>%
    dplyr::rename_all(list(~make.names(.))) # this replaces the spaces with dots in the column names
  if (class(data$Ref_Date)=="character" & !grepl("/",data$Ref_Date)){
    data<-data %>% #mutate(Ref_Date=ifelse(grepl("/",Ref_Date),Ref_Date,Ref_Date=as.yearmon(Ref_Date)))
      mutate(Ref_Date=as.yearmon(Ref_Date))
  }
  if ("GEO" %in% colnames(data)){
    data <- data %>%
      left_join(provnames,by="GEO")
  }
  if ("North.American.Industry.Classification.System..NAICS." %in% colnames(data)){
    data <- data %>%
      rename(NAICS=North.American.Industry.Classification.System..NAICS.) %>%
      mutate(NAICScode=str_match(NAICS,"\\[(.*?)\\]")[,2],
             NAICS=ifelse(regexpr(" \\[",NAICS)>1,
                          substr(NAICS,1,regexpr(" \\[",NAICS)-1),NAICS))
  }
  if (any(grepl("North.American.Product.Classification.System..NAPCS.",colnames(data)))){
    colnames(data)[grepl("North.American.Product.Classification.System..NAPCS.",colnames(data))]<-"NAPCS"
    data <- data %>%
      mutate(NAPCS=ifelse(regexpr(" \\[",NAPCS)>1,
                          substr(NAPCS,1,regexpr(" \\[",NAPCS)-1),NAPCS))
  }
  sourcetable<-gsub("(\\d{2})(\\d{2})(\\d{4})$","\\1-\\2-\\3",x)
  comment(data)<-paste("Statistics Canada data table",sourcetable)
  return(data)
}
# Function to download the StatCan Delta File
getDELTA<-function(x) {
  url<-paste0("https://www150.statcan.gc.ca/n1/delta/",x,".zip") ## date format: YYYYMMDD
  temp<-tempfile()
  download.file(url,temp)
  unzip(temp,paste0(x,".csv"))
  data<-fread(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE) %>%
    mutate(Ref_Date=as.yearmon(refPer,"%Y-%m"),
           VECTOR=as.character(paste0("v",vectorId)))
  return(data)
}
loadDELTA<-function(x) {
  data<-fread(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE) %>%
    mutate(Ref_Date=as.yearmon(refPer,"%Y-%m"),
           VECTOR=as.character(paste0("v",vectorId)))
  return(data)
}
# Function to update any CANSIM data set with the new Delta File
update_data<-function(df){
  if (("value" %in% colnames(df)) | ("value_orig" %in% colnames(df))){ # need this in case you've previously used update_data()
    df<-df %>% select(-value,-value_orig)
  }
  temp<-deltafile %>% filter(VECTOR %in% unique(df$VECTOR))
  temp2<-CJ(Ref_Date=unique(c(unique(df$Ref_Date),unique(temp$Ref_Date))),
            VECTOR=unique(df$VECTOR)) %>%
    left_join(df %>% filter(Ref_Date==max(Ref_Date)) %>% select(-Value,-Ref_Date),by=c("VECTOR")) %>%
    left_join(df %>% select(Ref_Date,VECTOR,Value),by=c("Ref_Date","VECTOR")) %>%
    left_join(temp %>% select(Ref_Date,VECTOR,value),by=c("Ref_Date","VECTOR")) %>%
    mutate(value_orig=Value,
           Value=ifelse(is.na(value),Value,value))
  return(temp2)
}


# Project dataset "x" from date "y" onwards for z years
ProjectOut<-function(x,y,z) {
  startextra=y
  plotstart=as.numeric(min(x$Ref_Date))
  plotend=as.numeric(as.yearmon(y)+z)
  p<-ggplot(x,aes(Ref_Date,Value))+
    stat_smooth(data=subset(x,x$Ref_Date<=startextra),se=FALSE,method="lm",fullrange=TRUE,size=2)+
    scale_x_yearmon(limits=c(plotstart,plotend))
  extrapolate<-ggplot_build(p)$data[[1]]
  extrapolate<-extrapolate[extrapolate$x>=as.numeric(as.yearmon(startextra)),]
  extrapolate$x<-as.yearmon(extrapolate$x)
  return(extrapolate)
}

# Default theme
#update_geom_defaults("point", list(colour = "firebrick"))
#update_geom_defaults("line", list(colour = "firebrick"))

# Instructions to add additional fonts
# go to fonts.google.com and pick a font; download it; unzip it
# use font_import("../Downloads/blaablaa")
# font_import("../Downloads/Quicksand")
# then run loadfonts()
loadfonts()

extrafont::loadfonts(device="win") # needed for fonts (on windows, not sure about unix/mac)
mytheme<-theme_minimal()+theme(
  #text = element_text(family="Quicksand Medium"),
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10,margin = margin(r = 10, unit = "pt")),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  legend.title=element_blank(),
  strip.background = element_rect(fill="gray90",color="transparent"),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold",size=14),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)
mythemebar<-mytheme+theme(
  panel.grid.major.x = element_blank(),
  axis.text.x = element_text(size=12,hjust=0.5,face="bold",colour="black")
)
mythemebarflip<-theme_minimal()+theme(
  #text = element_text(family="Quicksand Medium"),
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  legend.title=element_blank(),
  plot.caption = element_text(size = 6, color = "gray40"),
  plot.title = element_text(face = "bold",size=14),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank()
)
mythemegif<-mytheme+theme(
  axis.title.y = element_text(size=12),
  axis.title.x = element_text(size=12),
  axis.text.y = element_text(size=12),
  axis.text.x = element_text(size=12,hjust=0.5),
  panel.grid.major.x = element_blank(),
  plot.subtitle = element_text(size = 10, face = "italic"),
  plot.title = element_text(size = 20, face = "bold")
)
mythememap<-theme(
  #text = element_text(family="Quicksand Medium"),
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.y=element_blank(),
  axis.ticks.x=element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.position="top",
  legend.text=element_text(size=10),
  plot.title = element_text(size = 16, face = "bold",hjust=0.5),
  plot.subtitle = element_text(size = 7, color="gray50",hjust=0.5),
  plot.caption = element_text(size = 6, color="gray50")
)

# This replicates the StatsCan trend-cycle estimate
gettrend<-function(x,y){
  x<-x %>%
    mutate(periodsleft=12*(max(Ref_Date)-Ref_Date)) %>%
    rename(var=y) %>%
    mutate(TC=-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
             0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
             -0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
             0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1),
           TC5=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  -0.007*lead(var,5)+0.031*lead(var,4)+
                  0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027),
           TC4=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.031*lead(var,4)+
                  0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007),
           TC3=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031),
           TC2=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067),
           TC1=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+
                  0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067-0.136),
           TC0=(-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+
                  0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var)/(1+0.027+0.007-0.031-0.067-0.136-0.188)) %>%
    mutate(TC=ifelse(round(periodsleft,0)==5,TC5,TC),
           TC=ifelse(round(periodsleft,0)==4,TC4,TC),
           TC=ifelse(round(periodsleft,0)==3,TC3,TC),
           TC=ifelse(round(periodsleft,0)==2,TC2,TC),
           TC=ifelse(round(periodsleft,0)==1,TC1,TC),
           TC=ifelse(round(periodsleft,0)==0,TC0,TC)) %>%
    mutate(TC=ifelse(row_number()==1,
                     (0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067-0.136-0.188),TC),
           TC=ifelse(row_number()==2,
                     (0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067-0.136),TC),
           TC=ifelse(row_number()==3,
                     (0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031-0.067),TC),
           TC=ifelse(row_number()==4,
                     (0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007-0.031),TC),
           TC=ifelse(row_number()==5,
                     (0.031*lag(var,4)+0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027+0.007),TC),
           TC=ifelse(row_number()==6,
                     (-0.007*lag(var,5)+0.031*lag(var,4)+0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1))/(1+0.027),TC),
           TC=ifelse(row_number()==7,
                     (-0.027*lag(var,6)-0.007*lag(var,5)+0.031*lag(var,4)+0.067*lag(var,3)+0.136*lag(var,2)+0.188*lag(var,1)+0.224*var+-0.027*lead(var,6)-0.007*lead(var,5)+0.031*lead(var,4)+
                        0.067*lead(var,3)+0.136*lead(var,2)+0.188*lead(var,1)),TC)) %>%
    rename(setNames("var", y)) %>%
    select(-TC0,-TC1,-TC2,-TC3,-TC4,-TC5,-periodsleft)
  return(x)
}

# Construct own within-group seasonal adjustment with trend
getseas<-function(df,g){
  df<-df %>%
    rename(group_var=g)
  if (("value" %in% colnames(df)) | ("value_orig" %in% colnames(df))){ # need this in case you've previously used update_data()
    df<-df %>% select(-value,-value_orig)
  }
  p<-ggsdc(df,aes(Ref_Date,Value,group=group_var,color=group_var),
           method="seas")+geom_line()
  temp<-p$data %>%
    filter(component %in% c("irregular","trend")) %>%
    group_by(x,group_var) %>%
    summarise(Value=sum(y)) %>%
    group_by(group_var) %>%
    rename(Ref_Date=x) %>%
    gettrend("Value") %>%
    rename(setNames("group_var", g)) %>%
    ungroup()
  return(temp)
}

# Adjust the cansim package for fetching specific vectors
getVECTOR<-function(v,d){
  vector_data<-get_cansim_vector(v,d)
  labels<-get_cansim_vector_info(v)
  column_names<-get_cansim_column_list(labels[1,]$table,language="english")
  labels2<-data.frame(do.call('rbind',strsplit(labels$title,";",fixed=T)))
  colnames(labels2)<-column_names$`Dimension name`
  if (any(grepl("NAICS",colnames(labels2)))){
    labels2 <- labels2 %>%
      rename(NAICS=`North American Industry Classification System (NAICS)`)
  }
  data<-vector_data %>%
    select(Value=VALUE,VECTOR,Ref_Date=REF_DATE) %>%
    mutate(Ref_Date=as.yearmon(Ref_Date)) %>%
    left_join(labels %>% select(VECTOR) %>%
                cbind(labels2),by="VECTOR") %>%
    rename(GEO=Geography) %>%
    left_join(provnames,by="GEO")
  comment(data)<-paste("Statistics Canada data table",labels[1,]$table)
  return(data)
}
