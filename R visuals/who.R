# Average number of cases of child(<5yrs) death in 2013-2018. World map and Uganda, based on WHO data
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(tmap)
library(foreign)
#install.packages("Hmisc")
#install.packages("sf")
library(Hmisc)
library(sf)

options(scipen = 999)
setwd("C:/Users/ZEF/Desktop/madina/programming")

####WHO data
WHO_data <- read.csv("who both sexes country level.csv")

##### Average number of cases of child death during 2013-2018 by countries
averages <- aggregate(WHO_data[,3:5], list(WHO_data$Country), mean) 
#averages <- format(averages, scientific = FALSE) 

##### renaming variables
colnames(averages)
names(averages)[names(averages)=="Group.1"] <- "Country"
names(averages)[names(averages)=="Number.of.under.five.deaths..thousands."] <- "u5"
names(averages)[names(averages)=="Number.of.infant.deaths..thousands."] <- "infants"
names(averages)[names(averages)=="Number.of.neonatal.deaths..thousands."] <- "neonatal"

#####Dividing countries by deaths # into 6 groups######
str(averages)
#means <- transform(averages, u5 = as.numeric(u5), infants = as.numeric(infants), neonatal = as.numeric(neonatal))
averages <- averages %>% mutate_if(is.numeric, ~round(., 1))
averages$range <- cut2(averages$u5, 6)
#averages$range <- format(averages$range, scientific = FALSE)
averages$category <- as.numeric(cut2(averages$u5, g=6))
table(averages$category)
table(averages$range)
WHO <- averages[order(averages$category), ]

#uplading ISO-3 codes for further merging
iso <- read.dta("ISOcode.dta")
#merging with ISO3 codes
WHO<-merge(WHO, iso, by.x="Country", by.y = "Country", all.x=TRUE)
#some countries were not assigned an ISO-3 code
######## assigning ISO-3 codes#######
WHO$ISO[which(WHO$Country == "Bahamas")] <- "BHS"
WHO$ISO[which(WHO$Country == "Bolivia (Plurinational State of)")] <- "BOL"
WHO$ISO[which(WHO$Country == "Congo")] <- "COG"
WHO$ISO[which(WHO$Country == "Cook Islands")] <- "COK"
WHO$ISO[which(WHO$Country == "Czechia")] <- "CZE"
WHO$ISO[which(WHO$Country == "Democratic People's Republic of Korea")] <- "PRK"
WHO$ISO[which(WHO$Country == "Democratic Republic of the Congo")] <- "COD"
WHO$ISO[which(WHO$Country == "Egypt")] <- "EGY"
WHO$ISO[which(WHO$Country == "Gambia")] <- "GMB"
WHO$ISO[which(WHO$Country == "Iran (Islamic Republic of)")] <- "IRN"
WHO$ISO[which(WHO$Country == "Kyrgyzstan")] <- "KGZ"
WHO$ISO[which(WHO$Country == "Lao People's Democratic Republic")] <- "LAO"
WHO$ISO[which(WHO$Country == "Micronesia (Federated States of)")] <- "FSM"
WHO$ISO[which(WHO$Country == "Niue")] <- "NIU"
WHO$ISO[which(WHO$Country == "Republic of Korea")] <- "KOR"
WHO$ISO[which(WHO$Country == "Republic of Moldova")] <- "MDA"
WHO$ISO[which(WHO$Country == "Republic of North Macedonia")] <- "MKD"
WHO$ISO[which(WHO$Country == "Saint Kitts and Nevis")] <- "KNA"
WHO$ISO[which(WHO$Country == "Saint Lucia")] <- "LCA"
WHO$ISO[which(WHO$Country == "Saint Vincent and the Grenadines")] <- "VCT" 
WHO$ISO[which(WHO$Country == "Sao Tome and Principe")] <- "STP" 
WHO$ISO[which(WHO$Country == "Slovakia")] <- "SVK"
WHO$ISO[which(WHO$Country == "Syrian Arab Republic")] <- "SYR"
WHO$ISO[which(WHO$Country == "United Kingdom of Great Britain and Northern Ireland")] <-"GBR"
WHO$ISO[which(WHO$Country == "United Republic of Tanzania")] <- "TZA"
WHO$ISO[which(WHO$Country == "United States of America")] <- "USA"
WHO$ISO[which(WHO$Country == "Venezuela (Bolivarian Republic of)")] <- "VEN"
WHO$ISO[which(WHO$Country == "Viet Nam")] <- "VNM"
WHO$ISO[which(WHO$Country == "Yemen")] <- "YEM"

##### World map#####
data("World")
WHO <- merge(World, WHO, by.x = "iso_a3", by.y="ISO", all.x=FALSE)
colnames(WHO)
WHO <- subset(WHO, select = c("iso_a3", "Country", "u5", "infants", "neonatal", "category", 
                              "range", "geometry"))

#####Maps 
tmap_options(max.categories = 165)

#Average number of cases of child death in 2013-2018
tmap_mode("plot")
tm_shape(WHO) +
  tm_polygons("category",
              title = "Average number of cases of child (<5 yrs) death in 2013-2018",
              palette = "Accent", labels = c("I(lowest number of cases)", "II", "III", "IV",
                                             "V(highest number of cases)")) +
  tm_layout(bg.color = "skyblue", legend.outside = TRUE) +
  tm_shape(World) +
  tm_borders("black", lwd = .5) +
  tm_text("iso_a3", size = "AREA") 

#Country of interest: Uganda
tm_shape(WHO[WHO$Country == "Uganda",]) +
  tm_polygons("u5", palette = "Accent", 
              title = "Average number of cases of child death, 2013-2018") +
  tm_layout(bg.color = "skyblue", legend.outside = TRUE) +
  tm_bubbles(size = "category", col = "blue") +
  tm_shape(World) +
  tm_borders("black", lwd = .5)+
  tm_text("iso_a3", size = "AREA") 

ttm()
  

