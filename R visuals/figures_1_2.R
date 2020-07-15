rm(list=ls())
library(tidyverse)
library(ggplot2)
library(tmap)
library(foreign)
library(Hmisc)
library(ISOcodes)

options(scipen = 999)
setwd("C:/Users/ZEF/Desktop/madina/programming")

#preparation of WHO data

mortality_rates <- read.csv("probability of dying per 1000 live births.csv")

colnames(mortality_rates)
#renaming columns for ease
names(mortality_rates)[names(mortality_rates)=="Under.5.mortality.rate"] <- "u5"
names(mortality_rates)[names(mortality_rates)=="Infant.mortality.rate"] <- "infants"
names(mortality_rates)[names(mortality_rates)=="Neonatal.mortality.rate"] <- "neonatal"

#####In introduction authors mention 2017 statistics, so I also keep only 2017
mortality_rates <- filter(mortality_rates, Year == 2017)

#####Grouping countries into 6 categories according to their mortality rates of children under-5
mortality_rates$range <- cut2(mortality_rates$u5,7)
mortality_rates$group <- as.numeric(cut2(mortality_rates$u5, g=7))

#uplading ISO-3 codes for further merging
iso_codes <- ISO_3166_1
colnames(iso_codes)
names(iso_codes)[names(iso_codes)  == "Alpha_3"] <- "ISO3_Code"
iso_codes <- subset(iso_codes, select = c("ISO3_Code", "Name"))

#merging 
mortality_rates <- merge(mortality_rates, iso_codes, by.x = "Country", by.y = "Name", all.x = TRUE)
#some countries were not assigned an ISO-3 code
######## assigning ISO-3 codes#######
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Bolivia (Plurinational State of)")] <- "BOL"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "CÃ´te d'Ivoire")] <- "CIV"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Democratic People's Republic of Korea")] <- "PRK"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Democratic Republic of the Congo")] <- "COD"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Iran (Islamic Republic of)")] <- "IRN"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Micronesia (Federated States of)")] <- "FSM"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Republic of Korea")] <- "KOR"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Republic of Moldova")] <- "MDA"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Republic of North Macedonia")] <- "MKD"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "United Kingdom of Great Britain and Northern Ireland")] <-"GBR"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "United Republic of Tanzania")] <- "TZA"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "United States of America")] <- "USA"
mortality_rates$ISO3_Code[which(mortality_rates$Country == "Venezuela (Bolivarian Republic of)")] <- "VEN"
##### World map #####
data("World")
WHO_2017 <- merge(World, mortality_rates, by.x = "iso_a3", by.y="ISO3_Code", all.x=TRUE)
colnames(WHO_2017)
WHO_2017 <- subset(WHO_2017, select = c("iso_a3", "Country", "Year", "u5", "infants", "neonatal", "group", 
                                        "range", "geometry"))

#####Mapping child mortality rates, under-5####
tmap_options(max.categories = 177)
#Under-5 child mortality rate, 2017 
tmap_mode("plot")
tm_shape(WHO_2017) +
  tm_polygons("group",
              title = "Child mortality rate (<5 yrs), WHO 2017",
              palette = "Accent", labels = c("I(lowest mortality rate)", "II", "III", "IV",
                                             "V", "VI((highest highest mortality rate)")) +
  tm_layout(bg.color = "skyblue", legend.outside = TRUE) +
  tm_shape(World) +
  tm_borders("black", lwd = .5) +
  tm_text("iso_a3", size = "AREA")
#exported as "WHO_2017.png"

#Country of interest: Uganda
tm_shape(WHO_2017[WHO_2017$Country == "Uganda",]) +
  tm_polygons("u5", palette = "Accent", 
              title = "Uganda: Child mortality rate(<5 yrs)") +
  tm_layout(bg.color = "skyblue", legend.outside = TRUE) +
  tm_bubbles(size = "group",border.col = "black", labels = c("I", "II", "IV","VI"),
             col = "blueviolet") +
  tm_shape(World) +
  tm_borders("black", lwd = .5)+
  tm_text("iso_a3", size = "AREA") 
#exported as "Uganda.png"
ttm()
