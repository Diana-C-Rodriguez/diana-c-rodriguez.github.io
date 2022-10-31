# Boxplot
  par(mar=c(5, 4, 4, 2))
boxplot(y ~ x, data = HPI, xlab="year", ylab="HPI", ylim=c(0,35))
mtext("Y", side=1, line=2.5, cex=0.8)

library(Hmisc)
library(xlsx)
library(tidyverse)
library(maps)
library(countrycode)
library(ggplot2)
library(dplyr)

# Reading the file, click run!
HPI <- read.xlsx("~/Diana-C-Rodriguez.github.io/HPI.xlsx", sheetIndex = 2)
head(HPI,15)
HPI2021 <- HPI[-c(0:7),]
View(HPI2021)

HPI2021 <- HPI2021[-1,]

HPI2021 <- HPI2021 %>%
  select(-'NA..2')
head(HPI2021)

#Rename columns for simplicity, click run!
HPI2021 <- HPI2021 %>%
  rename(HPI_rank = 'NA.') %>%
  rename(Country = 'X1..Rankings.for.all.countries..2006...2020') %>%
  rename(ISO = 'NA..1') %>%
  rename(Continent = 'NA..3') %>%
  rename(Pop = 'NA..4') %>%
  rename(Life_Exp = 'NA..5') %>%
  rename(Wellbeing = 'NA..6') %>%
  rename(Ecological_Footprint = 'NA..7') %>%
  rename(HPI = 'NA..8') %>%
  rename(Biocapacity = 'NA..9') %>%
  rename(GDP_per_capita = 'NA..10') 
  
#Remove row with index number 8 so the data set starts with 'Costa Rica', click run! 
HPI2021 <- HPI2021[-1,]

#Change to show a maximum of 15 digits, click run! 
options(digits = 15)

#Convert data type from character to numeric for selected columns, click run! 
HPI2021 <- HPI2021 %>%
  mutate(Pop = as.numeric(Pop)) %>%
  mutate(Life_Exp = as.numeric(Life_Exp)) %>%
  mutate(Wellbeing = as.numeric(Wellbeing)) %>%
  mutate(Ecological_Footprint = as.numeric(Ecological_Footprint)) %>%
  mutate(HPI = as.numeric(HPI)) %>%
  mutate(Biocapacity = as.numeric(Biocapacity)) %>%
  mutate(GDP_per_capita = as.numeric(GDP_per_capita)) 

#View cleaned data set, click run! 
str(HPI2021)
head(HPI2021)
view(HPI2021)

#View map of data 'data_map', click run!
require(maps)
require(countrycode)

data_map <- map_data("world")
head(data_map)

#Consulting the countrycode documentation for details, click run! 
?countrycode

data_map$ISO = countrycode(data_map$region, origin="country.name", destination = 'iso3c')

#view updated data_map
head(data_map)

#Merge HPI2019 with data_map to create a data set which will be used to plot HPI in the world map, click run! 
mergedHPI2021 <- full_join(data_map, HPI2021, by="ISO")

#View merged data set, click run! 
head(mergedHPI2021)

#Generate the world map chart, click run! 
ggplot(mergedHPI2021, aes(x = long, y = lat, group = group, fill = HPI)) +
  geom_polygon() +
  scale_fill_viridis_c() +
  labs(title = "The State of Global Happiness in 2019", subtitle = "Based on the Happy Planet Index Score")

par(mfrow=c(1,1))
par(mar=c(5,5,5,5))

boxplot(table(mergedHPI2021$Wellbeing,mergedHPI2021$HPI), 
        beside=TRUE, main= "The Effect of Wellbeing on HPI", xlab= "Wellbeing", ylab= "HPI")
