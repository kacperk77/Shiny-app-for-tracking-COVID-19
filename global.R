library(dplyr)
library(ggplot2)
library(ggthemes)
library(countrycode)
library(plotly)
library(ggmap)
library(shinythemes)
library(shinyWidgets)
library(googleVis)
options(scipen = 100000)


dane <-read.table(file="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",sep=",",dec=".",
                     header=T,stringsAsFactors=F)



dane <- dane %>% rename(Country = countriesAndTerritories, population_2019 = popData2019, 
                        continent = continentExp)

dane <- dane %>% filter(continent != 'Other')

dane <- dane %>% select(-c(geoId, countryterritoryCode))




dane$dateRep <- as.Date(dane$dateRep, format = "%d/%m/%Y")

dane$Country[dane$Country == 'United_States_of_America'] <- 'United States'
dane$Country[dane$Country == 'United_Kingdom'] <- 'United Kingdom'

dane <- dane %>% group_by(Country) %>% arrange(dateRep) %>% mutate(all_cases = cumsum(cases_weekly),
                                                                    all_deaths = cumsum(deaths_weekly)) %>% 
            mutate(all_cases_per_1million = (all_cases/population_2019)*1000000,
                   all_deaths_per_1million = (all_deaths/population_2019)*1000000)

dane$all_cases_per_1million <- round(dane$all_cases_per_1million, 2) 
dane$all_deaths_per_1million<- round(dane$all_deaths_per_1million, 2)                                                                     
  
dane2 <- dane %>% select(dateRep, Country, continent, cases_weekly, all_cases, deaths_weekly, all_deaths, population_2019,
                         all_cases_per_1million,all_deaths_per_1million, notification_rate_per_100000_population_14.days)

population <- Population

dane3 <- population %>% right_join(dane2) %>%
  select(dateRep, Country, Flag, continent, cases_weekly, all_cases, all_cases_per_1million, deaths_weekly, all_deaths, all_deaths_per_1million,
         notification_rate_per_100000_population_14.days) 

dane3 <- dane3 %>% rename(country = Country)

dane3 <- na.omit(dane3)


dane3$xx <- paste(dane3$Flag, dane3$country)

dane4 <- dane3 %>% select(-c(xx, notification_rate_per_100000_population_14.days))


dane5 <- dane3 %>% filter(dateRep == '2020-12-21' & continent %in% 'Europe') %>% 
  arrange(desc(all_cases_per_1million)) %>% head(10) 


