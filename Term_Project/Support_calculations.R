rm(list=ls())
library(tidyverse)
library(WDI)

# Support calculations for the markdown

# Getting data

#  X: GDP per capita
WDIsearch("gdp per capita.*constant") # --> "NY.GDP.PCAP.PP.KD"

#  Y: Life expectancy at birth, total (years)
WDIsearch("life expectancy at birth") # --> "SP.DYN.LE00.IN"

#  Z: Confounders

## Current health expenditure per capita, PPP (current international $)
WDIsearch('Capital health expenditure') # -->	SH.XPD.CHEX.PP.CD

## Urban population (% of total population)
WDIsearch('urba.*population.*total') # -->	SP.URB.TOTL.IN.ZS

## Public spending on education, total (% of GDP)(SE.XPD.TOTL.GD.ZS)
WDIsearch('public.*spending.*education')


data_raw <- WDI(indicator=c('NY.GDP.PCAP.PP.KD',
                            'SP.DYN.LE00.IN',
                            'SH.XPD.CHEX.PP.CD',
                            'SP.URB.TOTL.IN.ZS',
                            'SE.XPD.TOTL.GD.ZS'), 
                country="all", start=1990, end=2018, extra = TRUE)

colnames(data_raw) <- c("CC2","country","year","GDPPC","Life_exp","Health_exp","Urban_pop","Edu_exp", "CC")

data_raw_2 <- data_raw %>% select(c("CC","country","year","GDPPC","Life_exp","Health_exp","Urban_pop","Edu_exp"))

my_path <- "./Term_Project/"
write.csv(data_raw_2, paste0(my_path,'raw/data_raw_2.csv'), row.names = FALSE)

