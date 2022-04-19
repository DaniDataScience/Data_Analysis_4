# setwd("../../CEU/Study/da4/final_proj/Smoke_Cancer/")
library(tidyverse)
library(data.table)
who_2009 <- read_csv("WHO_global_tobacco_2009.csv")
who_2011 <- read_csv("WHO_global_tobacco_2011.csv")
who_2013 <- read_csv("WHO_global_tobacco_2013.csv")
who_2015 <- read_csv("WHO_global_tobacco_2015.csv")
who_2017 <- read_csv("WHO_global_tobacco_2017.csv")
who_2019 <- read_csv("WHO_global_tobacco_2019.csv")
who_2021 <- read_csv("WHO_global_tobacco_2021.csv")
cancer_cases_fem <- read_csv("https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/countries-etc-datapoints/ddf--datapoints--lung_cancer_number_of_new_female_cases--by--geo--time.csv")
cancer_cases_mal <- read_csv("https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/countries-etc-datapoints/ddf--datapoints--lung_cancer_number_of_new_male_cases--by--geo--time.csv")
cancer_full_c_names <- read_csv("https://raw.githubusercontent.com/open-numbers/ddf--gapminder--systema_globalis/master/ddf--entities--geo--country.csv")
wb_pop <- read_csv("world_bank_pop.csv")
cancer_2017_19 <- read_csv("195_cont_2017_19_cancer.csv")
cancer_2020 <- read_csv("185_cont_2020.csv")

# Compare country names

country_names_miss_2009 <- who_2009$COUNTRY[!(who_2009$COUNTRY %in% who_2021$COUNTRY)]
country_names_miss_2011 <- who_2011$COUNTRY[!(who_2011$COUNTRY %in% who_2021$COUNTRY)]
country_names_miss_2013 <- who_2013$COUNTRY[!(who_2013$COUNTRY %in% who_2021$COUNTRY)]
country_names_miss_2015 <- who_2015$COUNTRY[!(who_2015$COUNTRY %in% who_2021$COUNTRY)]
country_names_miss_2017 <- who_2017$COUNTRY[!(who_2017$COUNTRY %in% who_2021$COUNTRY)]
country_names_miss_2019 <- who_2019$COUNTRY[!(who_2019$COUNTRY %in% who_2021$COUNTRY)]
country_names_miss_2021_09 <- who_2021$COUNTRY[!(who_2021$COUNTRY %in% who_2009$COUNTRY)]
country_names_miss_2021_11 <- who_2021$COUNTRY[!(who_2021$COUNTRY %in% who_2011$COUNTRY)]
country_names_miss_2021_13 <- who_2021$COUNTRY[!(who_2021$COUNTRY %in% who_2013$COUNTRY)]
country_names_miss_2021_15 <- who_2021$COUNTRY[!(who_2021$COUNTRY %in% who_2015$COUNTRY)]
country_names_miss_2021_17 <- who_2021$COUNTRY[!(who_2021$COUNTRY %in% who_2017$COUNTRY)]
country_names_miss_2021_19 <- who_2021$COUNTRY[!(who_2021$COUNTRY %in% who_2019$COUNTRY)]

# Individually check which country names have to be updated and create lookup table

# 1. Identify and add countries which didn't exists before

country_names_miss_2009[24] <- "South Sudan"

# 2. Create comparison table to know how you need to reorder NEW COUNTRY NAMES (important:
# we don't want to overwrite order of base year's table, we want to update country names)

replace_tibble_09 <- tibble( old_country_names = country_names_miss_2009 ,
                             new_country_names = country_names_miss_2021_09)

# 3. Now create reference table with correct order.

new_names_09_21 <- c(country_names_miss_2021_09[c(1:13)] , country_names_miss_2021_09[c(15:18)] ,
                     country_names_miss_2021_09[21] , country_names_miss_2021_09[14] , country_names_miss_2021_09[19] ,
                     country_names_miss_2021_09[c(23:24)] , country_names_miss_2021_09[20] , country_names_miss_2021_09[22])

# 4. Based on it create lookup table for update.

replace_tibble_09 <- tibble( old_country_names = country_names_miss_2009 ,
                             new_country_names = new_names_09_21)

# 2011 

country_names_miss_2011[29] <- "South Sudan"

replace_tibble_11 <- tibble( old_country_names = country_names_miss_2011 ,
                             new_country_names = country_names_miss_2021_11)

new_names_11_21 <- c(country_names_miss_2021_11[c(1:8)] , country_names_miss_2021_11[c(10:13)] ,
                     country_names_miss_2021_11[c(16:21)] , country_names_miss_2021_11[9] , 
                     country_names_miss_2021_11[23] , country_names_miss_2021_11[14] , 
                     country_names_miss_2021_11[c(24:29)] , 
                     country_names_miss_2021_11[15] , country_names_miss_2021_11[22])

replace_tibble_11 <- tibble( old_country_names = country_names_miss_2011 ,
                             new_country_names = new_names_11_21)

# 2013, it gets easier from here

replace_tibble_13 <- tibble( old_country_names = country_names_miss_2013 ,
                             new_country_names = country_names_miss_2021_13)

# 2015 

replace_tibble_15 <- tibble( old_country_names = country_names_miss_2015 ,
                             new_country_names = country_names_miss_2021_15)

# 2017

replace_tibble_17 <- tibble( old_country_names = country_names_miss_2017 ,
                             new_country_names = country_names_miss_2021_17)

# 2019

replace_tibble_19 <- tibble( old_country_names = country_names_miss_2019 ,
                             new_country_names = country_names_miss_2021_19)

# Use a function to make corrections for each year

harmonize_country_names <- function(table, diff_countries_table, replace_tibble) {
  updated_list <- c()
  for (country in table$COUNTRY) {
    if (country %in% diff_countries_table) {
      new_name <- data.table(replace_tibble)[match(country, old_country_names),2]
      names(new_name) <-  NULL
      updated_list <- c(updated_list,new_name)
    }
    else{
      updated_list <- c(updated_list,country)
    }
  }
  updated_list <- unlist(updated_list) 
  table$COUNTRY  <- updated_list
  return(table)
}

# Use function

who_2009 <- harmonize_country_names(who_2009,country_names_miss_2009, replace_tibble_09)
who_2011 <- harmonize_country_names(who_2011,country_names_miss_2011, replace_tibble_11)
who_2013 <- harmonize_country_names(who_2013,country_names_miss_2013, replace_tibble_13)
who_2015 <- harmonize_country_names(who_2015,country_names_miss_2015, replace_tibble_15)
who_2017 <- harmonize_country_names(who_2017,country_names_miss_2017, replace_tibble_17)
who_2019 <- harmonize_country_names(who_2019,country_names_miss_2019, replace_tibble_19)

# Add missing countries (Only 1 South Sudan in 2009-11)

who_2009 <- rbind(who_2009 , c("South Sudan",rep("Missing" , ncol(who_2009) - 1))) 
who_2011 <- rbind(who_2011 , c("South Sudan",rep("Missing" , ncol(who_2011) - 1))) 

# Rearrange with updated country names

who_2009 <- who_2009 |> arrange(COUNTRY)  
who_2011 <- who_2011 |> arrange(COUNTRY)  
who_2013 <- who_2013 |> arrange(COUNTRY)  
who_2015 <- who_2015 |> arrange(COUNTRY)  
who_2017 <- who_2017 |> arrange(COUNTRY)  
who_2019 <- who_2019 |> arrange(COUNTRY)  

# Create intervention columns

cols_to_test = c(2, 3, 6, 7, 9)

int_cols <- function(table , year_of_table) {
  
  table$all_rel_same <- apply(table[cols_to_test], 1, function(x) length(unique(x)) == 1)
  
  col1 <- paste0('interv_' , year_of_table )
  col2 <- paste0('num_places_' , year_of_table )
  
  table[[col1]] <- with(table , ifelse((all_rel_same == TRUE) & (RESTAURANTS == "Yes") , 1 , 0)) 
  table[[col2]] <- with(table , rowSums(table== "Yes")) 
  
  return(table)
}

who_2009 <- int_cols(who_2009, 2009)
who_2011 <- int_cols(who_2011, 2011)
who_2013 <- int_cols(who_2013, 2013)
who_2015 <- int_cols(who_2015, 2015)
who_2017 <- int_cols(who_2017, 2017)
who_2019 <- int_cols(who_2019, 2019)
who_2021 <- int_cols(who_2021, 2021)

# Merging

# First we just compared 2013 and 2019 to know which data we have to manually update in 17-19

DF_2013_19 <- merge(who_2019, who_2013 , by.x = 'COUNTRY', by.y = 'COUNTRY')
DF_2013_19 <- DF_2013_19 |>  select(COUNTRY , interv_2013 , num_places_2013 , interv_2019 ,num_places_2019)
DF_2013_19 <-  DF_2013_19 |>  
  mutate(place_changes = num_places_2019 - num_places_2013,
         interv_change = ifelse(interv_2019 == 1 & interv_2013 == 0 , 1 , 0 ))


less_places <- data.table(DF_2013_19)[place_changes < 0 ]$COUNTRY 

# 13 countries where number of places decreased between 2013 and 2019

interv_change <- data.table(DF_2013_19)[interv_change == 1 ]$COUNTRY
# 24 countries reached intervention between 2013 and 2019
countries_to_manually_check <- c(less_places, interv_change)
# Total 36 countries manually checked and updated in 20175-17 csv files.
countries_to_manually_check <- sort(countries_to_manually_check)

# Getting t0 year for countries

t0_table <- merge(merge(merge(merge(merge(merge(who_2009 |> select(COUNTRY , interv_2009) ,
                                                who_2011 |> select(COUNTRY , interv_2011)),
                                          who_2013 |> select(COUNTRY , interv_2013)),
                                    who_2015 |> select(COUNTRY , interv_2015)),
                              who_2017 |> select(COUNTRY , interv_2017)),
                        who_2019 |> select(COUNTRY , interv_2019)),
                  who_2021 |> select(COUNTRY , interv_2021))

# Get first col where intervention is 1

t0_table$t0 <- names(t0_table[2:8])[max.col(t0_table[2:8] != 0, ties.method = "first")]

# This method prints 2009 if all years are 0, so replace those

t0_table <- t0_table |> 
  mutate(t0 = ifelse(t0 == "interv_2009" & interv_2009 == 0 ,
                     0, t0))
# Validation
data.table(t0_table)[t0 != 0 ]$COUNTRY 
data.table(t0_table)[t0 != 0 & interv_2021 == 0]

# Bolivia change really happens in 2021
t0_table <- data.table(t0_table)
t0_table[COUNTRY == 'Bolivia (Plurinational State of)']$t0 <- "interv_2021"

# Save countries with any intervention

any_int <- t0_table[t0 != 0]$COUNTRY

# Mongolia , Djibouti Mauritius  , Zambia  avg. number of institution above 7, keep
# Guinea, Maldives not, remove

t0_table[COUNTRY == 'Guinea']$t0 <- 0
t0_table[COUNTRY == 'Maldives']$t0 <- 0

# 32 countries btween 10 and 14, 40 until 16
int_between_10_14 <- t0_table[t0 %in% c('interv_2011' , 'interv_2013', 'interv_2015')]$COUNTRY
int_between_10_16 <- t0_table[t0 %in% c('interv_2011' , 'interv_2013', 'interv_2015', 'interv_2017')]$COUNTRY

# Hungary 
# . smoking areas can be designated on privately owned transport facilities (e.g. taxis, rented buses)
# https://www.euro.who.int/__data/assets/pdf_file/0020/263333/Tobacco-control-in-practice-Article-8-Protection-from-exposure-to-tobacco-smoke-the-story-of-Hungary.pdf

# Final intervention tables

poss_controls <- t0_table$COUNTRY[!(t0_table$COUNTRY %in% any_int)]

# Include only possible controls (no intervention) and relevant t0 years (between 2010 and 16)

countries_10_14 <- c(int_between_10_14 , poss_controls)
countries_10_16 <- c(int_between_10_16 , poss_controls)

t0_table_10_14 <- t0_table |> filter(COUNTRY %in% countries_10_14) |>  
  mutate(t0_year = as.numeric(str_sub(t0,-4,-1))) |> 
  select(COUNTRY , t0_year)

t0_table_10_16 <- t0_table |> filter(COUNTRY %in% countries_10_16) |>  
  mutate(t0_year = as.numeric(str_sub(t0,-4,-1))) |> 
  select(COUNTRY , t0_year)

# Joining to cancer dataset

# Getting full country names

merged_cancer <- merge(cancer_cases_fem , cancer_cases_mal ,  by = c("geo" , "time")) 
merged_cancer <- merge(merged_cancer , cancer_full_c_names |> select(country , name) ,
                       by.x = "geo", by.y = "country") 


# Updating cancer data country names

t0_miss <- t0_table$COUNTRY[!(t0_table$COUNTRY %in% unique(merged_cancer$name))]
cancer_miss <- unique(merged_cancer$name)[!(unique(merged_cancer$name) %in% unique(t0_table$COUNTRY))]

t0_miss[34] <- "American Samoa"

replace_tibble_cancer <- tibble( old_country_names = cancer_miss ,
                                 new_country_names = t0_miss)

new_names_cancer <- c("Missing","Missing",
                      t0_miss[c(1:2)] , t0_miss[6] , t0_miss[9],"Missing",t0_miss[3],t0_miss[7],
                      t0_miss[13] , t0_miss[29],"Missing","Missing",t0_miss[c(10:11)],
                      t0_miss[19] , t0_miss[12], t0_miss[23],
                      t0_miss[20] ,"Missing","Missing",t0_miss[8],t0_miss[17],
                      "Missing","Missing",t0_miss[21],t0_miss[c(26:27)],"Missing",
                      t0_miss[c(30:31)],t0_miss[24],t0_miss[c(32:33)])

replace_tibble_cancer <- tibble( old_country_names = cancer_miss ,
                                 new_country_names = new_names_cancer)


harmonize_country_names_can <- function(table, diff_countries_table, replace_tibble) {
  updated_list <- c()
  for (country in table$name) {
    if (country %in% diff_countries_table) {
      new_name <- data.table(replace_tibble)[match(country, old_country_names),2]
      names(new_name) <-  NULL
      updated_list <- c(updated_list,new_name)
    }
    else{
      updated_list <- c(updated_list,country)
    }
  }
  updated_list <- unlist(updated_list) 
  table$name  <- updated_list
  return(table)
}

merged_cancer <- harmonize_country_names_can(merged_cancer,cancer_miss, replace_tibble_cancer)
merged_cancer <- merged_cancer |> arrange(name) 

# Get total cancer cases by country by year

merged_cancer <- merged_cancer |> 
  mutate(total_cases = lung_cancer_number_of_new_female_cases + lung_cancer_number_of_new_male_cases)

# Remove last 5 rows that don't contain data

wb_pop <- head(wb_pop,-5)

t0_miss_wb <- t0_table$COUNTRY[!(t0_table$COUNTRY %in% unique(wb_pop$`Country Name`))]
pop_miss <- unique(wb_pop$`Country Name`)[!(unique(wb_pop$`Country Name`) %in% unique(t0_table$COUNTRY))]

t0_miss_wb[29:50] <- "Missing"

replace_tibble_pop <- tibble( old_country_names = pop_miss ,
                              new_country_names = t0_miss_wb)

new_names_pop <-    c(rep("Missing",2), t0_miss_wb[1], "Missing", t0_miss_wb[2],
                      rep("Missing",3), t0_miss_wb[8], "Missing", t0_miss_wb[5],
                      "Missing", t0_miss_wb[6], t0_miss_wb[9],
                      rep("Missing",2), t0_miss_wb[10],
                      rep("Missing",4), t0_miss_wb[11],"Missing", t0_miss_wb[7],
                      t0_miss_wb[17],"Missing",t0_miss_wb[c(12:13)],rep("Missing",3),
                      t0_miss_wb[18],rep("Missing",4), t0_miss_wb[22],t0_miss_wb[c(19:20)],
                      "Missing", t0_miss_wb[21], t0_miss_wb[24],
                      "Missing", t0_miss_wb[23], t0_miss_wb[c(25:27)],
                      rep("Missing",2), t0_miss_wb[28])

replace_tibble_pop <- tibble( old_country_names = pop_miss ,
                              new_country_names = new_names_pop)
names(wb_pop)[names(wb_pop) == "Country Name" ] <- 'name'
names(wb_pop)[names(wb_pop) == "Population, total [SP.POP.TOTL]" ] <- 'pop'
names(wb_pop)[names(wb_pop) == "Time" ] <- 'time'

wb_pop <- harmonize_country_names_can(wb_pop,pop_miss, replace_tibble_pop)
wb_pop <- wb_pop |> arrange(name) 


# Get total cancer by country by year

merged_cancer <- left_join(merged_cancer |> select(name, time , total_cases) , 
                           wb_pop |> select(name, time , pop) , by = c ("name" , "time"))

merged_cancer <- data.table(merged_cancer)[name != 'Missing']
merged_cancer <- merged_cancer |> mutate(pop = ifelse(pop == '..' , NA , as.numeric(pop)))
merged_cancer <- merged_cancer |> mutate(case_per_pop = total_cases / pop)
merged_cancer <- merged_cancer |> mutate(case_per_t = case_per_pop * 100000)

# Drop missing pop
missing_pop <- unique(merged_cancer |> select(name, pop) |> filter(is.na(pop)) |>  select(name))
merged_cancer <- merged_cancer |> filter(!(name %in% missing_pop$name)) |> select(name, time, case_per_t)

# Get cancer data 2017-20, join to table
# Drop countries with missing cancer data? Or get from other source?

missing_cont <- unique(t0_table_10_14$COUNTRY)[!(unique(t0_table_10_14$COUNTRY) %in% unique(merged_cancer$name))]

missing_cont_aux <- read_csv("missing_cont_old.csv")

merged_cancer <- rbind(merged_cancer , missing_cont_aux)

# Get years 2017 to 2020

missing_17 <- unique(merged_cancer$name)[!(unique(merged_cancer$name) %in% unique(cancer_2017_19$location))]
cancer_2017_19 <- cancer_2017_19 |> 
  mutate(location = ifelse(location == "United Kingdom" , 
                           "United Kingdom of Great Britain and Northern Ireland" ,location))
drop_17 <- unique(cancer_2017_19$location)[!(unique(cancer_2017_19$location) %in% unique(merged_cancer$name))]
cancer_2017_19 <- cancer_2017_19 |> 
  filter(!(location %in% drop_17))
names(cancer_2017_19) <- names(merged_cancer)

merged_cancer <- rbind(merged_cancer , cancer_2017_19)

# No data for Russia
merged_cancer <- merged_cancer |>  filter(name != "Russian Federation")

# 2020 cancer data

cancer_2020$time <- rep(2020, nrow(cancer_2020))
cancer_2020 <- cancer_2020[, c("Population", "time", "Crude Rate")]
names(cancer_2020) <- names(merged_cancer)
missing_20 <- unique(merged_cancer$name)[!(unique(merged_cancer$name) %in% unique(cancer_2020$name))]
drop_20 <- unique(cancer_2020$name)[!(unique(cancer_2020$name) %in% unique(merged_cancer$name))]

# Remaming

drop_20[23:29] <- "Missing"

replace_tibble_2020 <- tibble( old_country_names = drop_20 ,
                               new_country_names = missing_20)

new_names_2020 <-    c(rep("Missing",2), drop_20[17], drop_20[20],
                       drop_20[5],drop_20[21],rep("Missing",3),drop_20[15], 
                       rep("Missing",2), drop_20[1],drop_20[4],rep("Missing",3),
                       drop_20[2],drop_20[19],drop_20[13],rep("Missing",6),
                       drop_20[22],rep("Missing",2))

replace_tibble_2020 <- tibble( old_country_names = new_names_2020 ,
                               new_country_names = missing_20)


no_2020_data <- data.table(replace_tibble_2020)[old_country_names == "Missing", new_country_names]

harmonize_country_2020 <- function(table, diff_countries_table, replace_tibble) {
  updated_list <- c()
  for (country in table$name) {
    if (country %in% diff_countries_table) {
      new_name <- data.table(replace_tibble)[match(country, old_country_names),2]
      names(new_name) <-  NULL
      updated_list <- c(updated_list,new_name)
    }
    else{
      updated_list <- c(updated_list,country)
    }
  }
  updated_list <- unlist(updated_list) 
  table$name  <- updated_list
  return(table)
}


cancer_2020 <- harmonize_country_2020(cancer_2020,drop_20, replace_tibble_2020)
cancer_2020 <- cancer_2020 |> filter(!is.na(name))
merged_cancer <- rbind(merged_cancer , cancer_2020)
merged_cancer <- merged_cancer |> filter(!(name %in% no_2020_data))
merged_cancer <- merged_cancer |> mutate(time = as.numeric(time)) |> arrange(name , time)

# 174 country has cancer data for 31 years

# Intervention took place 1 year before report

t0_table_10_14 <- t0_table_10_14 |> 
  mutate(t0_year = ifelse(t0_year != 0 , t0_year - 1, 0))

t0_table_10_16 <- t0_table_10_16 |> 
  mutate(t0_year = ifelse(t0_year != 0 , t0_year - 1, 0))

merged_cancer_2014 <- merge(merged_cancer , t0_table_10_14 , by.x = "name" , by.y = "COUNTRY") |> 
  mutate(year_num = ifelse(t0_year != 0 , time - t0_year, time - 2012))
# 138 countries for 2010-14 intervention
treated_14 <- unique(merged_cancer_2014 |> filter(t0_year !=0) |> select(name)) 
control_14 <- unique(merged_cancer_2014 |> filter(t0_year ==0) |> select(name)) 
# treated 29, control 109

merged_cancer_2016 <- merge(merged_cancer , t0_table_10_16 , by.x = "name" , by.y = "COUNTRY") |> 
  mutate(year_num = ifelse(t0_year != 0 , time - t0_year, time - 2014))
# 146 countries for 2010-14 intervention

treated_16 <- unique(merged_cancer_2016 |> filter(t0_year !=0) |> select(name))
control_16 <- unique(merged_cancer_2016 |> filter(t0_year ==0) |> select(name))
# treated 37, control 109

# Final tables binary

# Wide format for cross-sectional OLS

# Long format for other regressions
# For long diff model: for 14 change untreated t0 to 2012, for 16 2014
# Causal var

merged_cancer_2014 <- merged_cancer_2014 |> mutate(treated = ifelse(t0_year !=0 , 1 ,0 ))
merged_cancer_2016 <- merged_cancer_2016 |> mutate(treated = ifelse(t0_year !=0 , 1 ,0 ))

# For long diff-in-diff

merged_cancer_2014_long_diff <- merged_cancer_2014 |> filter(year_num == -6 |year_num == 6 )
merged_cancer_2016_long_diff <- merged_cancer_2016 |> filter(year_num == -6 |year_num == 4 )

merged_cancer_2014_long_diff <- merged_cancer_2014_long_diff |> 
  mutate(d_case_per_t = ifelse(year_num == 6, case_per_t - lag(case_per_t,1) , NA))
merged_cancer_2016_long_diff <- merged_cancer_2016_long_diff |> 
  mutate(d_case_per_t = ifelse(year_num == 4, case_per_t - lag(case_per_t,1) , NA))

write_csv(merged_cancer_2014_long_diff ,'bin_diff_in_diff_to14.csv')
write_csv(merged_cancer_2016_long_diff ,'bin_diff_in_diff_to16.csv')

# For long FD, FE
# Get lags

merged_cancer_2014_long <- merged_cancer_2014 |> 
  group_by(name) |> 
  mutate(
    d_case_per_t  = case_per_t - lag(case_per_t ),
    d_case_per_t2  = lag(case_per_t ) - lag(case_per_t , 2),
    d_case_per_t3  = lag(case_per_t , 2) - lag(case_per_t , 3),
    d_case_per_t4  = lag(case_per_t , 3) - lag(case_per_t , 4),
    d_case_per_t5  = lag(case_per_t , 4) - lag(case_per_t , 5),
    d_case_per_t6  = lag(case_per_t , 5) - lag(case_per_t , 6),
    l_case_per_t6  = lead(case_per_t , 6) - lead(case_per_t , 5)
  ) |> 
  ungroup()

merged_cancer_2016_long <- merged_cancer_2016 |> 
  group_by(name) |> 
  mutate(
    d_case_per_t  = case_per_t - lag(case_per_t ),
    d_case_per_t2  = lag(case_per_t ) - lag(case_per_t , 2),
    d_case_per_t3  = lag(case_per_t , 2) - lag(case_per_t , 3),
    d_case_per_t4  = lag(case_per_t , 3) - lag(case_per_t , 4),
    d_case_per_t5  = lag(case_per_t , 4) - lag(case_per_t , 5),
    d_case_per_t6  = lag(case_per_t , 5) - lag(case_per_t , 6),
    l_case_per_t4  = lead(case_per_t , 4) - lead(case_per_t , 3)
  ) |> 
  ungroup()


# Get avg. 6 years prior diff to 6 years in future

merged_cancer_2014_long <- merged_cancer_2014_long |> 
  mutate(
    d_over_6_years  = ((d_case_per_t + d_case_per_t2  + d_case_per_t3 + 
                          d_case_per_t4 + d_case_per_t5 + d_case_per_t6) / 6) - l_case_per_t6
  ) |> filter(year_num == 0) |>  select(name, time , treated , d_over_6_years)

# to 2016

merged_cancer_2016_long <- merged_cancer_2016_long |> 
  mutate(
    d_over_4_years  = ((d_case_per_t + d_case_per_t2  + d_case_per_t3 + 
                          d_case_per_t4 + d_case_per_t5 + d_case_per_t6) / 6) - l_case_per_t4
  ) |> filter(year_num == 0) |>  select(name, time , treated , d_over_4_years)


write_csv(merged_cancer_2014_long ,'bin_long_fd_to14.csv')
write_csv(merged_cancer_2016_long ,'bin_long_fd_to16.csv')

# Wide format for year-by-year regression
merged_cancer_2014_wide <- merged_cancer_2014 |> filter(year_num >= - 20 & year_num <= 6) |> select(-"time")
merged_cancer_2014_wide <- reshape(merged_cancer_2014_wide, idvar = c("name","t0_year","treated" ), 
                                   timevar = "year_num", direction = "wide")

merged_cancer_2016_wide <- merged_cancer_2016 |> filter(year_num >= - 20 & year_num <= 4) |> select(-"time")
merged_cancer_2016_wide <- reshape(merged_cancer_2016_wide, idvar = c("name","t0_year","treated" ), 
                                   timevar = "year_num", direction = "wide")

# Clean column names

names(merged_cancer_2014_wide) <- c(names(merged_cancer_2014_wide)[1:3] ,
                             gsub("-" , "pre" , names(merged_cancer_2014_wide)[4:length(merged_cancer_2014_wide)]))
names(merged_cancer_2016_wide) <- c(names(merged_cancer_2016_wide)[1:3] ,
                             gsub("-" , "pre" , names(merged_cancer_2016_wide)[4:length(merged_cancer_2016_wide)]))

write_csv(merged_cancer_2014_wide ,'bin_wide_to14.csv')
write_csv(merged_cancer_2016_wide ,'bin_wide_to16.csv')

# Numeric

numeric_int_table <- merge(merge(merge(merge(merge(merge( who_2009 |> select(COUNTRY , num_places_2009) ,
                                                          who_2011 |> select(COUNTRY , num_places_2011)),
                                                          who_2013 |> select(COUNTRY , num_places_2013)),
                                                          who_2015 |> select(COUNTRY , num_places_2015)),
                                                          who_2017 |> select(COUNTRY , num_places_2017)),
                                                          who_2019 |> select(COUNTRY , num_places_2019)),
                                                          who_2021 |> select(COUNTRY , num_places_2021))

# Wide for reg

names(numeric_int_table) <- c(names(numeric_int_table)[1] , 
                              as.numeric(str_sub(names(numeric_int_table)[2:8],-4,-1)) - 1)


drop_count <- unique(numeric_int_table$COUNTRY)[!(unique(numeric_int_table$COUNTRY) %in%  unique(merged_cancer$name))]

numeric_int_table <- numeric_int_table |> filter(!(COUNTRY %in% drop_count))

numeric_int_table_for_long <- copy(numeric_int_table)

join_by_year <- function(year) {
  table <- left_join(numeric_int_table , merged_cancer |> filter(time == year) |> select(name, case_per_t),
                     by = c("COUNTRY" = "name"))
  names(table)[length(table)] <- paste0("case_per_t_" , year)
  return(table)
}

numeric_int_table <- join_by_year(2008)
numeric_int_table <- join_by_year(2010)
numeric_int_table <- join_by_year(2012)
numeric_int_table <- join_by_year(2014)
numeric_int_table <- join_by_year(2016)
numeric_int_table <- join_by_year(2018)
numeric_int_table <- join_by_year(2020)

write_csv(numeric_int_table ,'quant_wide.csv')

# Long for FE, FD

# Copy next year
numeric_int_table_for_long[["2009"]] <- numeric_int_table_for_long[["2010"]]
numeric_int_table_for_long[["2011"]] <- numeric_int_table_for_long[["2012"]]
numeric_int_table_for_long[["2013"]] <- numeric_int_table_for_long[["2014"]]
numeric_int_table_for_long[["2015"]] <- numeric_int_table_for_long[["2016"]]
numeric_int_table_for_long[["2017"]] <- numeric_int_table_for_long[["2018"]]
numeric_int_table_for_long[["2019"]] <- numeric_int_table_for_long[["2020"]]

# Add empty cols for later getting lags

numeric_int_table_for_long[["2002"]] <- NA
numeric_int_table_for_long[["2003"]] <- NA
numeric_int_table_for_long[["2004"]] <- NA
numeric_int_table_for_long[["2005"]] <- NA
numeric_int_table_for_long[["2006"]] <- NA
numeric_int_table_for_long[["2007"]] <- NA

numeric_int_table_for_long <- numeric_int_table_for_long[, c("COUNTRY", seq(2002,2020))] 

join_by_year_long <- function(year) {
  table <- left_join(numeric_int_table_for_long , merged_cancer |> filter(time == year) |> select(name, case_per_t),
                     by = c("COUNTRY" = "name"))
  names(table)[length(table)] <- paste0("case_per_t_" , year)
  return(table)
}

for (year in 2002:2020) {
  numeric_int_table_for_long <- join_by_year_long(year)
}


numeric_long_1 <- reshape(numeric_int_table_for_long, 
        direction = "long",
        varying = list(names(numeric_int_table_for_long)[2:20]),
        v.names = "policy_num",
        idvar = c("COUNTRY"),
        timevar = "Year",
        times = seq(2002,2020))
        

numeric_long_2 <- reshape(numeric_int_table_for_long, 
                   direction = "long",
                   varying = list(names(numeric_int_table_for_long)[21:39]),
                   v.names = "case_per_t",
                   idvar = c("COUNTRY"),
                   timevar = "Year",
                   times = seq(2002,2020))

numeric_long <- merge(numeric_long_1 |> select(COUNTRY, Year, policy_num) ,
                      numeric_long_2 |> select(COUNTRY, Year, case_per_t),
      by.x= c("COUNTRY" , "Year"), by.y= c("COUNTRY" , "Year"))

numeric_long <- numeric_long |> arrange(COUNTRY,Year)

numeric_long <- numeric_long |> 
  group_by(COUNTRY) |> 
  mutate(
    d_case_per_t  = case_per_t - lag(case_per_t ),
    d_case_per_t2  = lag(case_per_t ) - lag(case_per_t , 2),
    d_case_per_t3  = lag(case_per_t , 2) - lag(case_per_t , 3),
    d_case_per_t4  = lag(case_per_t , 3) - lag(case_per_t , 4),
    d_case_per_t5  = lag(case_per_t , 4) - lag(case_per_t , 5),
    d_case_per_t6  = lag(case_per_t , 5) - lag(case_per_t , 6),
    l_case_per_t1 = lead(case_per_t, 1) - case_per_t,
    l_case_per_t2 = lead(case_per_t, 2) - case_per_t,
    l_case_per_t3 = lead(case_per_t, 3) - case_per_t,
    l_case_per_t4 = lead(case_per_t, 4) - case_per_t
  ) |> 
  ungroup()

numeric_long <- numeric_long |> filter(!is.na(policy_num))

write_csv(numeric_long ,'quant_long.csv')
