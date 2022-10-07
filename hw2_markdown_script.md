Data Science HW2
================
Sabah Usmani
2022-10-07

``` r
library(tidyverse)
library(readxl)
```

### Problem 2

Load Mr Trash Dataset and Clean Data

``` r

mr_trash_data <- read_excel("data/Trash_Wheel_Collection_Data_updated.xlsx",sheet=1, skip = 1, range = cell_cols("A:N")) %>% 
  janitor::clean_names() %>%
  drop_na(dumpster) %>% #omit columns with no dumpster specific data
  mutate(year = as.numeric(year)) %>%
  mutate(sports_balls = as.integer(sports_balls)) %>% 
  mutate(wheel_type = "mister" ) %>%
  select(dumpster, wheel_type, month:chip_bags,homes_powered, sports_balls)
```

Load Professor Trash Data and Clean Data

``` r
prof_trash_data <- read_excel("data/Trash_Wheel_Collection_Data_updated.xlsx",sheet=2, skip = 1, range = cell_cols("A:M")) %>%
  janitor::clean_names() %>%
  drop_na(dumpster) %>% 
  mutate(wheel_type = "prof") %>%
  select(dumpster, wheel_type, month:homes_powered) %>%
  mutate(sports_balls = 0)
  
 
```

Combining the two datasets (mister + prof)

``` r

trash_data <- bind_rows(mr_trash_data, prof_trash_data)

obs <- trash_data %>%
  nrow()

tot_weight <- trash_data %>%
  pull(weight_tons) %>%
  sum()

prof_tot_weight <- prof_trash_data %>%
  pull(weight_tons) %>%
    sum()

mr_sports_2020 <- mr_trash_data %>%
  filter(year == 2020) %>%
  pull(sports_balls) %>%
  sum()
  
```

There are 641 observations in the combined dataset, which includes both
mr and professor trash wheel. There is no information about sports balls
for Professor trash wheel. Key variables include total weight of trash
collected in tonnes (weight_tons), volume of trash collected (cubic
yards), and the number of plastic bottles collected (plastic_bottles).
The total weight of trash collected by both wheels is 1938.48

The total weight of trash collected by professor trash wheel was 190.12.
856 sports balls were collected by Mr Trash wheel in 2020.

### Problem 3

Load all three datasets

``` r

pols_data <- read_csv("data/fivethirtyeight_datasets/pols-month.csv")

unemployment_data <- read_csv("data/fivethirtyeight_datasets/unemployment.csv")

snp_data <- read_csv("data/fivethirtyeight_datasets/snp.csv")
```

Cleaning the data in pols-month

``` r
pols_data_clean <- pols_data %>%
 separate(mon, into = c("year", "month", "day")) %>%
  mutate(year = as.numeric(year)) %>%
 mutate(month = recode(month, `01` = "january", `02` = "february", `03` = "march", `04` = "april", `05` = "may", `06` = "june", `07` = "july", `08` = "august", `09` = "september", `10` = "october", `11` = "november", `12` = "december")) %>%
  mutate(president = if_else(prez_gop == 1 , "gop", "dem")) %>%
  select(-prez_dem, -prez_gop, -day) %>%
  relocate(year, month, president) 
```

Tidying the data in snp

``` r
snp_data_clean <- snp_data %>%
 separate(date, into = c("month", "day", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = recode(month, `1` = "january", `2` = "february", `3` = "march", `4` = "april", `5` = "may", `6` = "june", `7` = "july", `8` = "august", `9` = "september", `10` = "october", `11` = "november", `12` = "december")) %>%
  select(-day) %>% 
  relocate(year, month) %>%
  mutate(year = if_else(year <= 22, year+2000, year+1900))
  
```

Tidying the unemployment data

``` r
unemploy_clean <- unemployment_data %>%
  janitor::clean_names() %>%
    pivot_longer(jan:dec,
               names_to = "month",
               #names_prefix = "bdi_score_",
               values_to = "perc_unemployed", 
               ) %>%
  mutate(month = recode(month, `jan` = "january", `feb` = "february", `mar` = "march", `apr` = "april", `may` = "may", `jun` = "june", `jul` = "july", `aug` = "august", `sep` = "september", `oct` = "october", `nov` = "november", `dec` = "december"))
```

Merging snp into pols

``` r

snp_into_pols_into_unemploy <- pols_data_clean %>%
  left_join(snp_data_clean, by = c("year", "month")) %>% 
 left_join(unemploy_clean, by = c("year", "month"))

min_year <- snp_into_pols_into_unemploy %>% pull(year) %>% min()

max_year <- snp_into_pols_into_unemploy %>% pull(year) %>% max()
  
```

The pols_month data includes 822 observations of 9 variables related to
the number of national politicians who are democratic or republican at
any given time. It includes the number of governors, senators, and,
representatives on both the democratic and republican side on associated
dates. The snp data contained 787 observations related to closing value
of the Standard & Poor’s stock market index (S&P) on specific dates of
observation. The unemployment data includes 68 observations and has
values percentage unemployment for the month for selected years.

The resulting joined data set is 822 X 11 tibble. The range of the years
is from 1947-2015. The name of key variables include year, month,
president (whether it was a gop or democratic president),
perct_unemployed. the number of republic or democratic senators is also
a helpful figure (sen_gop and sen_dem) to better understand if the
president had a house majority at the time.

### Problem 1

Below we import and clean data from
`NYC_Transit_Subway_Entrance_And_Exit_Data.csv`. The process begins with
data import, updates variable names, and selects the columns that will
be used in later parts fo this problem. We update `entry` from `yes` /
`no` to a logical variable. As part of data import, we specify that
`Route` columns 8-11 should be character for consistency with 1-7.

``` r
trans_ent = 
  read_csv(
    "data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
  janitor::clean_names() %>% 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

As it stands, these data are not “tidy”: route number should be a
variable, as should route. That is, to obtain a tidy dataset we would
need to convert `route` variables from wide to long format. This will be
useful when focusing on specific routes, but may not be necessary when
considering questions that focus on station-level variables.

The following code chunk selects station name and line, and then uses
`distinct()` to obtain all unique combinations. As a result, the number
of rows in this dataset is the number of unique stations.

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 465 × 2
##    station_name             line    
##    <chr>                    <chr>   
##  1 25th St                  4 Avenue
##  2 36th St                  4 Avenue
##  3 45th St                  4 Avenue
##  4 53rd St                  4 Avenue
##  5 59th St                  4 Avenue
##  6 77th St                  4 Avenue
##  7 86th St                  4 Avenue
##  8 95th St                  4 Avenue
##  9 9th St                   4 Avenue
## 10 Atlantic Av-Barclays Ctr 4 Avenue
## # … with 455 more rows
```

The next code chunk is similar, but filters according to ADA compliance
as an initial step. This produces a dataframe in which the number of
rows is the number of ADA compliant stations.

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 84 × 2
##    station_name                   line           
##    <chr>                          <chr>          
##  1 Atlantic Av-Barclays Ctr       4 Avenue       
##  2 DeKalb Av                      4 Avenue       
##  3 Pacific St                     4 Avenue       
##  4 Grand Central                  42nd St Shuttle
##  5 34th St                        6 Avenue       
##  6 47-50th Sts Rockefeller Center 6 Avenue       
##  7 Church Av                      6 Avenue       
##  8 21st St                        63rd Street    
##  9 Lexington Av                   63rd Street    
## 10 Roosevelt Island               63rd Street    
## # … with 74 more rows
```

To compute the proportion of station entrances / exits without vending
allow entrance, we first exclude station entrances that do not allow
vending. Then, we focus on the `entry` variable – this logical, so
taking the mean will produce the desired proportion (recall that R will
coerce logical to numeric in cases like this).

``` r
trans_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
## [1] 0.3770492
```

Lastly, we write a code chunk to identify stations that serve the A
train, and to assess how many of these are ADA compliant. As a first
step, we tidy the data as alluded to previously; that is, we convert
`route` from wide to long format. After this step, we can use tools from
previous parts of the question (filtering to focus on the A train, and
on ADA compliance; selecting and using `distinct` to obtain dataframes
with the required stations in rows).

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 60 × 2
##    station_name                  line           
##    <chr>                         <chr>          
##  1 Times Square                  42nd St Shuttle
##  2 125th St                      8 Avenue       
##  3 145th St                      8 Avenue       
##  4 14th St                       8 Avenue       
##  5 168th St - Washington Heights 8 Avenue       
##  6 175th St                      8 Avenue       
##  7 181st St                      8 Avenue       
##  8 190th St                      8 Avenue       
##  9 34th St                       8 Avenue       
## 10 42nd St                       8 Avenue       
## # … with 50 more rows

trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 17 × 2
##    station_name                  line            
##    <chr>                         <chr>           
##  1 14th St                       8 Avenue        
##  2 168th St - Washington Heights 8 Avenue        
##  3 175th St                      8 Avenue        
##  4 34th St                       8 Avenue        
##  5 42nd St                       8 Avenue        
##  6 59th St                       8 Avenue        
##  7 Inwood - 207th St             8 Avenue        
##  8 West 4th St                   8 Avenue        
##  9 World Trade Center            8 Avenue        
## 10 Times Square-42nd St          Broadway        
## 11 59th St-Columbus Circle       Broadway-7th Ave
## 12 Times Square                  Broadway-7th Ave
## 13 8th Av                        Canarsie        
## 14 Franklin Av                   Franklin        
## 15 Euclid Av                     Fulton          
## 16 Franklin Av                   Fulton          
## 17 Howard Beach                  Rockaway
```

\`\`\`
