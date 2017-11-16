library(Amelia)
library(dplyr)
library(tidyr)

# Tidy the earthquake data 
eq_data <- read.table(file = 'results.tsv', sep = '\t', 
                      header = TRUE, stringsAsFactors = F)

# Plot the missing field data using missmap
missmap(eq_data,x.cex=0.5,y.cex=0.5,rank.order = F)

# convert NA values in HOUR, MINUTE,SECOND to 0 
eq_data[c("HOUR","MINUTE","SECOND")][is.na(eq_data[c("HOUR","MINUTE","SECOND")])] <- 0 

# Convert Date from character to date in R
eq_date <- eq_data %>% unite(DATE,YEAR:DAY,sep="-",remove=T) 
eq_time <- eq_date %>% unite(E_TIME,HOUR:SECOND,sep=":",remove=T)
eq_origin <- eq_time %>% unite(ORIGIN,DATE:E_TIME,sep=" ",remove=T) 
eq_origin$ORIGIN <- as.POSIXct(eq_origin$ORIGIN,tz="GMT")

# reorder the columns and arrange the data frame by country 
country_eq <- eq_origin %>% 
  select(COUNTRY:LONGITUDE,FOCAL_DEPTH,REGION_CODE,ORIGIN,everything()) %>% 
  arrange(COUNTRY)

# tidy the world gdp data
world_gdp <- read.csv(file='world_gdp_Data.csv',sep=",",
                     col.names=c("Series.Name","Series.Code",
                     "Country.Name","Country.Code",1960:2016),
                     stringsAsFactors = F,check.names = F)
                      
missmap(world_gdp,x.cex=0.5,y.cex=0.5,rank.order=F)

world_gdp_data <- head(world_gdp,-5)
gdp_tidy <- world_gdp_data %>% gather("Year","Value",5:61)

gdp_order <- gdp_tidy %>% select(Country.Name,Country.Code,everything()) %>% 
  arrange(Country.Name)

gdp_spread <- gdp_order %>% unite(temp,Series.Name,Series.Code,sep="|") %>% 
  spread(key=temp,value = Value)

# tidy the population data 
popul <- read.csv(file="world_population_Data.csv",sep=",",
                       col.names=c("Series.Name","Series.Code",
                                   "Country.Name","Country.Code",1960:2016),
                  stringsAsFactors = F,check.names = F)

missmap(popul,x.cex=0.5,y.cex=0.5,rank.order=F)
world_popu <- head(popul,-5)
popu_tidy <- world_popu %>% gather("Year","Population",5:61)
popu_order <- popu_tidy %>% arrange(Country.Name) %>% 
  select(Country.Name,Country.Code,Year,everything())

# need to add in population as a column. 
gdp_population <- left_join(gdp_spread,popu_order)
colnames(gdp_population)
