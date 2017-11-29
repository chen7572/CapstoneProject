library(Amelia)
library(dplyr)
library(tidyr)
library(ggplot2)

# Tidy the earthquake data 
eq_data <- read.table(file = 'results.tsv', sep = '\t', 
                      header = TRUE, stringsAsFactors = F)

empty_cell <- is.na(eq_data)
cell_cor <- cor(empty_cell)

# visulaize the correlation of missing fields 
par( mar = c( 10, 10, 0.5, 0.5 ),cex = 0.5)
image(cell_cor, col = rev(heat.colors(12)), xaxt = "n", yaxt = "n")
axis( 1, at=seq(0,1,length.out=nrow( cell_cor ) ), 
      labels= rownames( cell_cor ), las= 2 )
axis( 2, at=seq(0,1,length.out=ncol( cell_cor ) ), 
      labels= colnames( cell_cor ), las= 2)

# Plot the missing field data using missmap
missmap(eq_data,x.cex=0.5,y.cex=0.5,rank.order = F)

# convert NA values in HOUR, MINUTE,SECOND to 0 
# eq_data[c("HOUR","MINUTE","SECOND")][is.na(eq_data[c("HOUR","MINUTE","SECOND")])] <- 0 

# Convert Date from character to date in R
eq_date <- eq_data %>% unite(DATE,YEAR:DAY,sep="-",remove=T) 
#eq_time <- eq_date %>% unite(E_TIME,HOUR:SECOND,sep=":",remove=T)
#eq_origin <- eq_time %>% unite(ORIGIN,DATE:E_TIME,sep=" ",remove=T) 
#eq_origin$ORIGIN <- as.POSIXct(eq_origin$ORIGIN,tz="GMT")
eq_date$DATE <- as.POSIXct(eq_date$DATE)

# reorder the columns and arrange the data frame by country 
country_eq <- eq_date %>% 
  select(COUNTRY:LONGITUDE,FOCAL_DEPTH,REGION_CODE,DATE:SECOND,everything()) %>% 
  arrange(COUNTRY)


# tidy the world gdp data
world_gdp <- read.csv(file='world_gdp_Data.csv',sep=",",
                     col.names=c("Series.Name","Series.Code",
                     "Country.Name","Country.Code",1960:2016),
                     stringsAsFactors = F,check.names = F)
                      
missmap(world_gdp,x.cex=0.5,y.cex=0.5,rank.order=F)

world_gdp_data <- head(world_gdp,-5)
gdp_tidy <- world_gdp_data %>% gather("Year","Value",5:61)

gdp_order <- gdp_tidy %>% select(Country.Name,Country.Code,everything(),-Series.Code) %>% 
  arrange(Country.Name)

#gdp_spread <- gdp_order %>% unite(temp,Series.Name,Series.Code,sep="|") %>% 
#  spread(key=temp,value = Value)

gdp_spread <- gdp_order %>% spread(key=Series.Name,value = Value)
  
# tidy the population data 
popul <- read.csv(file="world_population_Data.csv",sep=",",
                       col.names=c("Series.Name","Series.Code",
                                   "Country.Name","Country.Code",1960:2016),
                  stringsAsFactors = F,check.names = F)

missmap(popul,x.cex=0.5,y.cex=0.5,rank.order=F)
world_popu <- head(popul,-5)
popu_tidy <- world_popu %>% gather("Year","Population",5:61)
popu_order <- popu_tidy %>% arrange(Country.Name) %>% 
  select(Country.Name,Country.Code,Year,everything(),-Series.Code,-Series.Name)

# Add in population as a column. 
gdp_population <- left_join(gdp_spread,popu_order)
colnames(gdp_population)

# Perform data analysis 
gdp_population[gdp_population == ".."] = NA

ggplot(country_eq,aes(x=EQ_PRIMARY, y = log(TOTAL_DEATHS))) +
  geom_point()

#cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
#               "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(country_eq,aes(x=FOCAL_DEPTH, y = log(TOTAL_DEATHS),color = EQ_PRIMARY)) +
  geom_jitter() +
  scale_color_gradientn(colors=rainbow(4)) +
  scale_x_continuous(limits = c(0, 300))

ggplot(country_eq,aes(x=FOCAL_DEPTH, y = log(TOTAL_DAMAGE_MILLIONS_DOLLARS),
                      color = EQ_PRIMARY)) +
  geom_jitter() +
  scale_color_gradientn(colors=rainbow(4)) +
  scale_x_continuous(limits = c(0, 100))

ggplot(country_eq,aes(x=log(TOTAL_DAMAGE_MILLIONS_DOLLARS),y=log(TOTAL_DEATHS))) + 
  geom_point()

ggplot(country_eq, aes(x= format(DATE,"%Y"), y = log(TOTAL_DEATHS))) +
  stat_summary(fun.y=sum,geom="bar") + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete("Year")

# Need to make sure the names are consistent 
colnames(gdp_population)[1] <- "COUNTRY"
gdp_population$COUNTRY <- toupper(gdp_population$COUNTRY)
country_eq$Year <- format(country_eq$DATE,"%Y")
df_eq_gdp <- left_join(country_eq,gdp_population)

# Need to clean up some of the inconsistent names       
a <- unique(gdp_population$COUNTRY)
b <- unique(country_eq$COUNTRY)
c <- setdiff(b,a)

colnames(df_eq_gdp)[48] <- "GDP_constant2010USD"
colnames(df_eq_gdp)[49] <- "GDP_currentUSD"
colnames(df_eq_gdp)[50] <- "GDP_per_capita_2010USD"
colnames(df_eq_gdp)[51] <- "GDP_per_capita_currentUSD"

ggplot(df_eq_gdp,aes(x = as.numeric(GDP_per_capita_currentUSD),y=log(TOTAL_DEATHS))) +
  geom_jitter() +
  scale_x_continuous("GDP per capita current $US",limits = c(0,60000))

ggplot(df_eq_gdp,aes(x = as.numeric(Population),y=log(TOTAL_DEATHS))) +
  geom_jitter() +
  scale_x_continuous("Country Population")

ggplot(df_eq_gdp,aes(x=TOTAL_DEATHS)) +
  geom_histogram(bins = 50,color="black",fill = "white")

