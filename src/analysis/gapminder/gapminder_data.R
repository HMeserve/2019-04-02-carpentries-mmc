library(here)
library(tidyverse)
gapminder <- readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))

mean(gapminder$gdpPercap[gapminder$continent == "Africa"])
#more efficient method
year_country_gdp <- select(gapminder, year, country, gdpPercap)
head(year_country_gdp)

year_country_gdp <- gapminder %>%
  filter(continent=='Africa') %>%
  select(gapminder,continent, lifeExp) %>%
  
  
  gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))
#adding plot
ggplot(data = gapminder, aes(x=year, y=lifeExp, lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)

#taking filtered gapminder data
gapminder %>%
  filter(continent =="Africa") %>%
  ggplot(aes(x=year, y=lifeExp, lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)

library(tidyr)
library(tidyverse)
gapminder <-read_csv('data/gapminder/raw/gapminder_data.csv')

str(gapminder)
skimr::skim(gapminder)

gap_wide <- gapminder %>%
  gather(key = 'key', 
         value = 'value',
         c('pop', 'lifeExp', 'gdpPercap')) %>%
  mutate(year_var = paste(key, year, sep = '_')) %>%
  select(country, continent, year_var, value) %>%
  spread(key = 'year_var', value = 'value')

#wide to long
gap_long <- gap_wide %>%
  gather(key='obstype_year', 
         value='obs_values', 
         starts_with('pop'),
         starts_with('lifeExp'), 
         starts_with('gdpPercap'))
str(gap_long)
skimr::skim(gap_long)

gap_long2<-gap_long %>%
  separate(obstype_year, into=c('obs_type','year'),sep="_")

gap_long2$year <- as.integer(gap_long2$year)

gap_summary<-gap_long2 %>%
  group_by(continent, obs_type)%>%
  summarize(means=mean(obs_values))

#bring long dataset back to wide data format
gap_normal<-gap_long2 %>%
  spread(obs_type,obs_values)
#dim tell you the dimension
dim(gap_normal)

#all.equal() only used for exact matched columns
#gap_normal <- gap_normal[,names(gapminder)]
#all.equal(gap_normal,gapminder)
# adding new variable var_ID which has continent and country combined with _
gap_temp <- gap_long %>% 
  unite(var_ID,continent,country,sep="_", remove=FALSE)
str(gap_temp)

gap_wide_new <- gap_long2 %>%
  unite(ID_var,continent,country,sep="_") %>%
  unite(var_names,obs_type,year,sep="_") %>%
  spread(var_names,obs_values)
str(gap_wide_new)

#challenge
gap_ludicrously_wide <- gap_long %>%
  unite(var_names,obs_type,year,country,sep="_") %>%
  spread(var_names,obs_values)
