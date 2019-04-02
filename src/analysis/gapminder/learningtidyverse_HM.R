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

ggplot(data = gapminder, aes(x=year, y=lifeExp, lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)

#taking filtered gapminder data
gapminder %>%
  filter(continent =="Africa") %>%
  ggplot(aes(x=year, y=lifeExp, lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)

