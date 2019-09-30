library(tidyverse)
library(dplyr)
library(gapminder)

summary(gapminder)
glimpse(gapminder)
head(gapminder)
tail(gapminder)
sum(is.na(gapminder))

gp_cnt_life <- select(gapminder, country, lifeExp)
head(gp_cnt_life)

gp_no_pop <- select(gapminder, -pop)
head(gp_no_pop)

gp_1957 <- gapminder %>% filter(year == 1957)
glimpse(gp_1957)
head(gp_1957, n=10)

gp_us <- gapminder %>% filter(country == "United States")
head(gp_us, n=15)

gp_1957_Asia <- gapminder %>% filter(year == 1957, continent == "Asia")
head(gp_1957_Asia, 8)

write.csv(gp_1957_Asia, "gapminder1957Asia.csv")

gapminder %>% arrange(pop)

gapminder %>% arrange(desc(pop))

gapminder %>%
  filter(year == 1957) %>%
  arrange(desc(gdpPercap))

gapminder %>% mutate(pop = pop/1000000)
gapminder %>% mutate(gdp = gdpPercap * pop)
gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1957) %>%
  arrange(desc(gdp))

head(gapminder)

gap_gdp_1957 <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1957) %>%
  arrange(desc(gdp))

gapminder %>% 
  summarize(meanLifeExp = mean(lifeExp))

gapminder %>%
  filter(year == 1957) %>%
    summarize(meanLifeExp = mean(lifeExp))

gapminder %>%
  filter(year == 1957) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

# my code for mean life exp and gdp of U.S. in 2002
gapminder %>%
  filter(year == 2002, country == "United States") %>%
  summarize(medianLifeExp = median(lifeExp),
            gdp = median(gdpPercap * pop))

gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

# my code for mean gdp of every country every year
gapminder %>%
  group_by(country, year) %>%
  summarize(gdp = (gdpPercap * pop))


# visualizations

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))

ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point() + 
  expand_limits(y=0)

by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))

ggplot(by_year_continent, aes(x = year, y = totalPop, color = continent)) +
  geom_point() + 
  expand_limits(y=0)

# my code for visualizing mean gdp per cap of each continent as function of time

by_year_continent_gdp <- gapminder %>%
  group_by(year, continent) %>%
  summarize(mean_gdpPercap = mean(gdpPercap))

ggplot(by_year_continent_gdp, aes(x = year, y = mean_gdpPercap, color = continent)) +
  geom_point() + 
  expand_limits(y=0)


# descriptive data analysis

summary(gapminder)

gap2007 <- gapminder %>% filter(year == "2007")

gap2007[which.min(gap2007$lifeExp),]
gap2007[which.min(gap2007$pop),]
gap2007[which.min(gap2007$gdpPercap),]

gap2007[which.max(gap2007$lifeExp),]
gap2007[which.max(gap2007$pop),]
gap2007[which.max(gap2007$gdpPercap),]

start_year <- min(gapminder['year'])
end_year <- max(gapminder['year'])
start_pop <- min(gapminder['pop'])
end_pop <- max(gapminder['pop'])

pop_growth_rate <- (end_pop - start_pop)/(end_year - start_year)
pop_growth_rate

gap_grouped <- gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)),
            meanGdpPercap = mean(gdpPercap))
summary(gap_grouped)

ggplot(gap_grouped, aes(x = year, y = meanLifeExp, color = continent)) + geom_line()
ggplot(gap_grouped, aes(x = year, y = totalPop, color = continent)) + geom_line()
ggplot(gap_grouped, aes(x = year, y = meanGdpPercap, color = continent)) + geom_line()

my_continents <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
sapply(my_continents, function(cont){
  gapminder %>%
    filter(continent == cont) %>%
    summary()
})

summary(gapminder)
