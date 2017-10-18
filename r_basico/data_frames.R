##########################################################################
# Jose Ramón Cajide, 2017-10
# Customer Analytics: Data Frames
##########################################################################

# Algunos paquetes traen conjuntos de datos
# install.packages('gapminder')
library(gapminder)
gapminder

# Motivación
library(tidyverse)
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country, color = continent)) +
  geom_line(alpha = 1/3) 



# importación de datos ----------------------------------------------------
# install.packages('tidyverse')
library(tidyverse)

gapminder <- read_csv("r_basico/data/gapminder.csv")

head(gapminder)
tail(gapminder, 2)
str(gapminder)
summary(gapminder)
summary(gapminder$continent)
gapminder$country
unique(gapminder$continent)
length(unique(gapminder$continent))
barplot(table(gapminder$continent))

# Motivación:
mean(gapminder[gapminder$continent == "Africa", "gdpPercap"]$gdpPercap, na.rm = TRUE)
# Y para Asia? Y

# Incluido en tidyverse
# install.packages('dplyr')
# library("dplyr")

# select()
year_country_gdp <- select(gapminder,year,country,gdpPercap)

# Usando pipe %>%

mean(1:20)

1:20 %>% mean()

head(gapminder)

gapminder %>% head %>% tail(2)

year_country_gdp <- gapminder %>% select(year,country,gdpPercap)

gapminder %>% select(year,country,gdpPercap)
gapminder %>% select(-continent, -lifeExp, -pop)

# filter()
year_country_gdp_euro <- gapminder %>%
  filter(continent=="Europe") %>%
  select(year,country,gdpPercap)

# group_by() sobre una variable

gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))


# ¿Qué países tienen la mayor y menor esperanza de vida? 

lifeExp_bycountry <- gapminder %>%
  filter(continent == "Europe") %>% 
  group_by(country) %>%
  summarize(mean_lifeExp=mean(lifeExp)) %>% 
  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp)) 


# group_by() sobre más de una variable

gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))

# summarise_each()
# Partimos de:

gapminder %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap),
            sd_gdpPercap=sd(gdpPercap),
            mean_pop=mean(pop),
            sd_pop=sd(pop))

gapminder %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise_each(funs(mean, median), gdpPercap, pop)

# count() y n()

# Cuantas observaciones hay por continente?

gapminder %>% 
  count(continent)

gapminder %>%
  group_by(continent) %>%
  summarise(n = n())

# Tabla de frecuencias
table(gapminder$continent)


# n_distinct()

gapminder %>%
  group_by(continent) %>%
  summarize(n = n(),
            n_countries = n_distinct(country))


# mutate()

gapminder %>% 
  mutate(gdp = pop * gdpPercap,
         prueba = row_number()) %>% View ()


# arrange

gapminder %>%
  filter(year == 2007) %>%
  arrange(lifeExp)

mola <- gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(lifeExp))



# rename

gapminder %>%
  rename(life_exp = lifeExp,
         gdp_percap = gdpPercap)

# reordenar variables

gapminder %>%
  filter(country == "Burundi", year > 1996) %>% 
  select(yr = year, lifeExp, gdpPercap) %>% 
  select(gdpPercap, everything())


# Incremento de la esperanza de vida desde 1952

gapminder %>% 
  group_by(country) %>% 
  # filter(country == "Burundi") %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp)) %>%  View()


