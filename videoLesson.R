library(gapminder)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Load the data
gapminder <- gapminder %>% mutate(year1950 = year - 1950)

# Nest the data by grouping by continent and country
by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

# Data can be now summarized country by country
by_country$data[[1]]

# Define a function that takes a dataframe and applies a model to it
country_model <- function(df){
  lm(lifeExp ~ year1950, data = df)
}

models <- by_country %>%
  mutate(
    mod = data %>% map(country_model)
  )

# Now all the models are nested in the data structure
models
models %>% filter(continent == "Africa")

models <- models %>%
  mutate(
    tidy    = mod %>% map(broom::tidy),
    glance  = mod %>% map(broom::glance),
    rsq     = glance %>% map_dbl("r.squared"), 
    augment = mod %>% map(broom::augment),
  )

models %>% arrange(desc(rsq))
