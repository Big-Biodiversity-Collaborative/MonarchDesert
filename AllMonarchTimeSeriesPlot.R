# plot monarch time series data
# katy prudic
# klprudic@email.arizona.edu
# 2021-12-06

# Load libraries for this script
library(ggplot2)
library(dplyr)
library(tidyr)

# Load datasets for this script
derhamplot <- read.csv(file = "data/derham-monarch-timeseries.csv")
nabaplot <- read.csv(file = "data/naba-monarch-timeseries.csv")
xercesplot <- read.csv(file = "data/xerces-monarch-timeseries.csv")

# Group derham data by year
derhamplot <- derhamplot %>%
  group_by(year) %>%
  summarize(totalmonarchs = sum(totalmonarchs), 
            countsdone = sum(countsdone)) %>%
  ungroup()

# Add seaon column to derham and naba data sets
derhamplot <- derhamplot %>%
  mutate(season = "derhamfall")

nabaplot <- nabaplot %>%
  mutate(season = "nabasummer") %>%
  rename(year = Year)

xercesplot <- xercesplot %>%
  mutate(season = "xercesfall")

# Join all 3 data sets together
threeplot <- xercesplot %>%
  select(-datasource) %>%
  bind_rows(derhamplot) %>%
  bind_rows(nabaplot)

# Filter data to years 1998 - 2020
threeplot <- threeplot %>%
  filter(year %in% 2005:2020)

# Standardize effort in monarch observations
threeplot <- threeplot %>%
  mutate(monarchspercount = totalmonarchs/countsdone)

# Plot timeseries data for all seasons
ggplot(data = threeplot, 
       mapping = aes(x = year, y = monarchspercount, color = season)) +
  geom_line()

# Need to plot relative abundance changes
threeplot <- threeplot %>%
  group_by(season) %>%
  mutate(relativemonarchs = monarchspercount/max(monarchspercount))

# Plot timeseries relative observation data for all seasons
ggplot(data = threeplot, 
       mapping = aes(x = year, y = relativemonarchs, color = season)) +
  geom_line()
ggsave(filename = "output/monarchsallyearsallsources.png")

# Make data for comparing between seasons
threewide <- threeplot %>%
  select(-c(totalmonarchs, countsdone, relativemonarchs)) %>%
  pivot_wider(id_cols = year, names_from = season, values_from = monarchspercount)
  
# Plot derhamfall xercesfall comparison
ggplot(data = threewide,
       mapping = aes(x = derhamfall, y = xercesfall)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(data = threewide,
       mapping = aes(x = nabasummer, y = derhamfall)) +
  geom_point()+
  geom_smooth(method = "lm")


# Plot nabasummer xercesfall comparison
ggplot(data = threewide,
       mapping = aes(x = nabasummer, y = xercesfall)) +
  geom_point() +
  geom_smooth(method = "lm")

# Run lm predicting xercesfall from nabasummer data
summer_lm <- lm(xercesfall ~ nabasummer, data = threewide)
summary(summer_lm)  

# Make data set with summer + 1 to compare with fall datasets
threewide <- threewide %>% 
  mutate(nextsummer = lead(nabasummer))

# Plot comparison derhamfall nextsummer
ggplot(data = threewide,
       mapping = aes(x = derhamfall, y = nextsummer)) +
  geom_point() +
  geom_smooth(method = "lm")


# Plot comparison xercesfall nextsummer
ggplot(data = threewide, 
       mapping = aes(x = xercesfall, y = nextsummer)) +
  geom_point() +
  geom_smooth(method = "lm")


