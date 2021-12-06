# filter derham monarch data for timeseries
# katy prudic
# klprudic@email.arizona.edu
# 2021-12-06

library(dplyr)
library(lubridate)

derhamraw <- read.csv(file = "data/derham-monarch-raw.csv", 
                      na.strings = c("-","NA"))

# Convert monarch counts to numbers
derhamraw <- derhamraw %>%
  mutate(observedmonarch = as.numeric(observedmonarch))

# Filter to 3 sites
derhamsites <- derhamraw  %>%
  filter(sitename %in% c("McElvoyCanyon", "WillowCreek", "HunterCanyon"))

# Filter to fall months (October, November, December)
derhamfall <- derhamsites %>%
  filter(month %in% c(10,11,12)) %>%
  filter(!is.na(observedmonarch))
  

# Group by year and site
# Sum total monarch observations, count number of counts
derhamtime <- derhamfall %>%
  group_by(sitename, year) %>%
  summarize(totalmonarchs = sum(observedmonarch), 
            countsdone = n())

# Output to Data Folder
write.csv(x = derhamtime, file = "data/derham-monarch-timeseries.csv", row.names = FALSE)

# Graph time series data by site
# Plot controling for effort
derhamcorrect <- derhamtime %>%
  mutate(monarchspercount = totalmonarchs/countsdone)
library(ggplot2)

derhamplot <- ggplot(data = derhamcorrect, 
                     mapping = aes(x = year, y = monarchspercount, color = sitename)) +
  geom_line()
derhamplot
