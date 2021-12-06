# filter naba monarch data for timeseries
# katy prudic
# klprudic@email.arizona.edu
# 2021-12-06

library(dplyr)
library(lubridate)

nabaraw <- read.csv(file = "data/naba-monarch-raw.csv")

# Convert data column from text to date format
nabaraw <- nabaraw %>%
  mutate(Date = as.Date(Date))

# + Limit to California (wait on this for now)

# + Limit to Summer Months (June, July, August, September)
nabasummer <- nabaraw %>%
  filter(month(Date) %in% c(6,7,8))
  
# + Limit from years 1997-2020
nabasummer <- nabasummer %>%
  filter(year(Date) %in% c(1997:2020))

# + Group by years
# + Sum columns NumSeen, Count number of counts
nabatime <- nabasummer %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(totalmonarchs = sum(NumSeen), 
            countsdone = n())

# Output to Data Folder
write.csv(x = nabatime, file = "data/naba-monarch-timeseries.csv", row.names = FALSE)
