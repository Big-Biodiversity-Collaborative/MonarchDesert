# download and filter naba data for monarchs
# katy prudic
# klprudic@email.arizona.edu
# 2021-11-15

library(dplyr)

# Download from GitHub
# naba1: "CO" "NM" "CA" "AZ" "ID" "MT" "NV"
naba1 <- read.csv(file = "https://raw.githubusercontent.com/jcoliver/citsci-western-butterflies/master/NABA1.csv")
# naba2: "OR" "WY" "WA" "UT"
naba2 <- read.csv(file = "https://raw.githubusercontent.com/jcoliver/citsci-western-butterflies/master/NABA2.csv")

# Filter for Monarch Observations
naba1_monarch <- naba1 %>%
  filter(ScientificName == "Danaus plexippus") %>%
  rename(Date = FromDate) %>%
  select(CountName, Date, State, Lat, Lng, ScientificName, NumSeen, Parties, Party_Hours)
naba2_monarch <- naba2 %>%
  filter(ScientificName == "Danaus plexippus") %>%
  select(CountName, Date, State, Lat, Lng, ScientificName, NumSeen, Parties, Party_Hours)
  
# Combine datasets
naba_monarch <- naba1_monarch %>%
  bind_rows(naba2_monarch)

# Format Date as a date
naba_monarch <- naba_monarch %>%
  mutate(Date = as.Date(Date, format = "%m/%e/%y"))

# Output to File
write.csv(x = naba_monarch, file = "data/naba-monarch-raw.csv", row.names = FALSE)

