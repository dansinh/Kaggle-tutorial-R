# Load packages
library(tidyverse)

# Read data
farmData <- read_csv("farmData.csv")

# Print the first 10 rows
head(farmData, 10)

# Get the number of rows and columns
dim(farmData)

# Piping example
c(1, 2, 5, 5, 10) %>%
  mean() %>%
  prod(5)

# Get and sort the column names
columnAlphaOrder <- farmData %>%
  names() %>%
  sort()

# Print the first six names
head(columnAlphaOrder)

# Select or remove columns
farmData %>%
  select(gender1) %>%
  head()
farmData %>%
  select(-gender1) %>%
  head()
farmData %>%
  select(starts_with("gender")) %>%
  head()
farmData %>%
  select(-starts_with("gender")) %>%
  head()

# Select rows
farmData %>%
  filter(vname == "Tikare")
farmData %>%
  filter(fplots >= 6)
farmData %>%
  filter(vname == "Tikare" | vname == "Sefula")
farmData %>%
  filter(vname == "Bonanza" & fplots < 3)

# Add a new variable
farmData <- farmData %>%
  mutate(landowner = (tenure1 == 1 | tenure1 == 2))
summary(farmData$landowner)

# Create a new variable
countOfMen <- farmData %>%
  select(starts_with("gender")) %>%
  magrittr::equals(1) %>%
  rowSums(na.rm = TRUE)
countOfWomen <- farmData %>%
  select(starts_with("gender")) %>%
  magrittr::equals(2) %>%
  rowSums(na.rm = TRUE)
farmData <- farmData %>%
  mutate(men = countOfMen, women = countOfWomen) %>%
  mutate(menPluswomen = men + women)
summary(farmData$menPluswomen)

# Compare old and new variables
(farmData$hhsize == farmData$menPluswomen) %>%
  na.omit() %>%
  table()

# Sort your dataframe based variable values using Arrange
farmData %>%
  arrange(menPluswomen) %>%
  head()
farmData %>%
  select(menPluswomen, hhsize) %>%
  arrange(menPluswomen) %>%
  head()
farmData %>%
  arrange(desc(menPluswomen)) %>%
  head()
farmData %>%
  select(menPluswomen, hhsize) %>%
  arrange(desc(menPluswomen)) %>%
  head()
farmData %>%
  select(menPluswomen, hhsize) %>%
  arrange(hhsize) %>%
  tail()

# Summarize variables
farmData %>%
  summarize(meanMenPlusWomen = mean(menPluswomen),
            meanhhsize = mean(hhsize, na.rm = TRUE))
farmData %>%
  summarize(missingMenPlusWomen = sum(menPluswomen == 0),
            missinghhsize = sum(is.na(hhsize)))

# Combine summarize and group_by
farmData %>%
  group_by(landowner) %>%
  summarize(plots = mean(fplots, na.rm = TRUE))
farmData %>%
  group_by(fplots) %>%
  tally()
farmData %>%
  group_by(landowner, fplots) %>%
  tally() %>%
  na.omit()
farmData %>%
  group_by(interviewer) %>%
  tally() %>%
  arrange(desc(n))
adaptationsCount <- farmData %>%
  select(contains("ad71")) %>%
  rowSums()
farmData %>%
  mutate(adaptations = adaptationsCount) %>%
  group_by(adaptations) %>%
  tally()
farmData %>%
  mutate(adaptations = adaptationsCount) %>%
  group_by(landowner) %>%
  summarize(AveNumAdaptations = mean(adaptations, na.rm = TRUE))
