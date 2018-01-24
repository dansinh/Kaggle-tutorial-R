# Load packages
library(tidyverse)
library(ggthemes)

# Read the data
candyRankings <- read_csv("candy_data.csv")
candyProduction <- read_csv("candy_production.csv")

# Scatterplot of candy rankings data
ggplot(data = candyRankings, aes(x = sugarpercent, y = pricepercent,
                                 label = competitorname)) +
  geom_point() +
  geom_smooth(method = "loess", lwd = 1.5) +
  geom_text(check_overlap = TRUE, vjust = "bottom", nudge_y = 0.01,
            angle = 30, size = 3) +
  labs(title = "More sugary candies are more expensive",
      x = "Sugar content (percentile)", y = "Price (percentile)") +
  theme_wsj()

# Select candy features from the dataset
candyFeatures <- candyRankings %>%
  select(2:10)
candyFeatures[] <- lapply(candyFeatures, as.logical)

# Make a barplot of candy features
ggplot(data = candyFeatures, aes(x = chocolate, fill = caramel)) +
  geom_bar(position = "dodge") +
  facet_wrap("caramel") +
  scale_fill_manual(values=c("#BBBBBB", "#E69F00")) +
  labs(title = "Chocolate candies are more likely to have caramel",
       x = "Is the candy chocolate?",
       y = "Count of candies") +
  theme(legend.position = c(0.9, 0.9),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme_fivethirtyeight()

# Line chart
ggplot(data = candyProduction, aes(x = observation_date, y = IPG3113N)) +
  geom_line() +
  geom_smooth(method = "loess", lwd = 1.5) +
  labs(title = "Monthly candy production (US)",
       x = "", y = "As percent of 2012 production") +
  theme_wsj()
