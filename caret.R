# Load packages
library(tidyverse)
library(caret)
library(randomForest)
library(Metrics)

# Read in the  data and filter observations
player_statistics <- read_csv("PUBG_Player_Statistics.csv")
player_statistics <- player_statistics %>%
  select(starts_with("solo"))

# Make a data frame with the max & min value per column
max_min <- data_frame(max = apply(player_statistics, 2, max),
                      min = apply(player_statistics, 2, min),
                      columns = names(player_statistics))

# Vector of useless column names
useless_columns <- max_min$columns[max_min$min == max_min$max]
# add  minus signs so select() will remove them
useless_columns <- paste("-", useless_columns, sep = "")

# Remove useless columns
player_statistics <- player_statistics %>%
  select_(.dots = useless_columns)

# Remove leaky variables
player_statistics <- player_statistics %>%
  group_by(solo_WinRatio) %>%
  select(-contains("win")) %>% 
  select(-contains("loss")) %>%
  ungroup()

player_statistics <- player_statistics %>%
  na.omit() %>%
  select_if(is.numeric)

# Split data into testing & training
set.seed(1234)

# Train/test split
training_indexs <- createDataPartition(player_statistics$solo_WinRatio, p = .2, list = F)
training <- player_statistics[training_indexs, ]
testing  <- player_statistics[-training_indexs, ]

# Get predictors
predictors <- training %>% select(-solo_WinRatio) %>% as.matrix()
output <- training$solo_WinRatio

# Train a random forest model
model <- randomForest(x = predictors, y = output, ntree = 50)
rmse(predict(model, testing), testing$solo_WinRatio)

# Tune a random forest model
model_tuned <- train(x = predictors, y = output, ntree = 10, method = "rf")
ggplot(model_tuned)
rmse(predict(model, testing), testing$solo_WinRatio)
rmse(predict(model_tuned, testing), testing$solo_WinRatio)

# Plot variable importance
par(mfrow = c(1,2))
varImpPlot(model, n.var = 5)
varImpPlot(model_tuned$finalModel, n.var = 5)
