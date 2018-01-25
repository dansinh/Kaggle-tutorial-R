# Load packages
library(xgboost)
library(tidyverse)

# Read the data in
diseaseInfo <- read_csv("Outbreak_240817.csv")

# Randomly reshuffle the dataset
set.seed(123)
diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)), ]

# View dataset
head(diseaseInfo)

# Clean the data
diseaseInfo_humansRemoved <- diseaseInfo %>%
  select(-starts_with("humans"))

# Create labels
diseaseLabels <- diseaseInfo %>%
  select(humansAffected) %>%
  is.na() %>%
  magrittr::not()

# check out the first few lines
head(diseaseLabels) # of our target variable
head(diseaseInfo$humansAffected) # of the original column

# Retain only relevant, numeric variables
diseaseInfo_numeric <- diseaseInfo_humansRemoved %>%
  select(-c(Id, longitude, latitude)) %>%
  select_if(is.numeric)
str(diseaseInfo_numeric)

# One-hot encoding matrix
region <- model.matrix(~ country - 1, diseaseInfo)
diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription,
                                              "domestic")

# Get a list of all the species by getting the last
speciesList <- diseaseInfo$speciesDescription %>%
  str_replace("[[:punct:]]", "") %>% # remove punctuation (some rows have parentheses)
  str_extract("[a-z]*$") # extract the least word in each row

# convert our list into a dataframe...
speciesList <- tibble(species = speciesList)

# and convert to a matrix using 1 hot encoding
options(na.action = 'na.pass') # don't drop NA values!
species <- model.matrix(~ species - 1,speciesList)

# Combine matrices
diseaseInfo_numeric <- cbind(diseaseInfo_numeric, region, species)
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)

# Split into training set and test set
Ntrain <- round(length(diseaseLabels) * 0.8)
# Training data
train_data <- diseaseInfo_matrix[1:Ntrain, ]
train_labels <- diseaseLabels[1:Ntrain, ]
# Test data
test_data <- diseaseInfo_matrix[-(1:Ntrain), ]
test_labels <- diseaseLabels[-(1:Ntrain), ]

# Convert into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# Train a xgboost model
model <- xgboost(data = dtrain, nround = 20, objective = "binary:logistic")

# Make predictions and calculate error
pred <- predict(model, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("Test error = ", err))

# Tuning model
model_tuned <- xgboost(data = dtrain, max.depth = 5, nround = 20,
                       objective = "binary:logistic")
pred <- predict(model_tuned, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("The test error = ", err))

# Reweight positive cases
negative_cases <- sum(train_labels == FALSE)
postive_cases <- sum(train_labels == TRUE)
model_tuned <- xgboost(data = dtrain, max.depth = 3,
                       nround = 20, early_stopping_rounds = 10,
                       objective = "binary:logistic",
                       scale_pos_weight = negative_cases/postive_cases,
                       gamma = 0.25)
pred <- predict(model_tuned, dtest)
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))

# Plot feature tree
xgb.plot.multi.trees(feature_names = names(diseaseInfo_matrix), model = model)

# Importance matrix plot
importance_matrix <- xgb.importance(names(diseaseInfo_matrix), model = model)
xgb.plot.importance(importance_matrix)
