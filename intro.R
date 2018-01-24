# Specify the number of digits
options(digits = 3)
options(dplyr.width = Inf)
options("scipen" = 4)

# Load packages
library(tidyverse) # utility function
library(rpart) # regression trees
library(randomForest) # random forests
library(modelr) # package with mae function

# Read the data
melbourne_data <- read.csv("melb_data.csv")

# Print a summary of the data
summary(melbourne_data)

# Print the list of variable names
names(melbourne_data)

# Fit a decision tree
fit <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
               YearBuilt + Lattitude + Longtitude, data = melbourne_data)
# Plot regression tree
plot(fit, uniform = TRUE)
text(fit, cex = 0.6)

# Make predictions for the first 6 houses
print("The predicted prices are: ")
print(predict(fit, head(melbourne_data)))
print("The actual prices are:")
head(melbourne_data$Price)

# Calculate mean absolute error
mae(model = fit, data = melbourne_data)

# Split data to calculate out-of-sample error
splitData <- resample_partition(melbourne_data, c(test = 0.25, train = 0.75))

# Calculate number of cases in each set
lapply(splitData, dim)

# Fit a new model to the training set
fit2 <- rpart(Price ~ Rooms + Bathroom + Landsize + BuildingArea + YearBuilt +
                Lattitude + Longtitude, data = splitData$train)

# Get the MAE on test set
mae(model = fit2, data = splitData$test)

# Compute the MAE as a function of tree depth
get_mae <- function(maxdepth, target, predictors, training_data, testing_data){
  # Create formula to pass to rpart
  predictors <- paste(predictors, collapse = "+")
  formula <- as.formula(paste(target, "~", predictors, sep = ""))
  
  # Build our model
  model <- rpart(formula, data = training_data,
                 control = rpart.control(maxdepth = maxdepth))
  
  # Get the MAE
  mae <- mae(model, testing_data)
  return(mae)
}

# Fine-tune tree depth using out-of-sample error
target <- "Price"
predictors <- c("Rooms", "Bathroom", "Landsize", "BuildingArea",
                "YearBuilt", "Lattitude", "Longtitude")
for(i in 1:10){
  mae <- get_mae(maxdepth = i, target = target, predictors = predictors,
                 training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ", i, "\t MAE: ", mae))
}

# Fit a random forest model
fitRF <- randomForest(Price ~ Rooms + Bathroom + Landsize + BuildingArea +
                        YearBuilt + Lattitude + Longtitude, data = splitData$train,
                      na.action = na.exclude)

# Get the MAE of RF model
mae(model = fitRF, data = splitData$test)
