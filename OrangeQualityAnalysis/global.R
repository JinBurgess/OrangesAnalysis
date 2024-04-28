library(readr)
library(dplyr)
library(caret)

df <- read_csv('./data/OrangeQualityData.csv')

colnames(df) <- c("Size", "Weight", "Sweetness",
                  "Acidity", "Softness", "HarvestTime", 
                  "Ripeness", "Color", "Variety", 
                  "Blemish", "Quality")
set.seed(101)
X <- df%>%
  select(-c("Variety", "Sweetness"))
y <- df$Sweetness

# Split data into training and testing sets
splitIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[splitIndex, ]
X_test <- X[-splitIndex, ]
y_train <- y[splitIndex]
y_test <- y[-splitIndex]