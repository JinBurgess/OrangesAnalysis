library(readr)
library(dplyr)

df <- read_csv('./data/OrangeQualityData.csv')

colnames(df) <- c("Size", "Weight", "Sweetness",
                  "Acidity", "Softness", "HarvestTime", 
                  "Ripeness", "Color", "Variety", 
                  "Blemish", "Quality")

