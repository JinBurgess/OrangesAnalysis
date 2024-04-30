# install.packages("randomForest")
# install.packages("e1071")
library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(stringr)

df <- read_csv('./data/OrangeQualityData.csv')

colnames(df) <- c("Size", "Weight", "Sweetness",
                  "Acidity", "Softness", "HarvestTime", 
                  "Ripeness", "Color", "Variety", 
                  "Blemish", "Quality")


overview.content <- tagList(
  tags$h3(tags$strong("Orange Craze"), style = "text-align: center;"),
  tags$h5("by Jin YuHan Burgess", style = "text-align: center;"),
  tags$p(),
  tags$p(),
  tags$p("Variables in Dataset:",
         tags$ul(
           tags$li("Size (cm)"), tags$li("Weight (g)"), tags$li("Brix (Sweetness)"),
           tags$li("pH (Acidity)"), tags$li("Softness (1-5)"), tags$li("HarvestTime (days)"),
           tags$li("Ripeness (1-5)"), tags$li("Color"), tags$li("Variety"),
           tags$li("Blemishes (Y/N)"), tags$li("Quality (1-5)")
         )
  ),
  tags$p(),
  tags$p("Data Set can be found at Kaggle:", href = "https://www.kaggle.com/datasets/shruthiiiee/orange-quality")
  )
