"""
                  SUPERVISED VECTOR MACHINE.
SVM models are commonly used for classification, it determiness a hypersurface, a best boundary line 
that separates feature classes. This creates the largest distance/margin between classes, kind of safe distance 
between groups. The optimal distance between the nearest data points of the groups is maximised, these points 
are refered to as support vectors. 
Since most data is not linearly separated, i.e remote sensed data, Svm circumvents this by using kernel functions. 
This model maps data to a hyperdimension where groups are separable. For remote sensing , the model will find the best 
boundary between (NDVI-NDWI-NDBI-SWIR-SAVI-BLUE-RED-GREEN-space) that separates land cover classes (Forest, Urban, Water, etc.).


"""
# load libraries 
library(caret)        # machine laerning
library(dplyr)        # data manipulation 
library(ggplot2)      # ploting
library(doParallel)   # Parallel processing
library(sf)
library(terra)
library(tmap)


# Load data folder 
dataFolder <- "E:/DISK E PETER/flux files/New folder/"

# Grided data 
grided.data <- read.csv(file.path(dataFolder, "NAIROBI_LANDSAT_MERCATOR/grided_data.csv"))

# Train data 
train.df<-read.csv(file.path(dataFolder, "NAIROBI_LANDSAT_MERCATOR/trainingData.csv"))
test.df<-read.csv(file.path(dataFolder, "NAIROBI_LANDSAT_MERCATOR/testData.csv"))

# Randomly select 10% of each data making it lighter for the machine 
train.df <- train.df %>% sample_frac(.1)
test.df <- test.df %>% sample_frac(.1)


# Transform grided, train and test data 
train.df <- train.df %>%
  rename(BLUE=SR_B2, RED=SR_B3, GREEN=SR_B4, NIR=SR_B5, SWIR1=SR_B6, SWIR2=SR_B7 ) %>%
  mutate(
        NDVI = (NIR - RED)/(NIR + RED),  
        NDWI = (GREEN - NIR)/(GREEN + NIR), 
        NDBI = (SWIR1 - NIR) / (SWIR1 + NIR),   
        SAVI = ((0.5 + 1) * (NIR - RED)) / (NIR + RED + 0.5)   
        )%>%
          # Select important columns 
  select(x, y, BLUE, RED, GREEN, SWIR1, SWIR2, NDVI, NDWI, NDBI, SAVI, LandUseClass)
    
test.df <- test.df %>%
    rename(BLUE=SR_B2, RED=SR_B3, GREEN=SR_B4, NIR=SR_B5, SWIR1=SR_B6, SWIR2=SR_B7 ) %>%
    mutate(
          NDVI = (NIR - RED)/(NIR + RED),  
          NDWI = (GREEN - NIR)/(GREEN + NIR), 
          NDBI = (SWIR1 - NIR) / (SWIR1 + NIR),   
          SAVI = ((0.5 + 1) * (NIR - RED)) / (NIR + RED + 0.5)   
          )%>%
    select(x, y, BLUE, RED, GREEN, SWIR1, SWIR2, NDVI, NDWI, NDBI, SAVI, LandUseClass)


grided.data <- grided.data %>%
    rename(BLUE=SR_B2, RED=SR_B3, GREEN=SR_B4, NIR=SR_B5, SWIR1=SR_B6, SWIR2=SR_B7 ) %>%
    mutate(
          NDVI = (NIR - RED)/(NIR + RED),  
          NDWI = (GREEN - NIR)/(GREEN + NIR), 
          NDBI = (SWIR1 - NIR) / (SWIR1 + NIR),   
          SAVI = ((0.5 + 1) * (NIR - RED)) / (NIR + RED + 0.5)   
          )%>%
    select(x, y, BLUE, RED, GREEN, SWIR1, SWIR2, NDVI, NDWI, NDBI, SAVI)


# --------------------------------------------------------------------
# Train model 
# ---------------------------------------------------------------------
# Initiate parralelization 
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# Tuning parameters 
myControl <- trainControl(method="repeatedcv", 
                          number=3, 
                          repeats=2,
                          returnResamp='all', 
                          allowParallel=TRUE)

set.seed(849)
fit.svm <- train(as.factor(LandUseClass)~BLUE + RED + GREEN + SWIR1 + SWIR2 + NDVI + NDWI + NDBI + SAVI, 
                data=train.df,
                method = "svmRadial",
                metric= "Accuracy",
                preProc = c("center", "scale"), 
                trControl = myControl
                )
fit.svm 

# stop clusture 
stopCluster(mc)

# confusion matrix on train data 
p1<-predict(fit.svm, train.df, type = "raw")
train.df$LandUseClass <- as.factor(train.df$LandUseClass)
confusionMatrix(p1, train.df$LandUseClass)


# confusion matrix on test data, convert feature class to factor 
p2<-predict(fit.svm, test.df, type = "raw") 
test.df$LandUseClass <- as.factor(test.df$LandUseClass)
confusionMatrix(p2, test.df$LandUseClass)








# CLEAN THE ENTIRE ENVIROMENT 
rm(list = ls())







