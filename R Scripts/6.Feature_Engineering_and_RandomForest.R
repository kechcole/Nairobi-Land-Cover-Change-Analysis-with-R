# ----------------------------------------------------------
# Feature Engineering 
# ----------------------------------------------------------
# A) Add more variales and study the effect on the model
# Transformation and creation of existing variables helps improve the model by helping 
# a model learn patterns more effectively. 
# Creating spectral indicies such as NDVI, enhanced vegetation index(EVI), NDWI, SAVI and others are 
# more discriminative than raw reflectance since they capture biophysical properties of the land i.e water , built-up areas
# into the model an retrain 


library(caret)        # machine laerning
library(dplyr)        # data manipulation 
library(ggplot2)      # ploting
library(doParallel)   # Parallel processing
library(sf)
library(terra)
library(tmap)


# I)Read data 
dataFolder <- "E:/DISK E PETER/flux files/New folder/"

# Grided data 
grided.data <- read.csv(file.path(dataFolder, "NAIROBI_LANDSAT_MERCATOR/grided_data.csv"))


# Train data 
train.df<-read.csv(file.path(dataFolder, "NAIROBI_LANDSAT_MERCATOR/trainingData.csv"))
test.df<-read.csv(file.path(dataFolder, "NAIROBI_LANDSAT_MERCATOR/testData.csv"))

# Randomly select 10% of each data making it lighter for the machine 
train.df <- train.df %>% sample_frac(.1)
test.df <- test.df %>% sample_frac(.1)


new_trainData <- train.df %>% 
      # Rename columns 
      rename(BLUE=SR_B2, RED=SR_B3, GREEN=SR_B4, NIR=SR_B5, SWIR1=SR_B6, SWIR2=SR_B7 ) %>%
      # Calculate spectral indicies 
      mutate(
            NDVI = (NIR - RED)/(NIR + RED),  # vegetation
            NDWI = (GREEN - NIR)/(GREEN + NIR), # WATER
            NDBI = (SWIR1 - NIR) / (SWIR1 + NIR),  # Built up areas 
            SAVI = ((0.5 + 1) * (NIR - RED)) / (NIR + RED + 0.5)  # Soil index 
      ) %>%
      # Reorder columns 
      select(x, y, BLUE, RED, GREEN, NIR, SWIR1, SWIR2, NDVI, NDWI, NDBI, SAVI, Class_ID, LandUseClass)


# II) tEST DATA 
new_testData <- test.df %>%
      rename(BLUE=SR_B2, RED=SR_B3, GREEN=SR_B4, NIR=SR_B5, SWIR1=SR_B6, SWIR2=SR_B7 ) %>%
      mutate(NDVI = (NIR - RED)/(NIR + RED),  # vegetation
             NDWI = (GREEN - NIR)/(GREEN + NIR), # WATER
             NDBI = (SWIR1 - NIR) / (SWIR1 + NIR),  # Built up areas 
             SAVI = ((0.5 + 1) * (NIR - RED)) / (NIR + RED + 0.5)  # Soil 
            ) %>%
      select(x, y, BLUE, RED, GREEN, NIR, SWIR1, SWIR2, NDVI, NDWI, NDBI, SAVI, Class_ID, LandUseClass)
      


# B) Train a random forest model and predict on unseen data 
# Define hyperparameters 

myControl2 <- trainControl( # Split data multiple times into training datasets 
      method="repeatedcv", 
      # Number of cross validation folds
     number=3, 
     # repeat cross validation 
     repeats=2,
     # Retain sampling results for all training in order to know how well it performed in each
     returnResamp='all', 
     # disable parallel processing  
     allowParallel=TRUE)

# Register parallelization 
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)

fit.rf2 <- train(  # Convert target variale into a cartegorical variale, then pass predictor variables as is  
      as.factor(LandUseClass)~BLUE + RED + GREEN + NIR + SWIR1 + SWIR2 + NDVI + NDWI + NDBI + SAVI, 
    data=new_trainData,     # Training dataset containing variables 
    method = "rf",     # Random forest classifier method 
    metric= "Accuracy",    # model evaluation method 
    preProc = c("center", "scale"),   # Standardize and center the data 
    trControl = myControl2      # training control setting 
    )
# check results 
fit.rf2

# Stop cluster 
stopCluster(cl)

# Save model 
saveRDS(fit.rf2, paste0(dataFolder,"Nairobi Landsat data/RandomForest2.rds"))

# read model
fit.rf2 <- readRDS(paste0(dataFolder,"Nairobi Landsat data/RandomForest2.rds"))

# . Predict on unseen test data
p4 <- predict(fit.rf2, new_testData)

# Compare predicted p2 values with classes in new test data 
new_testData$LandUseClass <- as.factor(new_testData$LandUseClass)

# check metrics , 
confusionMatrix(p4,new_testData$LandUseClass)



# C) Variable importance 
importance_df <- varImp(fit.rf2)$importance |>
  tibble::rownames_to_column("Variable")

ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Variable Importance - Random Forest",
       x = "Variables", y = "Importance Score") +
  theme_minimal()

# select only most important variables , from above NIR was not that important
# so we pick the top 9 of the 10
importance_vals <- varImp(fit.rf2, scale = TRUE)
print(importance_vals)

# Extract the numeric importance column safely
imp_df <- importance_vals$importance
imp_df$Variable <- rownames(imp_df)

# Order by importance descending
imp_df <- imp_df[order(imp_df$Overall, decreasing = TRUE), ]

# Select top 9 variables
top_vars <- imp_df$Variable[1:9]
cat("Selected important variables:\n")
print(top_vars)





# -------------------------------------------------------------------------------
# Predict at grid location and convert to raster 
# -------------------------------------------------------------------------------
# Grided data is organised into cells(pixels) covering a geographical area defined by x and y coordinates 
# and put in dataframe format. 
# Each cell represents an area on the ground (30m x 30m) and store other spectral variables 
# such as ndvi, red, green, blue, swir bands. 
# Data is extracted from landsat image in which we will predict their classes 

# i) Prepare grided data 
new_gridData <- grided.data %>% 
      # Rename columns because the columns must be similar to the one used in training data 
      rename(BLUE=SR_B2, RED=SR_B3, GREEN=SR_B4, NIR=SR_B5, SWIR1=SR_B6, SWIR2=SR_B7 ) %>%
      # Calculate spectral indicies 
      mutate(
            NDVI = (NIR - RED)/(NIR + RED),  # vegetation
            NDWI = (GREEN - NIR)/(GREEN + NIR), # WATER
            NDBI = (SWIR1 - NIR) / (SWIR1 + NIR),  # Built up areas 
            SAVI = ((0.5 + 1) * (NIR - RED)) / (NIR + RED + 0.5)  # Soil index 
      ) %>%
      # Reorder columns 
      select(x, y, BLUE, RED, GREEN, NIR, SWIR1, SWIR2, NDVI, NDWI, NDBI, SAVI)

head(new_gridData, 10)


# ii) Load the model and view properties inclusing accuracy , samples used, variables , tuning parameters 
randFstModel <- readRDS(paste0(dataFolder,"RandomForest2.rds"))
print(randFstModel)


# iii) Predict at grid locations, predict the class for each cell  
# the prediction is a dataframe with 1 column containing predicted class 
p4 <- as.data.frame(predict(randFstModel, newdata = new_gridData))

# Extract predicted landuse class contained in a prediction4 and append to dataframe
new_gridData$PredLandUse <- p4$predict
str(new_gridData)

# Get class id , a new column containing class ID of the predicted values 
new_gridData <- new_gridData %>% 
                     mutate(Class_ID = case_when(
                              PredLandUse == "water" ~ 2,
                              PredLandUse == "vegetation" ~ 4,
                              PredLandUse == "builtup" ~ 3,
                              PredLandUse == "bare" ~ 5,
                              TRUE ~ 1      # forest
                           ))
names(new_gridData)


# iv) Define raster extent and resolution similar to landsat image then 
# rasterize the sf object
# Convert df to sf object , coordinates x & y will be put to geometry column 
sf_data <- st_as_sf(new_gridData, coords = c("x", "y"), crs = 3395)
names(sf_data)

r <- rast(ext(sf_data), resolution = 30, crs = "EPSG:3395")
rasterized <- rasterize(sf_data, r, field = "Class_ID")


# v) Plot with tmap
# Define class labels and matching colors
class_labels <- c("Vegetation", "Water", "Built-up", "Grassland", "Bare Land")
class_colors <- c("#056d05", "#1E90FF", "#8B0000", "#82eb82", "#DAA520")

# Assign raster categories explicitly
levels(rasterized) <- data.frame(ID = 1:5, Class = class_labels)

# Create the map with a properly positioned legend
tm_shape(rasterized) +
  tm_raster(palette = class_colors, title = "Land Cover", style = "cat") +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right")




# CLAEAN THE ENTIRE ENVIROMENT 
rm(list = ls())
