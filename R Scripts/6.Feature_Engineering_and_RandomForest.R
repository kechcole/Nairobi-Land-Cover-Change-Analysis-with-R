# ---------------------------------------------------------------------------------------
#                              Feature Engineering 
# -------------------------------------------------------------------------------------

# ----------------------------------------------------------
# (A) MODEL IMPROVEMENTS THROUGH SPATIAL CROSS VALIDATION 

#  Train the model using spatial cross-validation to avoid inflated accuracy estimates (caused by spatial autocorrelation)
#  as observed in our previous model where it predicted over 98% of the features during training and testing. 
# Such high/inflated values are commonly attributed to spatial autocorrelation, a phenomena where pixels within the same proximity 
# have similar characteristics. Thus when modelling, a model encounters similar spectral characteristics as training
# and produces overly optimistic results that are deceiving and misleading due to spatial pattern similarity between test and train data. 
# This behaviour is commonly refered to as spatial data leakage. 
# The inability of the model to learn relationship causes it to perform poorly on unseen data. Training and test pixels are intermingled. 
# Neighbouring pixels may leak similar information across the split.
# To resolve this we will divide the training image data into blocks then use some for training and others for testing, 
# this is to reduce the likelyhod of similar closer pixels appearing in both train and test datasets. 





# ----------------------------------------------------------
# (B) FEATURE SELECTION 
# Add more variables and find optimal features that improve model performance. 
# Transformation and creation of existing variables helps improve the model by helping 
# a model learn patterns more effectively. 
# Creating spectral indicies such as NDVI, enhanced vegetation index(EVI), NDWI, SAVI and others are 
# more discriminative than raw reflectance since they capture biophysical properties of the land i.e water , built-up areas
# into the model an retrain 

install.packages("yardstick")

library(caret)        # machine laerning
library(dplyr)        # data manipulation 
library(sf)
library(blockCV)
library(ranger)
library(terra)
library(yardstick)
library(ggplot2)      # ploting


library(doParallel)   # Parallel processing
library(tmap)







# ---------------------------------------------------------------------------------------
# Load data 
# ---------------------------------------------------------------------------------------
# configure enviroment 
source("config/config_local.R")

# -------------------------------------------
# landsat data 
# Read raster data and select usable bands 
landsat_2023 <- rast(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR/Landsat_8_2023_Proj.tif"))
landsat_2023 <- landsat_2023[[c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")]]

# Convert raster to projected crs , utm 
landsat_2023 <- project(landsat_2023, "EPSG:32737")

# Inspect raster properties
landsat_2023

# Inspect layer/band names
names(landsat_2023)



# -----------------------------------------
# Load training data , convert vector data of digitized landsat data to dataframe of 30m 
poly <- st_read(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR", "Digitiezed_classes.gpkg"))
st_crs(poly)

# transform to a projected coordinate system 
poly <- st_transform(poly, 32737)

st_crs(poly)

# Build 30m template ----
template <- rast(poly, resolution = 30)
crs(template)

# Rasterize by Class_ID ----
r <- rasterize(vect(poly), template, field = "Class_ID", background = NA)
r

# Convert to dataframe with desired columns ----
df <- as.data.frame(r, xy = TRUE, na.rm = TRUE) %>%
  rename(x = x, y = y, Class_ID = Class_ID) %>%
  mutate(cell = cellFromXY(r, cbind(x, y))) %>%
  select(x, y, cell, Class_ID)

# Convert to SpatVector for extraction 
pts <- vect(df, geom = c("x", "y"), crs = crs(landsat_2023)) 

# Extract all band values that intersect with ----
vals <- extract(landsat_2023, pts)
vals

# Bind back with original attributes ----
result <- bind_cols(df, vals %>% select(-ID))
result


train.df <- result |> 
  dplyr::select(-cell)

train.df

# ----------------------------------------
# Add Human-Readable Class Names
train.df <- train.df %>%
  mutate(LandUseClass = case_when(
    Class_ID == 1 ~ "forest",
    Class_ID == 2 ~ "water",
    Class_ID == 3 ~ "builtup",
    Class_ID == 4 ~ "vegetation",
    Class_ID == 5 ~ "bare",
    TRUE          ~ NA_character_
  )) %>%
  mutate(LandUseClass = factor(LandUseClass))

# Inspect vector data 
str(train.df)
head(train.df)

# Check class distribution
table(train.df$LandUseClass)


# remove unwanted variables 
rm(result, vals, pts, df, template, poly)



# ---------------------------------------------------------------------------------------
# Generate spatial blocks 
# ---------------------------------------------------------------------------------------

# --------------------------
# convert columns to the required format coordinates to numeric , landuse class as factor and remove missing 
# values in all columns 
train_data <- train.df |>
  mutate(
    x = as.numeric(x),
    y = as.numeric(y),
    Class_ID = as.integer(Class_ID),
    LandUseClass = as.factor(LandUseClass)
  ) |>
  filter(
    complete.cases(
      x, y, Class_ID,
      SR_B2, SR_B3, SR_B4,
      SR_B5, SR_B6, SR_B7,
      LandUseClass
    )
  )

# check any missing values 
sum(is.na(train_data$LandUseClass))



# -----------------------------------------------------------------
# Estimate autocorrelation ranges , to investigate how far spatial dependence extends
# Autocorrelation of raster predictors tells you how geographically similar the input variables are

auto_range <- blockCV::cv_spatial_autocor(
  r = landsat_2023,
  num_sample = 5000,
  plot = TRUE
)
# Inspect results
auto_range$range

auto_range$range_table



# ----------------------------------------------------------------------
# Generate spatial blocks of 1500m 
set.seed(42)

spatial_folds <- cv_spatial(
  x = st_as_sf(as.points(r)),  # CONVERT spatial raster object to sf object
  column = "Class_ID",
  size = 1500,
  k = 5,
  hexagon = FALSE,
  selection = "random",
  iteration = 100,
  seed = 42,
  plot = TRUE
)

blockCV::cv_plot(
  cv = spatial_folds,
  x = st_as_sf(as.points(r))
)

str(spatial_folds$folds_list)

spatial_folds$records






# ---------------------------------------------------------------------------------------
# Train A random forest model from rangers which is quite fast for large datasets 
# ---------------------------------------------------------------------------------------
predictors <- c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")
predictors


class_levels <- levels(train_data$LandUseClass)

# Store results from each fold
spatial_results <- list()

# Store predictions
spatial_predictions <- list()

# Five-fold spatial cross-validation
for (i in seq_along(spatial_folds$folds_list)) {
      train_data
  # Extract training and testing indices
  train_index <- spatial_folds$folds_list[[i]][[1]]

  test_index <- spatial_folds$folds_list[[i]][[2]]

  # Subset training data
  fold_train <- train_data[
    train_index,
  ]

  # Subset testing data
  fold_test <- train_data[
    test_index,
  ]
  # Drop rows with NA response in the training fold
  fold_train <- fold_train[!is.na(fold_train$LandUseClass), ]

  # Train Random Forest
  rf_model <- ranger::ranger(
    LandUseClass ~ .,
    data = fold_train[, c("LandUseClass", predictors)],
    num.trees = 500,
    mtry = 3,
    importance = "permutation",
    seed = 42
  )

  # Predict testing observations
  predictions <- predict(
    rf_model,
    data = fold_test[, predictors]
  )$predictions

  # Ensure consistent factor levels
  predictions <- factor(
    predictions,
    levels = levels(train_data$LandUseClass)
  )

  actual <- factor(
    fold_test$LandUseClass,
    levels = levels(train_data$LandUseClass)
  )

  # Calculate evaluation metrics
  spatial_results[[i]] <- data.frame(
    Fold = i,

    Accuracy = yardstick::accuracy_vec(
      truth = actual,
      estimate = predictions
    ),

    Precision = yardstick::precision_vec(
      truth = actual,
      estimate = predictions,
      estimator = "macro"
    ),

    Recall = yardstick::recall_vec(
      truth = actual,
      estimate = predictions,
      estimator = "macro"
    ),

    F1 = yardstick::f_meas_vec(
      truth = actual,
      estimate = predictions,
      estimator = "macro"
    )
  )

  # Save predictions for later analysis
  spatial_predictions[[i]] <- data.frame(
    Actual = actual,
    Predicted = predictions,
    Fold = i
  )
}


# -----------------------------
# Model interpretation
results_df <- do.call(rbind, spatial_results)

# Per-fold
print(results_df)

# Mean ± SD across folds — this is your headline result
colMeans(results_df[, -1])
                        # On average the model has an accuracy of 97% 
                        # The F1 score shows the model the model is balanced accros all classes(no class bias)
                        # The model is highly robust considering we picked test pixels that are far away from 
                        # training pixels and still produced high accuracy, a trully world class model. 

# model stability 
apply(results_df[, -1], 2, sd)
                        # With the standard deviation of 89% accross all folds the models shows high stability 
                        # it was consistent in all folds, no single one with amazingly higher/lower than others 


# Confusion matrix 
all_preds <- do.call(rbind, spatial_predictions)

conf_mat(all_preds, truth = Actual, estimate = Predicted)
autoplot(conf_mat(all_preds, truth = Actual, estimate = Predicted), type = "heatmap")


# Variable importance , Only for the LAST fold's model
imp <- rf_model$variable.importance
sort(imp, decreasing = TRUE)
                        # Previously SR_B7 band was the most important, but in our new model SR_B6(SWIR 2) has 
                        # contributed the most. This could be due to its sensitivity to moisture where it 
                        # clearly delinates between built-up areas from vegetaion and bare ground
                        # All bands are critical to our analysis as their importance are not varying extremely, 
                        # between 0.23 to 0.36


# OOB classification error (1 - accuracy)
rf_model$prediction.error   
                # A random forest predicts by buiding trees and randomly separating then 70% for training and the remaining
                # 30% for test in a process called bootstraping. The test data is then predicted and gives us 
                # a prediction error, an out of box error of 1.24%. A substationlly minimall value , reinforcing 
                # the models good performance. 




# View all folds accuracy 
print(do.call(rbind, spatial_results))
print(rf_model)





# -----------------------------------------------------------------------------------------
# Retrain model using variable selected above 
# -----------------------------------------------------------------------------------------------------------------------------------------------





















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

# ------------------------------------------------------------------------
# Retrain model using variable selected above 
# -------------------------------------------------------------------------











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




# CLEAN THE ENTIRE ENVIROMENT 
rm(list = ls())


# REFERENCE 
# 1. Feature selection - https://mlr3book.mlr-org.com/chapters/chapter6/feature_selection.html
# 2. 
# 
