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
# (B) FEATURE ENHANCEMENT
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
# (B) FEATURE ENHANCEMENT  
# -----------------------------------------------------------------------------------------------------------------------------------------------
# Retrain model using more predictor variables generated from spectral indicies 

# Band mapping (Landsat 8/9 surface reflectance, Collection 2 Level-2):
#   SR_B2 = Blue
#   SR_B3 = Green
#   SR_B4 = Red
#   SR_B5 = NIR
#   SR_B6 = SWIR1
#   SR_B7 = SWIR2
#
# Index formulas:
#   NDVI = (NIR - Red) / (NIR + Red)              = (B5 - B4) / (B5 + B4)
#   NDWI = (Green - NIR) / (Green + NIR)          = (B3 - B5) / (B3 + B5)   [McFeeters 1996]
#   NDBI = (SWIR1 - NIR) / (SWIR1 + NIR)          = (B6 - B5) / (B6 + B5)   [Zha et al. 2003]
#   SAVI = ((NIR - Red) / (NIR + Red + L)) * (1 + L), L = 0.5

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
  ) |>
  mutate(
    # --- Spectral indices ---
    NDVI = (SR_B5 - SR_B4) / (SR_B5 + SR_B4),
    NDWI = (SR_B3 - SR_B5) / (SR_B3 + SR_B5),
    NDBI = (SR_B6 - SR_B5) / (SR_B6 + SR_B5),
    SAVI = ((SR_B5 - SR_B4) / (SR_B5 + SR_B4 + 0.5)) * (1 + 0.5)
  ) |>
  # Guard against division-by-zero producing Inf/-Inf, if any convert to NA then filter layer
  mutate(
    across(c(NDVI, NDWI, NDBI, SAVI), ~ ifelse(is.finite(.x), .x, NA_real_))
  ) |>
  # Drop any rows where an index became NA (e.g., zero-sum denominator)
  filter(complete.cases(NDVI, NDWI, NDBI, SAVI))


train_data





# ---------------------------------------------------------------------------------------
# Train A random forest model from rangers which is quite fast for large datasets 
# ---------------------------------------------------------------------------------------
# Predictors 
predictors <- c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7", "NDVI","NDWI","NDBI","SAVI")
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


apply(results_df[, -1], 2, sd)
                        # With the standard deviation of 84% accross all folds the models shows high stability 
                        # it was consistent in all folds, no single one with amazingly higher/lower than others 


# Per fold metric heatmap 
library(tidyr)

results_df |>
  mutate(Fold = factor(seq_len(nrow(results_df)))) |>
  pivot_longer(-Fold, names_to = "Metric", values_to = "Value") |>
  ggplot(aes(x = Fold, y = Metric, fill = Value)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Value, 2)), size = 2.8) +
  scale_fill_viridis_c(option = "C", direction = 1) +
  labs(
    title = "Per-fold metric heatmap",
    subtitle = "Row-wise patterns = systematically hard folds; column-wise = unstable metrics",
    x = "Spatial fold", y = NULL, fill = "Value"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())





# model stability 
summary_df <- results_df |>
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") |>
  group_by(Metric) |>
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd   = sd(Value,   na.rm = TRUE),
    .groups = "drop"
  )

ggplot(summary_df, aes(x = reorder(Metric, mean), y = mean)) +
  geom_col(fill = "steelblue", width = 0.65) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, colour = "grey20") +
  geom_text(aes(label = sprintf("%.3f ± %.3f", mean, sd)),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Model stability across spatial folds",
    subtitle = "Bars = mean; whiskers = ± 1 SD",
    x = NULL, y = "Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())



# Confusion matrix 
all_preds <- do.call(rbind, spatial_predictions)

conf_mat(all_preds, truth = Actual, estimate = Predicted)
autoplot(conf_mat(all_preds, truth = Actual, estimate = Predicted), type = "heatmap")



# Variable importance , Only for the LAST fold's model
imp <- rf_model$variable.importance
sort(imp, decreasing = TRUE)
                        # Previously SR_B6 band was the most important, but in our new model NDWI has 
                        # contributed the most. 
imp_df <- data.frame(
  variable   = names(imp),
  importance = as.numeric(imp)
)

# Order by importance (reorder sets factor levels; flip = TRUE makes highest at top)
imp_df$variable <- reorder(imp_df$variable, imp_df$importance)

# Optional: flag whether a variable is a spectral index or a raw band
imp_df$type <- ifelse(
  grepl("^SR_B", imp_df$variable), "Raw band", "Spectral index"
)

ggplot(imp_df, aes(x = importance, y = variable, fill = type)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(importance, 3)),
    hjust = -0.15, size = 3.2
  ) +
  scale_fill_manual(values = c("Raw band" = "#4C72B0",
                               "Spectral index" = "#DD8452")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Variable importance — Random Forest Land-Cover Enhanced Model",
    subtitle = "Mean decrease in accuracy (scaled)",
    x = "Importance",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold")
  )



# NDWI dominates (0.333), roughly 45% higher than the next variable — your model leans heavily on 
# water discrimination (Green vs NIR). Given typical land-use classes this is a strong signal, 
# but also worth checking that NDWI isn't simply separating a single dominant class (e.g., water bodies) 
# and masking weaker class separation.
# The spectral indices collectively outweigh the raw bands (NDWI + SAVI + NDBI + NDVI ≈ 0.976 vs. all SR_B* ≈ 0.941), 
# which supports your feature-engineering step. Worth mentioning in the write-up.
# SR_B5 (NIR) is the least important (0.111) even though it feeds three of the four indices — expected collinearity 
# artefact: once NDVI/NDWI/NDBI are in the model, the raw NIR adds little marginal information. 
# This is normal and not a problem for Random Forest prediction, but you can note it when discussing multicollinearity.
# Accuracy-based importance is unstable across runs (it depends on permutation noise). 


# Save model 






















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
