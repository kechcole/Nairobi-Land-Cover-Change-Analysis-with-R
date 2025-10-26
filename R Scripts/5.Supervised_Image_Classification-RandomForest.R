''' ---------------------------------------------------------------------------------
                    Section 2 : SUPERVISED CLASSIFICATION.
Using a set of pre-labelled sample data, an analyst trains a classifier that will then use knowledge learnt to classify an image.
Once an algorithm learns, it is exposed to unknown pixels and assign them to different classes. Sample data data can be fetched 
from field visits, expert knowledge, or ancilliary data such as maps. Input class information is based on prior knowledge of the user, this 
will guide how pixels will be grouped, these bounds are based on brighthness/spectral reflactance characteristics. 
Example of such algorithms include random forest, support vector 
machine, and decision trees. One huge advantage is that this algorithm meets user objectives or needs resulting to less effort and 
time/cost saving due to higher accuracies. The downside is that in real world scenario, data is homegenious this it does not capture 
natures variabilities and complexities. Users can also spend a lot of time when collecting sampling data.  

                  Steps In Supervised Classification. 
      a) Select Training Samples. 
Define the number of feature class of interest to your study and generate training samples for each. Can be through a digitsation process 
where a polygons or points are drawn, ensure it covers majority of the image and classes are distributed. 
 
      b) Generate Signature File.
Collected data values for different classes need to be stored for future reference in signature file. A spectral signature is the DN values, 
it defines a wavelength pattern for GCP and represent a certain class.  

      c) Classify. 
Finally, as an analyst will classify the image using methods such as Random forest, Maximum Likelihood, SVM, Iso-cluster, PCA, 
Maximum distance etc. 


                  2.1 Random Forest. 
This is an ensemble algorithm that involves many decison tree, each with a prediction that is then aggregated and the class with most votes 
takes the day. Each tree  


'''

# Install new packages
packages <- c("randomForest", "plyr", "RStoolbox", "RColorBrewer", "doParallel", "tmap")

ipak <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg)) 
       install.packages(new.pkg, dependencies = TRUE)
   sapply(pkg, require, character.only = TRUE)
 }


ipak((packages))

library(caret)        # machine laerning
library(raster)       # raster processing
library(dplyr)        # data manipulation 
library(ggplot2)      # ploting
library(doParallel)   # Parallel processing

library(sf)
library(terra)
library(tmap)



# Load data 
train.df<-read.csv("E:/DISK E PETER/flux files/New folder/NAIROBI_LANDSAT_MERCATOR/trainingData.csv", header = T)
test.df<-read.csv("E:/DISK E PETER/flux files/New folder/NAIROBI_LANDSAT_MERCATOR/testData.csv", header = T)

# Randomly select 10% of each data making it lighter for the machine 
train.df <- train.df %>% sample_frac(.1)
test.df <- test.df %>% sample_frac(.1)

names(train.df)


# -------------------------------------------------------------
# Train a Random Forest Classfier using caret package 
# -------------------------------------------------------------

# 1. Set up parallelization to increase efficiency by quickly running loops and obtaining outputs fast 
# because the data we using is large 
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# 2. Set seed the model can be reproduced later 
set.seed(120)

# Define hyperparameters or the Random forest function 
# These settings will guide how the model is trained and validated , it includes the resampling method 
# number of folds for validation, numer of times the cross validation is repeated, whether parralelization 
# is allowed
myControl <- trainControl( # Split data multiple times into training datasets 
                           method="repeatedcv", 
                           # Number of cross validation folds
                          number=3, 
                          # repeat cross validation 
                          repeats=2,
                          # Retain sampling results for all training in order to know how well it performed in each
                          returnResamp='all', 
                          # Enable parallel processing to speed up training and improve results 
                          allowParallel=TRUE)

# 3. Train model 
fit.rf <- train(  # Convert target variale into a cartegorical variale, predictor variables are B2-B7 
                  as.factor(LandUseClass)~SR_B2 + SR_B3 + SR_B4+ SR_B4 + SR_B6 + SR_B7, 
                data=train.df,     # Training dataset containing variables 
                method = "rf",     # Random forest classifier method 
                metric= "Accuracy",    # model evaluation method 
                preProc = c("center", "scale"),   # Standardize and center the data 
                trControl = myControl      # training control setting 
                )
fit.rf

# Stop cluster 
stopCluster(mc)

# Save model 
saveRDS(fit.rf, paste0(dataFolder,"RandomFores.rds"))



# 4. Evaluate the model in the training dataset by predicting land use class on train data 
#  using the model and computing the confusion matrix(compares actual values and predicted ones )
#  p1 will be a vector containing predicted values, (not attached automatically to the dataset)
p1<-predict(fit.rf,      # Model to be adopted 
            train.df,    # data to predict 
            type = "raw")    # Return raw values rather probailities

# Computes accuracy metrics
# Compare predicted p1 values with classes in train data 
# Results include  1.accuracy > higher betters, 
#                  2.Confidence level - higher better 
#                  3.kappa 
#                  4.p-value - 
train.df$LandUseClass <- as.factor(train.df$LandUseClass)
confusionMatrix(p1, train.df$LandUseClass)


# 5. Predict on unseen test data
p2 <- predict(fit.rf, test.df)

# Compare predicted p1 values with classes in test data 
test.df$LandUseClass <- as.factor(test.df$LandUseClass)
confusionMatrix(p2, test.df$LandUseClass)



# -------------------------------------------------------------------------------
# Lets predict at grid location , data contains spatial points
# terra and sf packages used 
# --------------------------------------------------------------------------
# Read raster and vector data as terra objects 
dataFolder <- "E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/"
landsat_2023 <- rast(paste0(dataFolder, 'NAIROBI_L8_2023.tif'))   # Raster objects

# Number of layers, names, in raster file 
nlyr(landsat_2023)
names(landsat_2023)

# Read polygon 
aoi <- vect(paste0(dataFolder, 'Nairobidata.gpkg'), layer="AOI")    # vector object
aoi
# Convert to sf
aoi_sf <- st_as_sf(aoi)

# Plot
ggplot(aoi_sf) +
  geom_sf(fill = "lightgreen", color = "darkgreen") +
  theme_classic() +
  labs(title = "Area of Interest (AOI)")



# Reproject raster to EPSG:3395 (World Mercator)
reprojectedLandsat <- project(landsat_2023, "EPSG:3395")
reprojectedLandsat

aoi <- project(aoi,  "EPSG:3395")
aoi
crs(aoi)


# Select bands used in predicting model, band 2-7. 
landsat <- subset(reprojectedLandsat, c("SR_B2", "SR_B3","SR_B4", "SR_B5", "SR_B6", "SR_B7"))
landsat

# Mask the landsat image on the Area of Interest(clipping)
landsat_clipped <- crop(landsat, aoi)  # Crop to bounding box
landsat_clipped <- mask(landsat, aoi)  # Mask to exact shape
landsat_clipped <- terra::trim(landsat_clipped) # Remove missing values for original areas not within the aoi


# Plot as RGB image
terra::plotRGB(landsat_clipped, r=3, g=2, b=1, stretch="lin", smooth=FALSE, axes=TRUE)  # Linear stretch


# Convert raster to a data frame with coordinates, each cell value in all bands 
# are captured
grided.data <- as.data.frame(landsat_clipped, xy=TRUE)
str(grided.data)



# Load random forest model and fit at grid location 
model.rf <- readRDS(paste0(dataFolder,"RandomFores.rds"))
p3 <- as.data.frame(predict(model.rf, grided.data))


# Extract predicted landuse class contained in a column and append to dataframe
grided.data$PredLandUse <- p3$predict
str(grided.data)


# Get class id , a new column containing class ID of the predicted values 
grid.data <- grided.data %>% 
                     mutate(Class_ID = case_when(
                              PredLandUse == "water" ~ 2,
                              PredLandUse == "vegetation" ~ 4,
                              PredLandUse == "builtup" ~ 3,
                              PredLandUse == "bare" ~ 5,
                              TRUE ~ 1      # forest
                           ))

names(grid.data)


# --------------------------------------
# RASTERIZATIoN with terra & sf
# -------------------------------------
# Convert to sf object , coordinates x & y will be put to geometry column 
sf_data <- st_as_sf(grid.data, coords = c("x", "y"), crs = 3395)
names(sf_data)

# Define raster extent and resolution similar to landsat image 
r <- rast(ext(sf_data), resolution = 30, crs = "EPSG:3395")

rasterized <- rasterize(sf_data, r, field = "Class_ID")


# ---------------------------------------------------------------
# Plot 
# ------------------------------------------------------------------
# base plot
plot(rasterized, main = "Rasterized Data", legend = FALSE, 
      col = c("#056d05", "#1E90FF", "#8B0000", "#82eb82", "#DAA520"))
    


# Plot with tmap
# Define class labels and matching colors
class_labels <- c("Vegetation", "Water", "Built-up", "Grassland", "Bare Land")
class_colors <- c("#056d05", "#1E90FF", "#8B0000", "#82eb82", "#DAA520")

# Assign raster categories explicitly
levels(rasterized) <- data.frame(ID = 1:5, Class = class_labels)

# Create the map with a properly positioned legend
tm_shape(rasterized) +
  tm_raster(palette = class_colors, title = "Class Value", style = "cat") +  
  tm_layout(legend.outside = TRUE, legend.outside.position = "right") +
  tm_add_legend(type = "fill", labels = class_labels, col = class_colors)


# --------------------------------------------------
# variable importance 
# ----------------------------------------------------------
# Extract and plot importance
var_imp <- varImp(model.rf)
plot(var_imp, main = "Variable Importance (Caret Random Forest)")



# ----------------------------------------------------------
# Feature Engineering 
# ----------------------------------------------------------
# A) Add more variales and study the effect on the model
# Create ndvi, enhanced vegetation index, false colour, ndwi, savi and other then incoorperate 
# into the model an retrain 

# I)Train data 
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
     allowParallel=FALSE)

# Register parallelisation 
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
fit.rf2

# Stop cluster 
stopCluster(mc)

# . Predict on unseen test data
p4 <- predict(fit.rf2, new_testData)

# Compare predicted p2 values with classes in new test data 
new_testData$LandUseClass <- as.factor(new_testData$LandUseClass)

# check metrics 
confusionMatrix(p4,new_testData$LandUseClass)



