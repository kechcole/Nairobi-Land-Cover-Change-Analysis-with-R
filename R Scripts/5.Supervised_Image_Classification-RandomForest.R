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
library(randomForest) # Random Forest
# library(rgdal)        # spatial data processing
library(raster)       # raster processing
# library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # ploting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # ploting
# library(sp)           # spatial data
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
# Train  a Random Forest Classiication using caret package 
# -------------------------------------------------------------

# 1. Set up parallelization to increase eiciency y quickly running loops and obtaining outputs fast 
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
                           # Numebr of cross validation folds
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
# 
p1<-predict(fit.rf,      # Model to e adopted 
            train.df,    # data to predict 
            type = "raw")    # Return raw values rather probailities

# Compare predicted p1 values with classes in train data 
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
# Get raster values as csv
dataFolder <- "E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/"
landsat_2023 <- rast(paste0(dataFolder, 'NAIROBI_L8_2023.tif'))

landsat_2023
# Number of layers, names, 
nlyr(landsat_2023)
names(landsat_2023)

# Reproject to EPSG:3395 (World Mercator)
reprojectedLandsat <- project(landsat_2023, "EPSG:3395")
reprojectedLandsat
crs(reprojectedLandsat)

# Select bands used in predicting model, band 2-7. 
landsat <- subset(reprojectedLandsat, c("SR_B2", "SR_B3","SR_B4", "SR_B5", "SR_B6", "SR_B7"))
landsat

# Convert raster to a data frame with coordinates , each cell value in all bands are captured
grided.data <- as.data.frame(landsat, xy=TRUE)
str(grided.data)
# View first few rows
head(grided.data)

# Load and Predict at grid location 
fit.rf <- readRDS(paste0(dataFolder,"RandomFores.rds"))
p3 <- as.data.frame(predict(fit.rf, grided.data))


# Extract predicted landuse class contained in a column and append to datarame
grided.data$PredLandUse <- p3$predict





# Get class id  
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
# RASTERIZATIN with terra & sf
# Convert to sf object (if not already)
sf_data <- st_as_sf(grid.data, coords = c("x", "y"), crs = 3395)
names(sf_data)

# Define raster extent and resolution
r <- rast(ext(sf_data), resolution = 15, crs = "EPSG:3395")  # Adjust resolution as needed

# Rasterize using the first column of data (change "value" to your column name)
rasterized <- rasterize(sf_data, r, field = "Class_ID", fun = mean)  # Use mean, sum, etc.

# Plot rasterd
plot(rasterized, main = "Rasterized Data", legend = TRUE, col = terrain.colors(10))

# Add legend manually (optional, for more customization)
legend("topright", legend = seq(min(values(rasterized), na.rm = TRUE), 
                                max(values(rasterized), na.rm = TRUE), 
                                length.out = 5),
       fill = terrain.colors(5), title = "Value")


# Convert raster to terra format if needed
r_terra <- rast(r)

# Define a categorical classification
tm_shape(r_terra) +
  tm_raster("Class_ID", palette = c("light grey", "burlywood4", "forestgreen", 
                                    "light green", "dodgerblue"), 
            title = "Land Use Classification") +
  tm_layout(legend.position = c("right", "center"))






# Color Palette
myPalette <- colorRampPalette(c("light grey","burlywood4", "forestgreen","light green", "dodgerblue"))
# Plot Map
LU<-spplot(r,"Class_ID", main="Supervised Image Classification: Random Forest" , 
      colorkey = list(space="right",tick.number=1,height=1, width=1.5,
              labels = list(at = seq(1,4.8,length=5),cex=1.0,
              lab = c("Road/parking/pavement" ,"Building", "Tree/buses", "Grass", "Water"))),
              col.regions=myPalette,cut=4)
LU
