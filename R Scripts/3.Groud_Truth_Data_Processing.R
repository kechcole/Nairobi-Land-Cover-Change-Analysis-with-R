'''
            DATA PREPARATION FOR MACHINE LEARNING.
Reproject landsat data to a projected system that uses meter units 
Digitize land classes as vector and in the same projection as raster , through QGIS 
Rasterize the polygin data representing features then create grid pixels of 15m x 15m resolution 
Extract all band values and create vector data  
Create training, validation and test data by spliting vector data. 
Use data for predicting land use class

'''
# ---------------------------------------------------------------------------------
# Update and load packeges then setup enviroment 
# ---------------------------------------------------------------------------------
update.packages("sf")
update.packages("tidyverse")
install.packages("tidyverse")

library(terra)
library(dplyr)       # data manipulation 
library(ggplot2)     # ploting
library(sf)          # spatial data
library(tidyverse)   # data MANIPULATION


# ---------------------------------------------------------
# Setup enviroment 
source("config/config_local.R")
project_root <- system("git rev-parse --show-toplevel", intern = TRUE)
DATA_ROOT    <- file.path(project_root, "Nairobi Landsat data")




# -------------------------------------------------------------------------------------------------------------
# 1. Load data. Landsat data used for an area covering the greater Nairobi region downloaded from Google Earth Engine.
# -------------------------------------------------------------------------------------------------------

landsat_2023 <- rast(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR/Landsat_8_2023_Proj.tif"))
class(landsat_2023)
crs(landsat_2023)       # A mecartor projection (3395)


class(landsat_2023)     # type of layer 
crs(landsat_2023)      # Projection 
res(landsat_2023)       # Resolution 
nlyr(landsat_2023)      # Number of layers 
names(landsat_2023)     # Bands 


# -------------------------------------------------------------------------------------------------------------
# 2. Load vector data and reproject to same CRS as raster
# -------------------------------------------------------------------------------------------------------------

# The raster image was digitized to 5 classes, 1-forest, 2-water, 3-builtup, 4-vegetation and 5-bare lands
# The layer were then dissolved based on their class id leading to 5 layers. The polygons will be our training labels

# Load geopackage using sf package , layer is train_2023
gpkg_path <- file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR", "Digitiezed_classes.gpkg")

# Check available layers
st_layers(gpkg_path)

# load layer , because there is only one layer then no need to pass the layer name 
polygon1 <- st_read(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR", "Digitiezed_classes.gpkg"))

# Metadata 
polygon1


class(polygon1)     # data type 
crs(polygon1)      # Projection 
names(polygon1)        # Attributes in the data 
unique(polygon1$Class_Name)     # All classes available 



# -------------------------------------------------------------------------------------------------------------
# 3. Visualize Gigitized Classes 
# -------------------------------------------------------------------------------------------------------------

# Choose attribute containing class names to be Plotted 
attribute <- polygon1$Class_Name 

# Plot
plot(polygon1["Class_Name"], col = terrain.colors(length(unique(attribute))), 
     main = "Digitized Areas Colored by Land Type.", legend=TRUE)

# Add a legend
legend("bottomright", legend = unique(attribute), 
        fill = terrain.colors(length(unique(attribute))), title = "Legend", cex = 0.8)






# -------------------------------------------------------------------------------------------------------------
# 4. RASTERIZATION : Convert Polygon to raster of cell size 15m 
# -------------------------------------------------------------------------------------------------------------

# Converts vector polygons into a raster of class labels. This makes it trivial to align class values with 
# raster pixels — same grid, same CRS, same resolution.


# Define the raster resolution and extent, EPSG for UTM Zone 37S, WGS72
rast_template <- rast(ext(polygon1), resolution = 15, crs = "EPSG:3395")  

# Rasterize the polygon based on the 'Class_ID' attribute (numeric field required)
# rasterize() requires a numeric field. Class_ID works; Class_Name (a string) would not
rp <- rasterize(polygon1, rast_template, field = "Class_ID")
class(rp)
rp

# Plot the rasterized object
plot(rp, main = "Rasterized Polygon1", col = terrain.colors(5))




# -------------------------------------------------------------------------------------------------------------
# 5. Convert Rasterized Classes to Points 
# -------------------------------------------------------------------------------------------------------------
#  You need the class label as a point at each pixel location so it can be extracted alongside the spectral bands. 
#  Using points (rather than extracting polygon-wise) guarantees a 1:1 match between class and pixel.
# Raster → data frame with coordinates and cell numbers, Converts SpatRaster to a data frame with coordinates , 
rp.df <- as.data.frame(rp, xy = TRUE, cells = TRUE)  # xy = TRUE includes coordinates

class(rp.df)
colnames(rp.df) <- c("x", "y", "cell", "Class_ID")

# Build a SpatVector of points from the class raster
points_vect <- vect(rp.df, geom = c("x", "y"), crs = crs(rp))
points_vect$Class_ID <- rp.df$Class_ID

points_vect


# -------------------------------------------------------------------------------------------------------------
# 6. Extract Band Values at Class Points
# -------------------------------------------------------------------------------------------------------------
# Extract all band values at every class-labeled point
# Converting to a factor is essential for ML models — they need categorical targets.

# Extract raster values that intersect with each layer to points file 
# extract(): Works on SpatRaster objects with SpatVector points.
# bind = TRUE: Merges the extracted values with the point attribute table.
train_df <- extract(selected_bands, points_vect, bind = TRUE) %>%
  as.data.frame()

train_df

# ----------------------------------------
# Add Human-Readable Class Names
train_df <- train_df %>%
  mutate(LandUseClass = case_when(
    Class_ID == 1 ~ "forest",
    Class_ID == 2 ~ "water",
    Class_ID == 3 ~ "builtup",
    Class_ID == 4 ~ "vegetation",
    Class_ID == 5 ~ "bare",
    TRUE          ~ NA_character_
  )) %>%
  mutate(LandUseClass = factor(LandUseClass))

unique(train_df$LandUseClass)
table(train_df$LandUseClass)


# Save as csv
write.csv(train_df, paste0(dataFolder, '.\\pointsData.csv'))





# -------------------------------------------------------------------------
# 7. Randomly split data into training(70%) and test(30%) 
# -------------------------------------------------------------------------


library(caret)

# import csv
data <- read.csv(
  file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR", "pointsData.csv")
)

str(data)

# Set seed for replication and split data 
set.seed(78)

trainIndex <- createDataPartition(data$LandUseClass,   # target variable (factor)
                                    p = .7, 
                                    list=FALSE,
                                    time = 1)

train <- data[trainIndex, ]
test <- data[-trainIndex, ]
# Export as csv
write.csv(train, paste0(dataFolder, '.\\trainingData.csv'), row.names=F)
write.csv(test, paste0(dataFolder, '.\\testData.csv'), row.names=F)

# CLAEAN THE ENTIRE ENVIROMENT 
rm(list = ls())




# REERENCES 
# 1. https://zia207.github.io/geospatial-r-github.io/ground-truth-data-processing.html


