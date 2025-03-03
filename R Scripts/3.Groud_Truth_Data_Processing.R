
# library(conflicted)
# library(rgdal)       # spatial data processing
library(terra)
library(raster)      # raster processing
library(plyr)        # data manipulation 
library(dplyr)       # data manipulation 
library(RStoolbox)   # ploting spatial data 
library(RColorBrewer)# color
library(ggplot2)     # ploting
# library(sp)          # spatial data
library(sf)          # spatial data

library(gridExtra)
library(tidyverse)   # data MANIPULATION

update.packages("sf")
update.packages("tidyverse")
install.packages("tidyverse")


# Load data. Landsat data used for an area covering the greater Nairobi region downloaded from Google Earth Engine.
dataFolder <- "E:/DISK E PETER/flux files/New folder/NAIROBI_LANDSAT_MERCATOR/"

# Load raster data and create a terra stack , find the coordinate reference system
landsat_23 <- rast(paste0(dataFolder, 'Landsat_8_2023_Proj.tif'))
class(landsat_23)
crs(landsat_23)       # A mecartor projection (3395)

class(landsat_23)

# Subset bands(BLUE, GREEN, RED, NIR,SWIR1,SWIR2) and plot 
selected_bands <- landsat_23[[2:7]]
# Confirm if oject is a stack, SpatRaster
class(selected_bands)
plot(selected_bands)




# Step 4. Load vector data and reproject to same CRS as raster
# The raster image was digitized to 5 classes, 1-forest, 2-water, 3-builtup, 4-vegetation and 5-bare lands
# The layer were then dissoled ased on their class id leading to 5 layers. 

# Load geopackage using sf package , layer is train_2023
polygon1 <- st_read(paste0(dataFolder, 'Digitiezed_classes.gpkg'), layer = 'layer')

# Metadata 
polygon1

# Polygon type 
class(polygon1)

# Projection 
crs(polygon1)

# Plot
plot(st_geometries(polygon1))



#Step 5. 
# Choose attribute to be Plotted 
attribute <- polygon1$Class_Name 

# Plot
plot(polygon1["Class_Name"], col = terrain.colors(length(unique(attribute))), 
     main = "Digitized Areas Colored by Land Type.", legend=TRUE)

# Add a legend
legend("bottomright", legend = unique(attribute), 
        fill = terrain.colors(length(unique(attribute))), title = "Legend", cex = 0.8)






# ------------------------------------------------------
# Convert Polygon to raster

# Polygon extent
extent <- extent(polygon1)
extent

# Get CRS /projection of polygon to be used in rasterisation 
crs = st_crs(polygon1)$wkt
crs



# -------- USING Tera package , modern  -----------
# rast(): Creates an empty raster template with the specified extent, resolution, and CRS.
# ext(): Automatically extracts the extent from the polygon object.
# crs: Assigns the EPSG code (use "EPSG:32237" for UTM Zone 37S with WGS72).
# rasterize(): Directly rasterizes the polygon using the numeric Class_ID attribute.

# Define the raster resolution and extent, EPSG for UTM Zone 37S, WGS72
rast_template <- rast(ext(polygon1), resolution = 15, crs = "EPSG:3395")  

# Rasterize the polygon based on the 'Class_ID' attribute (numeric field required)
rp <- rasterize(polygon1, rast_template, field = "Class_ID")
class(rp)
rp

# Plot the rasterized object
plot(rp, main = "Rasterized Polygon1", col = terrain.colors(10))


# ------------------------
# Converts SpatRaster to a data frame with coordinates
rp.df <- as.data.frame(rp, xy = TRUE, cells = TRUE)  # xy = TRUE includes coordinates
class(rp.df)
colnames(rp.df)  # See what the column names are

# Rename the raster value column to "Class_ID" which are raster values (from the attribute column)
colnames(rp.df)[4] <- "Class_ID"
colnames(rp.df)

# Create a SpatVector of points from the data frame
xy <- rp.df[, c("x", "y")]  # Subset coordinates
# Creates a SpatVector point object from coordinate data.
point.SPDF <- vect(xy, geom = c("x", "y"),
             crs = crs(rp))     # Automatically assigns the CRS from the original raster
point.SPDF

# Add a new column since there are no attributes as shown above in dimensions 
point.SPDF$Class_ID <- rp.df$Class_ID  # Assign attribute values





# -------------------------------------------------------------------
# Extract data from a remote sensed image 

# Plot natural colour and false urban colour
nlayers(selected_bands)
names(selected_bands)
ncell(selected_bands)

naturalColour <-ggRGB(selected_bands, r=3, g=2, b=1, stretch = "lin")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Natural Color\n (R= Red, G= Green, B= Blue)")
    
healthyVeg <- ggRGB(selected_bands, r=4, g=5, b=1, stretch = "lin")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Healthy egetation\n (R= SWIR2, G= SWIR1,  B= Red)")
    

    
grid.arrange(naturalColour, healthyVeg, nrow = 2)


# Extract raster values that intersect with each layer to points file 
# extract(): Works on SpatRaster objects with SpatVector points.
# bind = TRUE: Merges the extracted values with the point attribute table.
point.df <- extract(selected_bands, point.SPDF, bind = TRUE, method = "simple")

point.df

# Combine with data frame
point.mf<-cbind(rp.df,point.df)

# Datarame characteristics
head(point.mf)
class(point.mf)     
str(point.mf)                # structure of the dataframe
colnames(point.mf)            # columns available 
unique(point.df$Class_ID)        # Unique values 
sum(is.na(point.df))           # missing alues 


# Create a new column that add class type name 
point.mf$LandUseClass <- ifelse(point.mf$Class_ID == 1, 'forest' , 
                         ifelse(point.mf$Class_ID == 2, 'water' , 
                         ifelse(point.mf$Class_ID == 3, 'builtup', 
                         ifelse(point.mf$Class_ID == 4, 'vegetation',
                         ifelse(point.mf$Class_ID == 5, 'bare',
                                  NA
                              )))))

# Check new column and conirm no missing vlues 
colnames(point.mf)
unique(point.mf$LandUseClass)
str(point.mf)

# Remove duplicate column using dplyr
cleanData <- point.mf %>%
                  select(-5)
colnames(cleanData)

# Save as csv
write.csv(cleanData, paste0(dataFolder, '.\\pointsData.csv'))


# -------------------------------------------------------------------------
# Randomly split data into training(70%) and test(30%) datasets using CARET(
# short for _C_classification _A_nd _RE_regression _T_raining) package
library(caret)

# import csv
data <- read.csv("E:/DISK E PETER/flux files/New folder/NAIROBI_LANDSAT_MERCATOR/pointsData.csv")
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




