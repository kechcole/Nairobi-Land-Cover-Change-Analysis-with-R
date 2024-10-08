
library(conflicted)
# library(rgdal)       # spatial data processing
library(raster)      # raster processing
library(plyr)        # data manipulation 
library(dplyr)       # data manipulation 
library(RStoolbox)   # ploting spatial data 
library(RColorBrewer)# color
library(ggplot2)     # ploting
library(sp)          # spatial data
library(sf)          # spatial data

library(gridExtra)
library(tidyverse)   # data 


# Load data. Landsat data used for an area covering the greater Nairobi region downloaded from Google Earth Engine.
dataFolder <- "c:/Users/admin/Downloads/Nairobi Landsat data/"

# Load raster data 
landsat_24 <- stack(paste0(dataFolder, 'NAIROBI_L8_2023.tif'))
crs(landsat_24)

# Subset bands and plot 
selected_bands <- stack(landsat_24[[2:7]])
plot(selected_bands)

# Reproject raster 
# You can use an EPSG code (32337 for UTM Zone 37S, WGS 84 datum) or a proj4string
target_crs <- CRS("+init=EPSG:32337")

# Step 3: Reproject the raster
landsat_rpUTM <- projectRaster(selected_bands, crs = target_crs)
plot(landsat_rpUTM)


# Step 4. Load vector data and reproject to same CRS as raster 
# Load geopackage using sf package , layer is train_2023
polygon_layers <- st_layers(paste0(dataFolder, 'Nairobidata.gpkg'))
polygon_layers

# Load a single layer 
polygon1 <- st_read(paste0(dataFolder, 'Nairobidata.gpkg'), layer = 'dissolved')
crs(polygon1)

# Reproject to Pseudo Mercator, projected coordinate system that uses meters 
poly_rpUTM <- st_transform(polygon1, crs=32337) 
crs(poly_rpUTM)


#Step 4. 
# Choose attribute to be Plotted 
attribute <- poly_rpUTM$Class_Name 

# Plot
plot(poly_rpUTM["Class_Name"], col = terrain.colors(length(unique(attribute))), 
     main = "Digitized Areas Colored by Land Type.")

# Add a legend
legend("bottomright", legend = unique(attribute), 
        fill = terrain.colors(length(unique(attribute))), title = "Legend", cex = 0.8)




# ------------------------------------------------------
# Convert Polygon to raster

# Get extent from raster 
extent <- extent(selected_bands)
extent

# Get projection of polygon to be used in rasterisation 
crs = st_crs(poly_rpUTM)$proj4string
crs

# Create a blank raster (define resolution of 2.5m  and extent)
# Define the raster resolution (in the units of the coordinate system)
rast_template <- raster(extent, resolution=2.5, 
        crs = "+proj=utm +zone=37 +south +ellps=WGS72 +datum=WGS84 +units=m +no_defs")

# Assign the extent of reprojected polygon to raster template 
extent(rast_template) <- extent(poly_rpUTM)

# Rasterize the polygon based on an attribute
# Choose the attribute for rasterization MUST be a numeric function
rp <- rasterize(poly_rpUTM, rast_template, 'Class_ID')

# Plot 
plot(rp, main="Ground truth data")



