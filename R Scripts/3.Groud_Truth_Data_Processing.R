
# library(conflicted)
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
library(tidyverse)   # data MANIPULATION

update.packages("sf")
update.packages("tidyverse")
install.packages("tidyverse")


# Load data. Landsat data used for an area covering the greater Nairobi region downloaded from Google Earth Engine.
dataFolder <- "E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/"

# Load raster data , ind the coordinate reference system
landsat_24 <- stack(paste0(dataFolder, 'NAIROBI_L8_2023.tif'))
crs(landsat_24)


# Subset bands(BLUE, GREEN, RED, NIR,SWIR1,SWIR2) and plot 
selected_bands <- stack(landsat_24[[2:7]])
plot(selected_bands)

# Reproject raster From Geographic -WGS 84 to a Projected 
# You can use an EPSG code (32337 for UTM Zone 37S, WGS 84 datum) or a proj4string
target_crs <- CRS("+init=EPSG:32337")

# Step 3: Reproject the raster
landsat_rpUTM <- projectRaster(selected_bands, crs = target_crs)
# Check projection of new image 
crs(landsat_rpUTM)
# Plot reprojected image 
plot(landsat_rpUTM, main = " Raster Bands Reprojected To UTM Images.")



# Step 4. Load vector data and reproject to same CRS as raster
# The raster image was digitized to 5 classes, 1-orest, 2-water, 3-builtup, 4-vegetation and 5-bare lands
# The layer were then dissoled ased on their class id leading to 5 layers. 

# Load geopackage using sf package , layer is train_2023
polygon_layers <- st_layers(paste0(dataFolder, 'Nairobidata.gpkg'))
plot(polygon_layers)


# Load a single layer 
polygon1 <- st_read(paste0(dataFolder, 'Nairobidata.gpkg'), layer = 'dissolved')

# Metadata 
polygon1

# Projection 
crs(polygon1)

# Plot
plot(st_geometries(polygon1))

# Reproject to Pseudo Mercator, projected coordinate system that uses meters 
# Same projection as raster 
poly_rpUTM <- st_transform(polygon1, crs=32337) 
crs(poly_rpUTM)


#Step 5. 
# Choose attribute to be Plotted 
attribute <- poly_rpUTM$Class_Name 

# Plot
plot(poly_rpUTM["Class_Name"], col = terrain.colors(length(unique(attribute))), 
     main = "Digitized Areas Colored by Land Type.", legend=TRUE)

# Add a legend
legend("bottomright", legend = unique(attribute), 
        fill = terrain.colors(length(unique(attribute))), title = "Legend", cex = 0.8)




# ------------------------------------------------------
# Convert Polygon to raster

# Get extent from raster 
extent <- extent(selected_bands)
extent

# Polygon extent
extent <- extent(poly_rpUTM)
extent

# Get projection of polygon to be used in rasterisation 
crs = st_crs(poly_rpUTM)$proj4string
crs

# Create a blank raster (define resolution of 2.5m and extent)
# Define the raster resolution (in the units of the coordinate system)
rast_template <- raster(extent, resolution=2.5, 
        crs = "+proj=utm +zone=37 +south +ellps=WGS72 +datum=WGS84 +units=m +no_defs")

# Assign the extent of reprojected polygon to raster template 
extent(rast_template) <- extent(poly_rpUTM)

# Rasterize the polygon based on an attribute
# Choose the attribute for rasterization MUST be a numeric function
rp <- rasterize(poly_rpUTM, rast_template, 'Class_ID')

# Plot 
plot(rp, main="Rasterized Ground Truth Data")

# Convert raster to data.frame and rename colum to “layer”" to Class_ID
rp.df <- as.data.frame(rasterToPoints(rp))
 colnames(rp.df)[3] <- 'Class_ID'

#  Create a Spatial point Data frame
 xy <- rp.df[,c(1,2)]
 point.SPDF <- SpatialPointsDataFrame(coords = xy,
                                 data=rp.df,
                                 proj4string = CRS("+proj=utm +zone=37 +south +ellps=WGS72 +datum=WGS84 +units=m +no_defs"))



