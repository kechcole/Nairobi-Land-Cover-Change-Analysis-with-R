
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

# Load raster data , find the coordinate reference system
landsat_23 <- stack(paste0(dataFolder, 'Landsat_8_2023_Proj.tif'))
crs(landsat_23)       # A mecartor projection (3395)

class(landsat_23)

# Subset bands(BLUE, GREEN, RED, NIR,SWIR1,SWIR2) and plot 
selected_bands <- stack(landsat_23[[2:7]])
plot(selected_bands)




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



# ----METHOD 1 : USING raster lirary, will be deprecated ----------------
# Create a blank raster (define resolution of 20m and extent)
# Define the raster resolution (in the units of the coordinate system)
rast_template <- raster(extent, resolution=20, 
        crs = "+proj=utm +zone=37 +south +ellps=WGS72 +units=m +no_defs")

# Assign the extent of reprojected polygon to raster template 
extent(rast_template) <- extent(poly_rpUTM)

# Rasterize the polygon based on an attribute
# Choose the attribute for rasterization MUST be a numeric function
rp <- rasterize(poly_rpUTM, rast_template, 'Class_ID')

# Plot 
plot(rp, main="Rasterized Ground Truth Data")





# -------- METHOD 2 : USING Tera package , modern  -----------
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



# Convert raster to data.frame and rename colum to “layer”" to Class_ID
rp.df <- as.data.frame(rasterToPoints(rp))
rp.df
 colnames(rp.df)[3] <- 'Class_ID'

#  Create a Spatial point Data frame
 xy <- rp.df[,c(1,2)]          # Subset rp dataframe and fetch columns containing  xy values
 point.SPDF <- SpatialPointsDataFrame(coords = xy,
                                 data=rp.df,
                                 proj4string = CRS("+proj=utm +zone=37 +south +ellps=WGS72 +datum=WGS84 +units=m +no_defs"))


# -------------------------------------------------------------------
# Extract data from a remote sensed image 

# Plot natural colour and false urban colour
nlayers(landsat_rpUTM)
names(landsat_rpUTM)
ncell(landsat_rpUTM)

naturalColour <-ggRGB(landsat_rpUTM, r=3, g=2, b=1, stretch = "lin")+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        ggtitle("Natural Color\n (R= Red, G= Green, B= Blue)")

falseColourUrb <- ggRGB(landsat_23, r=6, g=5, b=3, stretch = "lin")+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        ggtitle("False Color Urban \n (R= SWIR2, G= SWIR1,  B= Red)")


healthyVeg <- ggRGB(landsat_23, r=5, g=6, b=2, stretch = "lin")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Healthy Vegetation \n (R= NIR, G= SWIR1,  B= Green)")

grid.arrange(naturalColour, healthyVeg, falseColourUrb, nrow = 2)

# Extract raster values that intersect with each layer to points file 
point.df <- raster::extract(landsat_rpUTM, point.SPDF, df=TRUE, method='simple')


point.df
