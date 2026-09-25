 '''
 Author : Collins Kechir
 Date Updated : 29/01/2024


LAND COVER CHANGE ANALYSIS IN THE GREATER NAROBI METROPOLITAN AREA. 

A landsat 8 image of the greater Nairobi region was acquired from google earth engine 
for the years 2013, 2018, and 2023. 


'''
 
 
 # Install libraries and load
# ---------------------------------------------------- 
# 1.Install required libraries 
# Write a function to check iof package is installed 
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
 
# Packages to be installed assigned to a vector
packages <- c("raster", "rasterVis", "sp", "rgdal", 
              "RStoolbox", "ggplot2", "raster")

# Install 
ipak((packages))
 
install.packages("rgdal")
 
# Load libraries 
library(raster)       # raster data
library(rasterVis)   # raster visualisation     
library(sp)          # spatial data processing          
# library(rgdal)       # spatial data processing      
library(RStoolbox)   # Image analysis
library(ggplot2)     # plotting
library(gridExtra)   # plot arrangement

 
 
 
# Load raster data and study properties 
# ---------------------------------------------------- 
# The image contains all the landsat 8 bands stacked in one image, 
# 1-14
landsat_24 <- stack("E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/NAIROBI_L8_2023.tif")

# Attributes of the whole image  
landsat_24
 
# Use a range to subset the bands then create a stack
# bands Blue, Green, Red, NIR, swir1, swir2
selected_bands <- stack(landsat_24[[2:7]])
 
# Attribute of subset bands 
selected_bands
 


 # Plot data  
# ---------------------------------------------------- 
# Plot data using base R 
plot(selected_bands)
 
# Plot histogram 
 hist(selected_bands)


 # Plot using ggplot 
 # 1.True colour image, RGB image. It replicates what the human eyes see using 432 bands 
 # Healthy vegetation is green, urban areas are white and grey while water is dark or blue 
 naturalColour <-ggRGB(landsat_24, r=4, g=3, b=2, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("Natural Color\n (R= Red, G= Green, B= Blue)")
naturalColour

# 2.Colour infrared image(543) is used for analysing vegetation. Vegetation have higher reflectance 
 # for NIR light, vegetative areas appear red while water which absorbs this band appears dark 
 # as urban areas are white. 
 falseColourVeg <- ggRGB(landsat_24, r=5, g=4, b=3, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("False Color Vegetation \n (R= NIR, G= Red,  B=Green)")
falseColourVeg

 
 
 # 3.SWIR is also false colour but used for urban area analysis using swir band combination, it displays vegetative areas as 
 # dark green while urban areas are whit/grey/purple  while soils have shades of brown, water and snow is black or blue shades. 
 # The composite is useful for detecting wildfires and calderas of volcanoes, as they are displayed in shades of red and yellow.
 falseColourUrb <- ggRGB(landsat_24, r=7, g=6, b=4, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("False Color Urban \n (R= SWIR2, G= SWIR1,  B= Red)")
falseColourUrb
 
 
 # Agriculture 
 # This band combination uses SWIR-1 (6), near-infrared (5), and blue (2). It’s commonly used for crop monitoring 
 # because of the use of short-wave and near-infrared. Healthy vegetation appears dark green. But bare earth 
 # has a magenta hue.
 agric <- ggRGB(landsat_24, r=6, g=5, b=2, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("agriculture \n (R= SWIR1, G= NIR,  B= Green)")
agric

 

#  Healthy vegetation
healthyVeg <- ggRGB(landsat_24, r=5, g=6, b=2, stretch = "lin")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Healthy Vegetation \n (R= NIR, G= SWIR1,  B= Green)")
healthyVeg
 
#  Land and water 
landWater <- ggRGB(landsat_24, r=5, g=6, b=4, stretch = "lin")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      ggtitle("Land and Water \n (R= NIR, G= SWIR1,  B= Red)")
 landWater
 
 
 # Plot all data together 
grid.arrange(naturalColour, falseColourVeg, falseColourUrb, agric, nrow = 1)

 
 
 
 '''              Pansharpening. 
It involves improving resolution of a multispectral-image using the panchromatic band. 
The panchromatic band has a high resolution if 15m as compared to other imgaes with 30m. 
The other bands can be "sharpened" using this band resulting to better quality images. 
this will use panSharpen() function in RSToolbox package on three channel RGB images.
'''
 # Load data 
landsat_2022 <- stack("E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/Landsat8_2022.tif") 
 panBand_2022 <- raster("E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/Landsat8_Panchromatic_2022.tif")

 landsat_2022
 panBand_2022

 # Stack band 432 and rename them, remember we dont have B1 thus use 321
 rgb_stack2022 <- stack(landsat_2022[[3]], landsat_2022[[2]], landsat_2022[[1]])
 names(rgb_stack2022) <- c('Red', 'Green', 'Blue')
 rgb_stack2022

 # Plot rgb 
 trueColour2022 <- ggRGB(rgb_stack2022, r=1, g=2, b=3, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("True Colour Image 2022 \n (R= red, G= green,  B= blue)")
trueColour2022
 
 # Pansharpen stack above 
 rgb_pan <- panSharpen(rgb_stack2022,    # Image to be pansharpend
                       panBand_2022,     # pansharpening band 
                       r = "layer.1",    # Layer refers to rgb stack 
                       g = "layer.2",
                       b = "layer.3", 
                       method = "pca",  # Use principle component analysis 
                       norm=TRUE)

# Plot pansharpened image 
pansharpened <- ggRGB(rgb_pan, stretch = "lin")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("Pansharpened 2022 Image \n Principal Component Analysis. ")
pansharpened 
 
 
 
#  REERENCES
#  1. Landsat visualisation - https://zia207.github.io/geospatial-r-github.io/landsat-8-image-processing.html