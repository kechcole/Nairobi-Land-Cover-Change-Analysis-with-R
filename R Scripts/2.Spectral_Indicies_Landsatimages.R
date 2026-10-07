'''
              SPECTRAL INDICIES. 
Spectral indicies compares the spectral reflectance from two or more wavelengths showing relative 
abundance of a feature of interest. This analysis is commonly used to study vegetation, burnt areas, 
built-up areas, water and geological features.  
Before processing data, some pre-processing on raw raster data must be performed, this includes : 
      a) Clipping all bands to area of study 
      b) Convert radiance values to reflactance values, this normalised pixel values 

By extracting information about various features on the earth surfaces, we get to enhance our knowledge and make 
better decisions regarding the enviroment. By tracking changes in plant health we can easily predict output of farmers. 
There are several indicies but more traditional ones include 
            * NDVI - Normalised Difference Vegetation Index is derived from NIR and Red bands. 
                   - Healthy vegetation absorbs visible light but reflects near infrared light, but the inverse is true 
                   - for unhealthy vegetation. 
                                NDVI = (NIR - R)/(NIR + R)
                   - A value of 1 means the vegeation is extremely green, 0 means no vegetation at all, while negative 
                     value indicates unhealthy cover.  
            
            * NDWI - This numerical indicator is derived from nir and short wave infrared spectral bands to identify water content in 
                     vegetation. This can be used in monitoring crop health, discerning inland water from sea water etc.
                     Water strongly absorbs visible to infrared range of wavelenths.  
                                NDWI = (NIR - SWIR1) / (NIR + SWIR1) 
            
            * EVI2 - A two band Enhanced Vegetation Index(EVI2) and Enhanced vegetation Index(EVI) are used to counter the shortcommings of NDVI,
                     because they increase visibility for areas with dense vegetation. This makes them well suited for studying palnt phenology. 
                     The former has been recently introduced as a proxy for phenology, quantity and activity, it has the advantage of clearly discerning soil.
                                EVI2 = G * ( (NIR - RED) / (NIR + 2.4*RED + 1) ) 

            * SAVI - Soil Adjusted Vegetation Index is similar to NDVI bare soil values are suppressed through an adjustment factor(L). 
                     L is a function of vegetation density, in places with low vegetation cover, a value of 0.5 is recomended. 
                     This index is well suited for areas with relative sparse vegetation and soil is more pronounced. 
                              SAVI = ((L + 1) * (NIR - RED)) / (NIR + RED + L)

            * IBI - Index Based Built-up Index is a new indicator used to extract built-up enviroment features. Its a bit different from other indocators because it uses existing(thematic) 
                    index-derived bands to construct and index rather than adopting the original satellite bands. For exaple it makes use of 
                    other known MNDWI, SAVI, and NDBI which map major urban components of vegetation, water and built-up land.
                    Its advantageous because it suprises banckground noise while enhancing built-up areas.  
                    Higher values close to 1 show density of built-up areas while lower values close to -1 is for low to no built areas orwater/vegetation. 
                                       (2 * SWIR1/(SWIR1 + NIR) - [ NIR/(NIR + RED) + GREEN/(GREEN + RED)] )
                            IBI =       --------------------------------------------------------------------
                                       (2 * SWIR1/(SWIR1 + NIR) + [ NIR/(NIR + RED) + GREEN/(GREEN + RED)] )



'''

# ---------------------------------------------------------------------
# Install and load libraries and data 
# ---------------------------------------------------------------------
# install ggplot dependancy and reinstall it 
install.packages("cli")
install.packages("ggplot2")

# check version 
packageVersion("cli")
packageVersion("ggplot2")

# load libraries 
library(terra)       # raster data   
library(ggplot2)    # ploting


# Load data. Landsat data used for an area covering the greater Nairobi region downloaded from Google Earth Engine.
source("config/config_local.R")
 
landsat_2023 <- rast(file.path(DATA_ROOT, "NAIROBI_L8_2023.tif"))



# select individual bands
blue <- landsat_2023[[2]]
red <- landsat_2023[[4]]
green <- landsat_2023[[3]]
nir <- landsat_2023[[5]]
swir1 <- landsat_2023[[6]]



#-------------------------------------------------------------------------------
# Calculate NDVI.
#-------------------------------------------------------------------------------
ndvi = (nir - red) / (nir + red)

# Plot data using terra's plot function 
plot(ndvi,    # spatial Raster object 

        # colour pellets for ploting vegetation : red - low vegetation, yellow - intermediate veg, 
        # green - dense and heavy vegetation , 100 is for more colors generating a smoother map
     col = colorRampPalette(c("red", "yellow", "green", "green4"))(100),
     main = "Normalised Difference Vegetation Index.",
     axes = FALSE,
     plg = list(title = "NDVI", cex = 0.8))


#--------------------------------------------------------------------------
# band distribution for NDVI 
#--------------------------------------------------------------------------

# Convert raster to data frame
ndvi_df <- as.data.frame(ndvi, xy = FALSE, na.rm = TRUE)
names(ndvi_df)

ggplot(ndvi_df, aes(x = SR_B5)) +
  geom_histogram(bins = 60, fill = "green4", color = "white") +
  labs(title = "NDVI Distribution", x = "NDVI", y = "Frequency") +
  theme_minimal()




#-------------------------------------------------------------------------------
#   Two bands Enhanced Vegetation Index.
#-------------------------------------------------------------------------------
G = 2.5    # Gain values 
evi2 = G * (nir - red) / (nir + 2.4 * red + 1)

plot(evi2,
     col = colorRampPalette(c("red", "yellow", "green", "blue"))(100),
     main = "2-Bands Enhanced Vegetation Index(EVI2).",
     axes = FALSE,
     plg = list(title = "EVI2", cex = 0.8))



#------------------------------------------------------------------
#  NDWI 
#--------------------------------------------------------------------
ndwi = (nir - swir1) / (nir + swir1)

plot(ndwi,
     col = colorRampPalette(c("blue", "green", "yellow", "red"))(100),
     main = "Normalised Difference Water Index.",
     axes = FALSE,
     plg = list(title = "Ndwi", cex = 0.8))


#--------------------------------------------------------------------------
#        SAVI 
# -------------------------------------------------------------------------------
L = 0.5
savi = (L + 1)*(nir - red) / (nir + red + L)

plot(savi,
     col = colorRampPalette(c("red", "yellow", "green", "green4"))(100),
     main = "Soil Adjusted Vegetation Index.",
     axes = FALSE,
     plg = list(title = "SAVI", cex = 0.8))




#--------------------------------------------------------------------------
#         IBI 
# -------------------------------------------------------------------------------
ibi <- ( (2*swir1 / (swir1 + nir)) - (nir/(nir + red) + green/(green + red)) ) /
       ( (2*swir1 / (swir1 + nir)) + (nir/(nir + red) + green/(green + red)) )

plot(ibi,
     col = colorRampPalette(c("red", "yellow", "green", "blue"))(100),
     main = "2-Band Enhanced Vegetation Index(EVI1).",
     axes = FALSE,
     plg = list(title = "EVI2", cex = 0.8))



#--------------------------------------------------------------------------
#         NBI - normalised difference build-up index  
# -------------------------------------------------------------------------------
ndbi = (swir1 - nir) / (swir1 + nir)

plot(ndbi,
     col = colorRampPalette(c("red", "yellow", "blue", "blue4"))(100),
     main = "Normalised Difference Built-up Index.",
     axes = FALSE,
     plg = list(title = "NDBI", cex = 0.8))



#--------------------------------------------------------------------------
# PLOT THE SPECTRAL PROFILE OF POINT ON THE IMAGE
#--------------------------------------------------------------------------
# Extract band values at a single pixel (row, col)
vals <- extract(landsat_2023, cellFromRowCol(landsat_2023, 500, 500))
vals <- as.numeric(vals[1, -1])   # drop ID column
vals

wavelengths <- c(443, 482, 561, 655, 865, 1609)  # Landsat 8/9 example
bands <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

length(bands)         # should equal number of bands
length(wavelengths)   # must match bands
length(vals)          # must match bands
str(vals)             # check if it's a data.frame with extra columns


df <- data.frame(Band = factor(bands, levels = bands),
                 Wavelength = wavelengths,
                 Reflectance = vals)

ggplot(df, aes(x = Wavelength, y = Reflectance)) +
  geom_line(color = "green4", linewidth = 1) +
  geom_point(color = "green4", size = 2) +
  labs(title = "Spectral Profile of a Single Pixel",
       x = "Wavelength (nm)", y = "Reflectance") +
  theme_minimal()





# Matters to handle 
# 1. Band value distribution plot, outliers how to elliminate, spectral profile

# REFRENCES.
# 1. Tutorial link - https://zia207.github.io/geospatial-r-github.io/spectral-indices.html
