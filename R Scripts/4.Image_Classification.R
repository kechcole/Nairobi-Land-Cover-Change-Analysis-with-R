'''
                    IMAGE CLASSIFICATION. 
This is the process of assingning new land cover classes to an image resulting to thematic maps that 
show land use maps. This activity is based on the reflactance value of different features, i.e a built up area
has different value a s compared to vegetative areas. By assigning classes, the image acquires new 
labels / themes. Classification can be based on spectral values, spatial(textural and proximity) or 
temporal imformation. The two kost commonly used methods are supervised and unsupervised, and have been in use 
for a long time. Imprtant to note, they are pixel based and dependent on spectaral information, DN values. 

          Procedure of classification. 
1. Image Preprocessing. Includes corrections i.e radiometric(missmatch between energy being emmited and one received), 
   atmospheric(radiance scattering/absorption), topographic, image enhancement, or sometimes clustering analysis, 
   geometric(incorrect coordinates). 
2. Design a classification scheme. Based of ground truth from field survey or other ancilliary data, infor 
   about ground features such as agriculture, built areas is determined. 
3. Create training areas. Representative areas of the image is created either through an earlier classification
   algorith(unsupervised) or manually developing training signatures through digitization. 
4. Run a classification algorithm. 
5. Post processing. Techniques aimed at improving the accuracy of an image thus raising value of infor generated from 
   an image. This includes image enhancement, orthorectification(horizontal accuracy using GCPs), spectral analysis to reveal hidden 
   information.  
6. Accuracy assesment. Checks the accuracy of your model by comparing ground truth and values generated.

          Image Classification Techniques. 
Remote sensing has a wide array of classifications groups based on characteristic of underlying algorithm. They include 
    a) Parametric and non-parametric techniques - one uses assumptions on data while the other ignores. 
    b) Hard or soft. Hard technique is where each pixel is assigned a class leading to data loss in cases where there are 
       a pixel can represent more than one class while the later allows pixels to have more than one classes, it assumes the 
       real world pattern where features are heterogenious. In soft a pixel is assigned a portion of the cover type it represents.
    c) Supervised and unsupervised classification. 
    d) Hybrid technique combines multiple other techniques by leveraging on AI and expert systems to improve on their deficiencies. 

I will delve into two major ones : supervided and supervised since they are commonly used and have been explored widely. 


        Unsupervised classification. 
This algorithm does not have any prior knowledge of the data and uses pixel properties such as simmilaries or differences in radiance value
to design clusters that minimises feature variation and maximises class separation. One huge advantage over supervised classification is 
that the algorith is able to reveal hidden patterns that human capacity easily ignores. Conversly, is fall short of meeting user 
as in may require expert knowledge to interpret. 


'''
# ---------------------------------------------------------------------------------
#                    Section 1 : UNSUPERVISED CLASSIFICATION.
#----------------------------------------------------------------------------------

# Load Libraries 
library(raster)      # raster data
library(rasterVis)  # raster visualisation  
library(sp)         # Spatial data processing           
# library(rgdal)      # Spatial data processing
library(RStoolbox)  # Image analysis
library(ggplot2)    # ploting


# Load multiband raster data 
dataFolder <- "E:/DISK E PETER/flux files/New folder/Nairobi Landsat data/"
landsat_2023 <- stack(paste0(dataFolder, 'NAIROBI_L8_2023.tif'))

# Confirm multiband 
landsat_2023

# K-means Clustering using 8 classes. 

# Set seed for easy duplication
set.seed(4)

# Run algorithm 
unsupClass_08 <- unsuperClass(landsat_2023,      # raster image
                        nSamples = 100,   # Number of random samples to draw to fit cluster map 
                        nClasses= 8,      # Number of classes  
                        nStarts = 5)      # Number of random starts for kmeans algorithm

# Plot map 
colours <- colorRampPalette(c("white", "dark grey", "darkgreen","green", "light blue", "yellow", "red", "blue", "orange"))
spplot(unsupClass_08$map,    # Object 
      main="Unsupervised K-Classificantion with 8 Classes" ,   # Tilte  
      # Legend apperance and behaviour 
      colorkey = list(space="right",    # Position 
                      tick.number=1,height=1, width=1,  # Size 
                      # cex controls text label size, 
                      labels = list(at = 1:8, labels = paste("Class", 1:8), cex = 1.2)),
      col.regions=terrain.colors(8), 
      cut=7)  # 7 intervals has 8 classes 

# Save the image on disk 
writeRaster(unsupClass_08$map, filename=paste0(dataFolder, 'Unsup_Kmeans.tif'), format="GTiff", overwrite=TRUE)

print(paste0(dataFolder, 'Unsup_Kmeans.tif'))




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
packages <- c("randomForest", "plyr", "RStoolbox", "RColorBrewer", "doParallel")

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
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # ploting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # ploting
library(sp)           # spatial data
library(doParallel)   # Parallel processing


















