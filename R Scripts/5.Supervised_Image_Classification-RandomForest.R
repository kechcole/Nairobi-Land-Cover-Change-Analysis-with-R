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
# -------------------------------------------------------------
# Train a Random Forest Classfier using caret package 
# -------------------------------------------------------------

# Install new packages
packages <- c("randomForest", "plyr", "RStoolbox", "RColorBrewer", "doParallel", "tmap", "ranger")

ipak <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg)) 
       install.packages(new.pkg, dependencies = TRUE)
   sapply(pkg, require, character.only = TRUE)
 }


ipak((packages))

library(caret)        # machine laerning
library(doParallel)   # Parallel processing
library(dplyr)        # data manipulation 
library(ranger)     # faster rf backend
library(terra)
library(ggplot2)      # ploting




source("config/config_local.R")

# Load data train and test data 
train.df <- read.csv(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR/trainingData.csv"))
test.df <- read.csv(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR/testData.csv"))



# Randomly select 10% of data making it lighter for the machine 
train.df <- train.df %>% sample_frac(.1)
test.df <- test.df %>% sample_frac(.1)

# view data properties , solumns and 
names(train.df)
str(train.df)



# ---- 1. Prep the data ONCE (not inside the formula) -------------------------
# Convert target to factor outside the formula — avoids re-evaluating
# the conversion on every CV fold.
train.df$LandUseClass <- as.factor(train.df$LandUseClass)

# Sanity check class balance (helps explain why Accuracy may be misleading)
print(table(train.df$LandUseClass))

# Optional but recommended: drop rows with NA in the predictors
train.df <- train.df[complete.cases(train.df[, c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7","LandUseClass")]), ]


# ---- 2. Training control ----------------------------------------------------
# - repeats = 1 while iterating (bump to 3–5 for the final model)
# - returnResamp = "final" is enough; "all" just stores extra per-fold stats
# - allowParallel only matters if a backend is registered (see bottom)
myControl <- trainControl(
  method        = "repeatedcv",
  number        = 3,
  repeats       = 1,
  returnResamp  = "final",
  allowParallel = TRUE,
  savePredictions = "final",
  classProbs      = TRUE     # handy for confusion matrix / ROC later
)

# ---- 3. Small, explicit tuning grid -----------------------------------------
# mtry = number of predictors sampled at each split.
# With 5 predictors (SR_B2, B3, B4, B5, B6, B7), sensible values are 2–3.
tuneGrid <- expand.grid(mtry = c(2, 3))

# ---- 4. Train the model -----------------------------------------------------
# Notes:
#   * method = "ranger"  -> much faster than "rf", same algorithm family
#   * preProc removed    -> random forests do NOT need center/scale
#   * ntree = 300        -> lighter than the default 500; tune up if needed
#   * importance = "impurity" -> lets you inspect variable importance
fit.rf <- train(
  LandUseClass ~ SR_B2 + SR_B3 + SR_B4 + SR_B5 + SR_B6 + SR_B7,
  data       = train.df,
  method     = "ranger",
  metric     = "Accuracy",
  trControl  = myControl,
#   tuneGrid   = tuneGrid,
  num.trees  = 300,
  importance = "impurity",
  verbose    = FALSE
)


# ---- 5. Inspect results -----------------------------------------------------
print(fit.rf)
print(fit.rf$bestTune)
print(fit.rf$results)


# Confusion matrix on the held-out CV predictions
confusionMatrix(fit.rf)


# Variable importance
varImp(fit.rf)
plot(varImp(fit.rf))




# ------------------------------------------------------------------------------------------------
# 6. Evaluate the model in the training dataset by predicting land use class on train data 
# ------------------------------------------------------------------------------------------------
#  using the model and computing the confusion matrix(compares actual values and predicted ones

# 6.1 Sanity check to ensure test data matches training data 
# Same factor levels for the target, same predictor columns
test.df$LandUseClass <- factor(test.df$LandUseClass,
                               levels = levels(train.df$LandUseClass))
test.df$LandUseClass

# Confirm predictor names match exactly what the model expects
stopifnot(all(c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7") %in% names(test.df)))

# Drop rows with NA in predictors (ranger can't predict them)
test.df <- test.df[complete.cases(
  test.df[, c("SR_B2","SR_B3","SR_B4","SR_B5","SR_B6","SR_B7","LandUseClass")]
), ]


# 6.2 Fit the model on the new dataset 
pred_class <- predict(fit.rf, newdata = test.df)          # class labels
pred_prob  <- predict(fit.rf, newdata = test.df, type = "prob")  # class probs


# 6.3 Confusion matrix + metrics 
cm <- confusionMatrix(pred_class, test.df$LandUseClass)
print(cm)


# Per-class accuracy (a.k.a. "user's accuracy" / precision per class)
print(cm$byClass)

# Overall numbers pulled out
cat("Overall accuracy :", round(cm$overall["Accuracy"], 4), "\n")
cat("Kappa            :", round(cm$overall["Kappa"], 4), "\n")
cat("95% CI (accuracy):", round(cm$overall["AccuracyLower"], 4),
    "-", round(cm$overall["AccuracyUpper"], 4), "\n")



# 6.4. Compare CV accuracy vs. test accuracy 


# Large drop here = overfitting or (more likely for Landsat data) spatial
# autocorrelation inflating the CV estimate.
cat("CV accuracy (train)  :", round(max(fit.rf$results$Accuracy), 4), "\n")
cat("Test accuracy        :", round(cm$overall["Accuracy"], 4), "\n")



# 6.5 Important metrics 
# Overall accuracy is the headline number, but reviewers and stakeholders usually want these too:
# if one land-cover class dominates (e.g., 80% "vegetation"), a model can hit 
# 80% overall accuracy by predicting that one class for everything. 
# Macro F1 and per-class recall expose that.

# Per-class precision, recall, F1 (caret gives these in cm$byClass)
by_class <- as.data.frame(cm$byClass)
by_class$Class <- rownames(by_class)
print(by_class[, c("Class", "Precision", "Recall", "F1",
                   "Balanced Accuracy")])

# Macro-averaged F1 (treats each class equally, regardless of size)
cat("Macro F1:", round(mean(by_class$F1, na.rm = TRUE), 4), "\n")


# 6.6 Visualise confusion matrix 
library(reshape2)

cm_df <- as.data.frame(cm$table)
cm_df <- melt(cm$table)
cm_df$prop <- cm_df$value / ave(cm_df$value, cm_df$Prediction, FUN = sum)

ggplot(cm_df, aes(x = Reference, y = Prediction, fill = prop)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white", size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion matrix (test set)", x = "Actual", y = "Predicted") +
  theme_minimal()






# ------------------------------------------------------------------------------------------------
# 7. Predict on another image of a different year , 2018, but similar landsat series 
# --------------------------------------------------------------------------------------
# Have similar acqusition geometry, scalling and collected at almost the same time of the year(same processing pipeline). 
# Though different years could have different phenology, sun angles and collection residuals. 

# Read raster data 
landsat_2018 <- rast(file.path(DATA_ROOT, "NAIROBI_LANDSAT_MERCATOR/Landsat_8_2018_Proj.tif"))
landsat_2018

# Check whether band names match and crs , must be similar with training datset 
names(landsat_2018)
crs(landsat_2018)

# Select bands 
new_img <- landsat_2018[[c("SR_B2", "SR_B3", "SR_B4", "SR_B5", "SR_B6", "SR_B7")]]
new_img


# Predict 
# Key point: terra::predict() streams the raster in chunks — it does not load the whole image into RAM. 
# This is exactly what you want given the 98% memory situation earlier
pred_raster <- predict(new_img, fit.rf, na.rm = TRUE, type = "raw")


# Plot predicted raster of 2018 landsa image 
# Create colour codes 
lvls <- levels(train.df$LandUseClass)
print(lvls)


# display names, in the SAME order as lvls
class_labels <- c("bare", "builtup", "forest", "vegetation", "water")  # <-- match lvls order
class_colors <- c("#DAA520",   "#8B0000",  "#056d05", "#82eb82",  "#1E90FF") # <-- match lvls order


# Assign a colour table to the categorical raster
coltab(pred_raster) <- data.frame(
  value = 1:length(class_colors),
  col   = class_colors
)

plot(pred_raster,
     main = "Predicted Land Use 2018",
     col  = class_colors,      # used when coltab isn't set
     type = "classes",
     mar  = c(3, 3, 2, 8))     # extra right margin for the legend




# CLEAN THE ENTIRE ENVIROMENT 
rm(list = ls())















