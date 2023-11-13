## ---------------------------
##
## Script name: fl_veg_classification
##
## Purpose of script: Use band indices, unsupervised, and supervised 
##  classification models to determine the amount of emergent vegetation 
##  around Frank Lake
##
## Author: Samuel Woodman
##
## Date Created: 2023-06-20
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

## Raster 
fl_multi_spec <- rast(here("data/raw/fl_multi_spec.tif"))
fl_indices <- rast(here("data/processed/fl_indices.tif"))

## Vector
### outer boundary of vegetation delineated using Google Earth imagery
fl_b1_shoreline_boundary <- vect(here("data/raw/fl_b1_shoreline_boundary.gpkg"))
fl_b2_shoreline_boundary <- vect(here("data/raw/fl_b2_shoreline_boundary.gpkg"))
fl_b3_shoreline_boundary <- vect(here("data/raw/fl_b3_shoreline_boundary.gpkg"))

### vegetation zone using the Google earth delineated shoreline boundary and edge
fl_b1_veg_zone <- vect(here("data/processed/fl_b1_veg_zone.gpkg"))
fl_b2_veg_zone <- vect(here("data/processed/fl_b2_veg_zone.gpkg"))

### training polygons identifying water, veg, algae, bare ground, and structures
fl_b1_train_poly <- vect(here("data/raw/fl_b1_training_data.gpkg"))


# Extract areas of interest -----------------------------------------------

## combine raw bands and indices to limited repeated code
fl_rast <- c(fl_multi_spec, fl_indices) 

## Basin 1
### Full area
#### google earth 
fl_rast_b1_full <- crop(fl_rast, fl_b1_shoreline_boundary)
plot(fl_rast_b1_full)
plot(fl_rast_b1_full[[13]])

### Crop/Rangeland removed
#### google earth 
fl_rast_b1 <- mask(crop(fl_rast, fl_b1_shoreline_boundary),
                        fl_b1_shoreline_boundary) 
plot(fl_rast_b1)
plot(fl_rast_b1[[13]])

### Vegetation zone
#### google veg zone
fl_b1_veg_zone <- mask(crop(fl_rast, fl_b1_veg_zone), fl_b1_veg_zone)
plot(fl_b1_veg_zone)
plot(fl_b1_veg_zone[[13]])

## Basin 2
### Full area
#### google earth 
fl_rast_b2_full <- crop(fl_rast, fl_b2_shoreline_boundary)
plot(fl_rast_b2_full)
plot(fl_rast_b2_full[[13]])

### Crop/Rangeland removed
#### google earth 
fl_rast_b2 <- mask(crop(fl_rast, fl_b2_shoreline_boundary),
                   fl_b2_shoreline_boundary) 
plot(fl_rast_b2)
plot(fl_rast_b2[[13]])

### Vegetation zone
#### google veg zone
fl_b2_veg_zone <- mask(crop(fl_rast, fl_b2_veg_zone), fl_b2_veg_zone)
plot(fl_b2_veg_zone)
plot(fl_b2_veg_zone[[13]])

## Basin 3
### Full area
#### google earth 
fl_rast_b3_full <- crop(fl_rast, fl_b3_shoreline_boundary)
plot(fl_rast_b3_full)
plot(fl_rast_b3_full[[13]])

### Crop/Rangeland removed
#### google earth 
fl_rast_b3 <- mask(crop(fl_rast, fl_b3_shoreline_boundary),
                   fl_b3_shoreline_boundary) 
plot(fl_rast_b3)
plot(fl_rast_b3[[13]])


# Visualise ---------------------------------------------------------------

plotRGB(crop(stretch(fl_indices[[c("NDVI1", "EVI", "NDWI")]]), fl_b1_shoreline_boundary))
plotRGB(crop(stretch(fl_indices[[c("NDVI1", "NHFD", "NDWI")]]), fl_b1_shoreline_boundary))
plotRGB(crop(stretch(fl_indices[[c("EVI", "Chlr_redEdge", "NDWI")]]), fl_b1_shoreline_boundary))

plotRGB(crop(stretch(fl_indices[[c("NDVI1", "EVI", "NDWI")]]), fl_b2_shoreline_boundary))
plotRGB(crop(stretch(fl_indices[[c("NDVI1", "NHFD", "NDWI")]]), fl_b2_shoreline_boundary))
plotRGB(crop(stretch(fl_indices[[c("EVI", "Chlr_redEdge", "NDWI")]]), fl_b2_shoreline_boundary))

plotRGB(crop(stretch(fl_indices[[c("NDVI1", "EVI", "NDWI")]]), fl_b3_shoreline_boundary))
plotRGB(crop(stretch(fl_indices[[c("NDVI1", "NHFD", "NDWI")]]), fl_b3_shoreline_boundary))
plotRGB(crop(stretch(fl_indices[[c("EVI", "Chlr_redEdge", "NDWI")]]), fl_b3_shoreline_boundary))

# Model -------------------------------------------------------------------

## Unsupervised classification ----
### PCA ----
library(stats)
#### Using either the raw spectral data or the calculated indices, the data is 
#### decomposed into several principal components (n = number of input 
#### variables). Each Axis can then be manually inspected to see which 
#### variables they are most associated with. 

#### Run PCA
names(fl_rast_b1_full)
##### Raw bands 
pca_ms <- prcomp(fl_rast_b1_full[[c(1:8)]])
summary(pca_ms)
pca_ms_pred <- predict(fl_rast_b1_full, pca_ms)
plot(pca_ms_pred)
plotRGB(stretch(pca_ms_pred))

##### RGB
pca_rgb <- prcomp(fl_rast_b1_full[[c("Red", "Green", "Blue")]])
summary(pca_rgb)
pca_rgb_pred <- predict(fl_rast_b1_full, pca_rgb)
plot(pca_rgb_pred)
plotRGB(stretch(pca_rgb_pred))

##### Indices 
pca_ind <- prcomp(scale(fl_rast_b1_full[[c(9:20)]]))
summary(pca_ind)
pca_ind_pred <- predict(fl_rast_b1_full, pca_ind)
plot(pca_ind_pred)
plotRGB(stretch(pca_ind_pred))

##### Curated Indices 1
pca_cur_ind1 <- prcomp(scale(fl_rast_b1_full[[c("NDVI1", "Chlr_g", "Chlr_redEdge", 
                                        "EVI", "NDWI")]]))
summary(pca_cur_ind1)
pca_cur_ind_pred1 <- predict(fl_rast_b1_full, pca_cur_ind1)
plot(pca_cur_ind_pred1[[1]])
plotRGB(stretch(pca_cur_ind_pred1))

##### Curated Indices 2
pca_cur_ind2 <- prcomp(scale(fl_rast_b1_full[[c("Chlr_g", "Chlr_redEdge", "NDWI")]]))
summary(pca_cur_ind2)
pca_cur_ind_pred2 <- predict(fl_rast_b1_full, pca_cur_ind2)
plot(pca_cur_ind_pred2[[1]])
plotRGB(stretch(pca_cur_ind_pred2))

writeRaster(pca_ind_pred, here("tmp/ind_pca.tif"))


#### Outcome ----
#### PCA on the raw bands and all indices appear less able to group pixels 
#### according to land cover. This is likely due to the large number of vectors
#### used. Selecting specific vectors, such as RGB or indices that appear 
#### visually important (NDVI, EVI, Chlr, NDWI) better highlight vegetation, 
#### algae, and water. 
#### 
#### Clustering may be needed to categories pca pixel values to discrete land
#### cover types. PCA also does not allow for NAs which means the surrounding
#### crop/rangeland are include. 

### K-mean clustering ----
library(cluster)
#### Data can be clustered based on single bands or raster stacks. For our 
#### purpose, multiple bands will be better for clustering since algal blooms
#### appear similarly to emergent vegetation for most greenness based metrics. 
#### https://gis.stackexchange.com/questions/123639/unsupervised-classification-with-kmeans-in-r

#### raw bands
##### convert raster to df without removing NAs
fl_ms_b1_df <- as.data.frame(fl_b1_veg_zone[[c(1:8)]], na.rm = F)
##### create an index of cell values 
idx_ms_b1 <- as.data.frame(fl_b1_veg_zone[[c(1:8)]], na.rm = F, cells = T) %>% 
  pull(cell)
##### remove all NAs from idex
idx_ms_b1 <- idx_ms_b1[-unique(which(is.na(fl_ms_b1_df), arr.ind=TRUE)[,1])]  
##### cluster, remove NAs and scale
set.seed(1284)
fl_clus_ms_b1 <- cluster::clara(na.omit(scale(fl_ms_b1_df)), k=4)
##### plot
clust_ms_b1 <- fl_b1_veg_zone[[1]] 
clust_ms_b1[] <- NA
clust_ms_b1[idx_ms_b1] <- fl_clus_ms_b1$clustering
plot(clust_ms_b1) 
writeRaster(clust_ms_b1, here("tmp/clust_ms_b1.tif"), overwrite = T)

#### indices
##### convert raster to df without removing NAs
fl_ind_b1_df <- as.data.frame(fl_b1_veg_zone[[c(9:20)]], na.rm = F)
##### create an index of cell values 
idx_ind_b1 <- as.data.frame(fl_b1_veg_zone[[c(9:20)]], na.rm = F, cells = T) %>% 
  pull(cell)
##### remove all NAs from idex
idx_ind_b1 <- idx_ind_b1[-unique(which(is.na(fl_ind_b1_df), arr.ind=TRUE)[,1])]  
##### cluster, remove NAs and scale
set.seed(1284)
fl_clus_ind_b1 <- cluster::clara(na.omit(scale(fl_ind_b1_df)), k=4)
##### plot
clust_ind_b1 <- fl_b1_veg_zone[[1]] 
clust_ind_b1[] <- NA
clust_ind_b1[idx_ms_b1] <- fl_clus_ind_b1$clustering
plot(clust_ind_b1) 
writeRaster(clust_ind_b1, here("tmp/clust_ind_b1.tif"), overwrite = T)


#### all data
##### basin 1
##### convert raster to df without removing NAs
fl_all_b1_df <- as.data.frame(fl_b1_veg_zone, na.rm = F)
##### create an allex of cell values 
idx_all_b1 <- as.data.frame(fl_b1_veg_zone, na.rm = F, cells = T) %>% 
  pull(cell)
##### remove all NAs from idex
idx_all_b1 <- idx_all_b1[-unique(which(is.na(fl_all_b1_df), arr.ind=TRUE)[,1])]  
##### cluster, remove NAs and scale
set.seed(1284)
fl_clus_all_b1 <- cluster::clara(na.omit(scale(fl_all_b1_df)), k=3)
##### plot
clust_all_b1 <- fl_b1_veg_zone[[1]] 
clust_all_b1[] <- NA
clust_all_b1[idx_all_b1] <- fl_clus_all_b1$clustering
plot(clust_all_b1) 
writeRaster(clust_all_b1, here("output/clust_all_b1_k3.tif"), overwrite = T)

aoi_poly <- as.polygons(aoi)

ggplot() + 
  geom_spatraster_rgb(data = crop(fl_multi_spec[[c("Red", "Green", "Blue")]], aoi_poly)) +
  geom_spatraster(data = as.factor(crop(clust_all_b1, aoi_poly)), 
                  alpha = 0.05) + 
  scale_fill_manual(values = c("red", "green", "blue", "yellow", "orange"),
                    na.translate = F) + 
  theme_classic()

##### basin 2
##### convert raster to df without removing NAs
fl_all_b2_df <- as.data.frame(fl_b2_veg_zone, na.rm = F)
##### create an allex of cell values 
idx_all_b2 <- as.data.frame(fl_b2_veg_zone, na.rm = F, cells = T) %>% 
  pull(cell)
##### remove all NAs from idex
idx_all_b2 <- idx_all_b2[-unique(which(is.na(fl_all_b2_df), arr.ind=TRUE)[,1])]  
##### cluster, remove NAs and scale
set.seed(1284)
fl_clus_all_b2 <- cluster::clara(na.omit(scale(fl_all_b2_df)), k=3)
##### plot
clust_all_b2 <- fl_b2_veg_zone[[1]] 
clust_all_b2[] <- NA
clust_all_b2[idx_all_b2] <- fl_clus_all_b2$clustering
plot(clust_all_b2) 
writeRaster(clust_all_b2, here("output/clust_all_b2_k3.tif"), overwrite = T)

##### basin 3
##### convert raster to df without removing NAs
fl_all_b3_df <- as.data.frame(fl_rast_b3, na.rm = F)
##### create an allex of cell values 
idx_all_b3 <- as.data.frame(fl_rast_b3, na.rm = F, cells = T) %>% 
  pull(cell)
##### remove all NAs from idex
idx_all_b3 <- idx_all_b3[-unique(which(is.na(fl_all_b3_df), arr.ind=TRUE)[,1])]  
##### cluster, remove NAs and scale
set.seed(1284)
fl_clus_all_b3 <- cluster::clara(na.omit(scale(fl_all_b3_df)), k=4)
##### plot
clust_all_b3 <- fl_rast_b3[[1]] 
clust_all_b3[] <- NA
clust_all_b3[idx_all_b3] <- fl_clus_all_b3$clustering
plot(clust_all_b3) 
writeRaster(clust_all_b3, here("output/clust_all_b3_k3.tif"), overwrite = T)

#### Outcome ----
#### Clara clustering (a method similar to K-mean clustering for large data) 
#### appears to predict vegetation cover well. Visual inspection shows the model
#### accurately identifies water and algae. Bare/less vegetated patches seem 
#### less well identified, however, this may be due to the coarse resolution 
#### making it difficult to identify these visually. 





### SOM clustering ----

library(kohonen)

fl_b1_df <- as.data.frame(fl_b1, na.rm = T)

# make a train data sets that scaled and convert them to be a matrix cause kohonen function accept numeric matrix
ads.train <- as.matrix(scale(fl_b1_df))

# make a SOM grid
set.seed(1287)
ads.grid <- class::somgrid(xdim = 10, ydim = 10, topo = "rectangular")

# make a SOM model
set.seed(1287)
ads.model <- som(ads.train, ads.grid, rlen = 500, radius = 2.5, keep.data = TRUE,
                 dist.fcts = "euclidean")
str(ads.model)

ads.model$unit.classif
head(data.frame(ads.train), 20)

plot(ads.model, type = "changes")

library(factoextra)

set.seed(100)
fviz_nbclust(ads.model$codes[[1]], kmeans, method = "wss")

set.seed(100)
clust <- kmeans(ads.model$codes[[1]], 10)

plot(ads.model, type = "codes", bgcol = rainbow(9)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(ads.model, clust$cluster)

ads.cluster <- data.frame(fl_b1_df, cluster = clust$cluster[ads.model$unit.classif])
tail(ads.cluster, 10)

## Supervised classifiaction ----
### Random Forest ----
#### https://geoscripting-wur.github.io/AdvancedRasterAnalysis/
fl_b1 <- mask(crop(c(fl_multi_spec, fl_indices), fl_bq_veg_zone), fl_b1_shoreline_boundary)
plot(fl_b1)

plot(fl_b1[[1]])
plot(fl_b1_train_poly, add = T)

fl_b1_train_poly$type <- as.factor(fl_b1_train_poly$type)
fl_b1_train_poly$code <- as.numeric(fl_b1_train_poly$type)

# Extract pixel values below the polygons into a dataframe
trainingData <- extract(fl_b1, fl_b1_train_poly, na.rm = T)

# Add a column specifying the class based on the polygon ID
trainingData$type <- fl_b1_train_poly$type[trainingData$ID]

# Remove the training polygon ID's from the dataframe
trainingData$ID <- NULL

# show tally
trainingData %>% group_by(type) %>% tally()

#### Remove values outside veg zone
trainingData <- na.omit(trainingData)

head(trainingData, n = 10)
tail(trainingData, n = 10)

#### Visualize 
val_water <- subset(trainingData, type == "water")
val_veg <- subset(trainingData, type == "veg")
val_algae <- subset(trainingData, type == "algae")

# NDWI
hist(val_water$NDWI, main = "water", xlab = "NDWI", xlim = c(0, 1), col = "blue")
hist(val_veg$NDWI, main = "veg", xlab = "NDWI", xlim = c(0, 1), col = "darkgreen")
hist(val_algae$NDWI, main = "algae", xlab = "NDWI", xlim = c(0, 1), col = "lightgreen")

library(ranger)

modelRF <- ranger(x = trainingData[, 1:ncol(trainingData)-1], y = trainingData$type,
                  importance = "permutation", seed = 1287)

# Inspect the structure and element names of the resulting model
modelRF
treeInfo(modelRF)
class(modelRF)
str(modelRF)
names(modelRF)

# Inspect the confusion matrix of the OOB error assessment
modelRF$confusion.matrix

as.data.frame(importance(modelRF)) %>% arrange(desc(importance(modelRF)))

#### Predict
predLC <- predict(fl_b1, modelRF, fun = function(...) predict(...)$predictions, na.rm = T)

freq(predLC)
plot(predLC)

### CART ----
library(rpart)

fl_b1 <- mask(crop(c(fl_multi_spec, fl_indices), fl_b1_veg_zone), fl_b1_shoreline_boundary)
plot(fl_b1)

fl_b1_train_poly <- subset(fl_b1_train_poly, fl_b1_train_poly$type != "structure")
fl_b1_train_poly <- subset(fl_b1_train_poly, fl_b1_train_poly$type != "bare")

## Sample classes
### number of sample points was selected arbitrarily
set.seed(1287)
classes_pnts <- spatSample(fl_b1_train_poly, 5000, "random")

plot(fl_b1[[1]])
plot(classes_pnts, add = T)

### insepect sample distribution
as.data.frame(classes_pnts) %>%
  group_by(type) %>%
  tally()

### convert the sample points to a matrix with coordinates for each location
sample_xy <- as.matrix(geom(classes_pnts)[,c('x','y')])
length(sample_xy)
length(sample_xy[!duplicated(sample_xy[,1]),])

### extract the values of the basin 1 raster at each sample coordinate
class_df <- terra::extract(fl_b1, sample_xy)

### create df for modelling by combining raster values and manual id names
sampdata <- data.frame(class = classes_pnts$type, class_df)

set.seed(1287)
cartmodel <- rpart(as.factor(class)~., data = sampdata,
                   method = 'class', minsplit = 5)
print(cartmodel)
plot(cartmodel, uniform=TRUE, main="Classification Tree")
text(cartmodel, cex = 1)

#### Predict
classified <- predict(fl_b1, cartmodel, na.rm = TRUE)
classified
plot(classified)

## Plot land cover class based on highest liklihood from cart model
### Island included
lc <- which.max(classified)
lc

cls <- c("algae","bare","structure", "veg", "water")
df <- data.frame(id = 1:5, class=cls)
levels(lc) <- df
as.numeric(lc)

plot(lc)

writeRaster(lc, here("tmp/cart_output.tif"), overwrite = T)

### k-fold validataion
#### data is split into k groups and the model is refit with one group being
#### used for model testing while the remaining will be used for model training
set.seed(1967)

k <- 5 # number of folds
j <- sample(rep(1:k, each = round(nrow(sampdata))/k))
table(j)

#### train and test the model five times, each time computing the predictions
#### and storing
x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class',
                minsplit = 5)
  pclass <- predict(cart, test, na.rm = TRUE)
  # assign class to maximum probablity
  pclass <- apply(pclass, 1, which.max)
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

#### combine the five list elements into a single dataframe and calculate
#### confusion matrix
y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
# confusion matrix
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- c("algae","veg", "water")
rownames(conmat) <- c("algae", "veg", "water")
print(conmat)

### Calculate overall accuracy
n <- sum(conmat) # number of total cases/samples
diag <- diag(conmat) # number of correctly classified cases per class
OA <- sum(diag) / n # overall accuracy
OA

### Calculate kappa statistics
#### kappa represents the overall agreement between categorical datasets
rowsums <- apply(conmat, 1, sum) # observed (true) cases per class
p <- rowsums / n

colsums <- apply(conmat, 2, sum) # predicted cases per class
q <- colsums / n

expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa

### Producer and user accuracy
PA <- diag / colsums # Producer accuracy
UA <- diag / rowsums # User accuracy
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc
