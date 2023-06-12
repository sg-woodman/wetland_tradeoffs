## ---------------------------
##
## Script name: calc_worldview_indices
##
## Purpose of script: Process the WorldView-2 raster imagery to quantify emergent 
##  vegetation around the basins of Frank Lake. Various indices need to be 
##  calcualted to determine how to best idenitfy vegetated area. These can the
##  be quanitfied across the image using supervised or unsupervised 
##  classification models. 
##
## Author: Samuel Woodman
##
## Date Created: 2023-06-08
##
## Email: samuel.woodman@gmail.com
##
## ---------------------------
##
## Notes: 
##    Bands according to WorldView multispectral images
##      https://earth.esa.int/eogateway/missions/worldview/description
##      B1 Coastal: 400-450 nm
##      B2 Blue: 450-510 nm
##      B3 Green: 510-580 nm
##      B4 Yellow: 585-625 nm
##      B5 Red: 630-690 nm
##      B6 Red Edge: 705-745 nm
##      B7 Near-IR1: 770-895 nm
##      B8 Near-IR2: 860-1040 nm
##    
##    Radiometric consideration can be found here: 
##      https://dg-cms-uploads-production.s3.amazonaws.com/uploads/document/file/104/Radiometric_Use_of_WorldView-2_Imagery.pdf
##    
##    Indices sources
##    - https://www.techforwildlife.com/blog/2019/1/22/analysing-drone-and-satellite-imagery-using-vegetation-indices
##    
##   
##
## ---------------------------

options(scipen = 6, digits = 4)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(terra)
library(sf)


# Functions ---------------------------------------------------------------

## Calculate normalized difference of raster bands
### inputs: 
### outputs: 
calc_nd <- function(b1, b2) {
  (b2 - b1) / (b2 + b1)
  }


# Load files --------------------------------------------------------------

fl_multi_spec <- rast(here("data/raw/fl_multi_spec.tif"))

names(fl_multi_spec) <- c("Coastal", "Blue", "Green", "Yellow", 
                          "Red", "Red_Edge", "Near_IR1", "Near_IR2") 
plot(fl_multi_spec)


# Calculate indices -------------------------------------------------------

## NDVI 1
### Uses Near IR1 according to https://gis.stackexchange.com/questions/279714/which-nir-bandwidth-is-suitable-to-calculate-ndvi
fl_ndvi1 <- calc_nd(fl_multi_spec[["Near_IR1"]], fl_multi_spec[["Red"]])

plot(fl_ndvi1)

## NDVI 2
### Uses Near IR2
fl_ndvi2 <- calc_nd(fl_multi_spec[["Near_IR2"]], fl_multi_spec[["Red"]])

plot(fl_ndvi2)

## WorldView Water Index (WWI)
### Wolf, A. Using WorldView 2 Vis-NIR MSI Imagery to Support Land Mapping and Feature Extraction Using Normalized Difference Index Ratios. Unpublished report, Longmont, CO: DigitalGlobe (2010).

fl_wwi <- calc_nd(fl_multi_spec[["Near_IR2"]], fl_multi_spec[["Coastal"]])

plot(fl_wwi)

## Excesss Greenness (ExG)
### From In Flights data presentation given to Larry
fl_exg <- 2*fl_multi_spec[["Green"]] - fl_multi_spec[["Red"]] - fl_multi_spec[["Blue"]]

plot(fl_exg)

## Chlorophyll green
### https://www.researchgate.net/figure/Vegetation-indices-calculated-from-Worldview-2-data_tbl1_335679774
fl_chlr_g <- (fl_multi_spec[["Near_IR1"]]/fl_multi_spec[["Green"]])^-1

plot(fl_chlr_g)

## Chlorophyll red edge
### https://www.researchgate.net/figure/Vegetation-indices-calculated-from-Worldview-2-data_tbl1_335679774
fl_chlr_re <- (fl_multi_spec[["Near_IR2"]]/fl_multi_spec[["Red_Edge"]])-1

plot(fl_chlr_re)

## Green Leaf Index
### https://www.researchgate.net/figure/Vegetation-indices-calculated-from-Worldview-2-data_tbl1_335679774
fl_gli <- (2 * fl_multi_spec[["Green"]] - fl_multi_spec[["Red"]] - fl_multi_spec[["Coastal"]])/
  (2 * fl_multi_spec[["Green"]] + fl_multi_spec[["Red"]] + fl_multi_spec[["Coastal"]])

plot(fl_gli)

## Chlorophyll Vegetation Index
### https://www.researchgate.net/figure/Vegetation-indices-calculated-from-Worldview-2-data_tbl1_335679774
fl_cvi <- fl_multi_spec[["Near_IR2"]] * (fl_multi_spec[["Red"]]/fl_multi_spec[["Green"]]^2)

plot(fl_cvi)

## EVI
### https://pdf.sciencedirectassets.com/272637/1-s2.0-S0303243415X00067/1-s2.0-S0303243415001117/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEDUaCXVzLWVhc3QtMSJHMEUCIQCQpOLDu6MpvmJtfhnc4lIuYFGamZHRJ4ZZPu26%2FM5fNAIgSJpt2%2FIhcyQYNEMjBfAIML%2B6bgrtXl6wXKeJdpacO7IqsgUIfhAFGgwwNTkwMDM1NDY4NjUiDBPkHGhu3g1gfqGvSCqPBdFKUSwmcab%2FjoXpemL9Q3bHSBJMxI2rsQR2%2FqwXEt1cOazjWZthdNDMv5y4igvzwS52j6wTUCWDzyZNE7sAFhvDbwFUwFQ0FrR%2BDxNYceJzUTFDBTAQ6g4GNp1KfaK8eYGtD34TV4OcMNGhe8WQ6EPM%2FbHvtIsPyXGQmIiAm0qiD0Qzqn0CuarNDHRNXwIDF%2BrP5SJ445permtTz1dIP0XJBZ5s%2F7TCe3sCyhfgrydHCnl9HnkvwqG62eZ3maCGp%2FZmhPcOyKvNQvcyNHDpFHWOG5AKZsTybCt19kMt2NGkFvhCelF0v4c%2FbH3aq7OzW9OOzoWYAAg614DLM%2FdqMv7%2Fp4lb79p9GUW%2BKbLeW5R8IkIZQX39GqGcgcNIPBMFShBVOLVk4aeg2yhQ9sI4%2FeLMQWebYJP36x4C%2BRTq7ouZroHUsoMydDLQUbfvBGDsGBnLTEuAwFHWKy0IdMVa5GDwD0lpTtQ04bDA8J2oKWfyV2n1VAGfUThVnGw%2BS%2B2wjmwu9Bx%2F0DhKl8FZdlpXwLX3uqS7DfYsETC9rLZPK%2BVNpKv9bOdQFLXZPjpETmN%2BB7aP35cJZbNFEs58N%2F0LNoT5TpZ%2BruiFbLfd1fpQjMuyo6YkoUBM6t%2BHt7xWFUQpVTkVnBw6c9XfH5AZMyfmIXiNy%2Fz5sy9bCnYUYZgl6LjIJbazd7FU2wzo6CtIE5RPeUuD0neSx2NonVtpjsjitGRWMt4WeFT09PjWTFgancAX00M2YGgCvNcwkpRzuUg835WBaY10BadsAJD1fZbbWvE1YVMo2WREKLGmBR%2FPAP%2BMlFS%2BxNNAc%2BJP4G4w2bHHv8vfjjrgaK5oqquVdsKmJsQ9rXgEGJLUJcAtFpwewXUwlvWIpAY6sQFq8034lXm%2BMpMWqZl%2FtmPztQJovLySXl8xfX4bA%2BAT8yDAIW%2Flb%2FPlwMRpR1t799C54dFBMXLUAdAOSlsXZO7GtOCRATiRMvrfJs3mtL14i2a0QNEJPfkRCii6Ir3QBVKots2ICKzYnZRHbdYM7Pt9paiczBDuGEcjIH11qYY5hizx9zc15QTNsjzPyfN5bTpAd4i4MYgGhhRBxRz9M0riDZwMPOBjqj4iEpbUI6KZ7C8%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20230608T213918Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYRAILBBKH%2F20230608%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=5b91b36beb0effe2bb238fb3d8ac6db55e85ae129fc63fbd5b1caf07f200cace&hash=95c9bcc585e90dbb16e404faa872c0fefe6c0a43160c536a7bcfb86d1a6f5927&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S0303243415001117&tid=spdf-47707e35-f1a8-4d1e-a9e9-1bc735d9db4c&sid=ca7a830d761e834777298399aa6a19631f50gxrqa&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=050759025a5750565550&rr=7d444963a80227e6&cc=ca
fl_evi <- (2.5 *(fl_multi_spec[["Near_IR2"]] - fl_multi_spec[["Red"]])) / (fl_multi_spec[["Near_IR2"]] + (6*fl_multi_spec[["Red"]]) + (7.5*fl_multi_spec[["Blue"]]) + 1)

plot(fl_evi)

## NDWI
### https://www.l3harrisgeospatial.com/portals/0/pdfs/envi/8_bands_Antonio_Wolf.pdf
fl_ndwi <- calc_nd(fl_multi_spec[["Near_IR2"]], fl_multi_spec[["Coastal"]])

plot(fl_ndwi)

## Normalized Difference Soil Index
### https://www.l3harrisgeospatial.com/portals/0/pdfs/envi/8_bands_Antonio_Wolf.pdf
fl_ndsi <- calc_nd(fl_multi_spec[["Yellow"]], fl_multi_spec[["Green"]])

plot(fl_ndsi)

## Non-Homogeneous Feature Difference
### https://www.l3harrisgeospatial.com/portals/0/pdfs/envi/8_bands_Antonio_Wolf.pdf
fl_nhfd <- calc_nd(fl_multi_spec[["Coastal"]], fl_multi_spec[["Red_Edge"]])

plot(fl_nhfd)

## Modified Chlorophyll Absorption in Reflectance Index
### https://www.techforwildlife.com/blog/2019/1/22/analysing-drone-and-satellite-imagery-using-vegetation-indices
fl_mcari <- (fl_multi_spec[["Red_Edge"]] - fl_multi_spec[["Red"]]) - 0.2 * (fl_multi_spec[["Red_Edge"]] - fl_multi_spec[["Green"]]) * (fl_multi_spec[["Red_Edge"]]/fl_multi_spec[["Red"]])

plot(fl_mcari)


