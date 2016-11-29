# GBIF Range Limits Code
# Critical Thinking - 5 February 2016
# Written by John Godlee, Gergana Daskalova and Isla Myers-Smith (isla.myers-smith@ed.ac.uk)

# Libraries
library(dplyr)
library(data.table)

# Load data - make sure to set your working directory to the folder where the GBIF csv files are stored and also make sure that you don't have any other csv files in that folder!

# Code to read in GBIF csv files
# list <- list.files(pattern = ".csv")
# GBIF <- bind_rows(lapply(list, fread))

# This is a pipe to extract the minimum and maximum latitudes and longidudes for each of the species and make a simplified calculation of their geographical extent based on the GBIF occurence data
GBIFcoords <- GBIF11 %>% distinct(.) %>% filter(., is.finite(decimallatitude)) %>% filter(., is.finite(decimallongitude)) %>% group_by(., species) %>% summarise(., maxlat = max(decimallatitude), minlat = min(decimallatitude), maxlong = max(decimallongitude), minlong = min(decimallongitude), countoccur = length(occurrenceid)) %>% mutate(., lengthlat = maxlat-minlat, lengthlong = maxlong-minlong, range = lengthlat*lengthlong)

# Write object to a csv file
# write.table(GBIFcoords, file = "GBIFcoordsv8.csv", sep = ",", col.names = NA)

# Write object to .Rdata
# save(GBIF, file = "GBIF.RData")

# LPIUKslopes <- read.csv("~/Desktop/LPI Data Feb2016/LPIUKslopes.csv")
# GBIFcoords <- read.csv("~/Desktop/GBIF_csv_files_Feb2016/GBIFcsvs/GBIFcoordsv1.csv")
# GBIFcat <- read.csv(file.choose())

# LPIGBIF <- merge(LPIUKslopes, GBIFcoords, x.by='species', y.by='species', all.x = TRUE)
# LPIGBIF <- merge(LPIGBIF, GBIFcat, x.by='Common.Name', y.by='GBIF.common.name')

# write.table(LPIGBIF, file = "LPIGBIFmerged.csv", sep = ",", col.names = NA)

# coords1 <- read.csv("GBIFcoordsv1.csv")
# coords2 <- read.csv("GBIFcoordsv2.csv")
# coords3 <- read.csv("GBIFcoordsv3.csv")
# coords4 <- read.csv("GBIFcoordsv4.csv")
# coords5 <- read.csv("GBIFcoordsv5.csv")
# 
# coords <- bind_rows(coords1, coords2, coords3, coords4, coords5)
# head(coords)
# 
# write.table(coords, file = "GBIFcoords.csv", sep = ",", col.names = NA)
