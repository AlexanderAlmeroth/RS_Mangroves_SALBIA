###########################################################################################################################
## POST CLASSIFCATION CORRECTION AND ANALYSIS OF THE LAND-USE RASTERS ##
###########################################################################################################################
## PREPARATION ##
###########################################################################################################################
# Load libraries
#########################################

library(tmap)
library(sf)
library(terra)
library(dplyr)
library(raster)

#########################################
# set working directory
#########################################

setwd("X:/XXXX/XXXX/XXX/XXXX_XXXXX")

#########################################
# Load merged and manually corrected rasters
#########################################

ras1988 <- rast("1988_Merge.tif") 
ras1999 <- rast("1999_Merge.tif") 
ras2010 <- rast("2010_Merge.tif") 
ras2020 <- rast("2020_Merge.tif") 
ras2023 <- rast("2023_Merge.tif") 

#########################################
# Load and project shapefile
#########################################

shapefile_folder <- "X:/XXXX/XXXX/XXX/XXXX_XXXXX"
mask_shape <- st_read(dsn = shapefile_folder, layer = "MaskGambia")  #You can find the shapefile in /Scripts_GEE/Imports_GEE/Shapes
shapefile_projected <- st_transform(mask_shape, crs = 32628)

threshold_folder <- "X:/XXXX/XXXX/XXX/XXXX_XXXXX"
threshold_mangrove <- st_read(dsn = threshold_folder, layer = "Threshold_Mangrove_updated") #You can find the shapefile in /Scripts_R/Shapes
threshold_mangrove_proj <- st_transform(threshold_mangrove, crs = 32628)

#########################################
# Reproject rasters
#########################################

ras1988_pro <- project(ras1988, "EPSG:32628", method = "near")
ras1999_pro <- project(ras1999, "EPSG:32628", method = "near")
ras2010_pro <- project(ras2010, "EPSG:32628", method = "near")
ras2020_pro <- project(ras2020, "EPSG:32628", method = "near")
ras2023_pro <- project(ras2023, "EPSG:32628", method = "near")

#########################################
# Mask rasters
#########################################

ras1988_masked <- mask(ras1988_pro, shapefile_projected)
ras1999_masked <- mask(ras1999_pro, shapefile_projected)
ras2010_masked <- mask(ras2010_pro, shapefile_projected)
ras2020_masked <- mask(ras2020_pro, shapefile_projected)
ras2023_masked <- mask(ras2023_pro, shapefile_projected)

###########################################################################################################################
## POST CLASSIFICATION CORRECTION ##
###########################################################################################################################
## Reclassification of threshold area 
##################################################################################

categories <- c('Water', 'Dryland', 'Mudflat', 'Mangrove', 'Woodland')

reclassify_mangrove <- function(raster) {
  mangrove_area <- mask(raster, threshold_mangrove_proj)
  reclass_matrix <- cbind(3, 4)
  mangrove_area_reclassified <- classify(mangrove_area, reclass_matrix)
  raster_final <- cover(mangrove_area_reclassified, raster)
  return(raster_final)
}

ras1988_recl <- reclassify_mangrove(ras1988_masked)
ras1999_recl <- reclassify_mangrove(ras1999_masked)
ras2010_recl <- reclassify_mangrove(ras2010_masked)
ras2020_recl <- reclassify_mangrove(ras2020_masked)
ras2023_recl <- reclassify_mangrove(ras2023_masked)

#########################################
## Calculate area of reclassified area within the threshold 
#########################################
area_per_pixel <- 29.21314 * 29.21314 / 1e6 
calculate_reclassified_area <- function(original_raster, reclassified_raster, from_class, to_class, area_per_pixel) {
  original_values <- values(original_raster)
  reclassified_values <- values(reclassified_raster)
  
  reclassified_pixels <- sum(original_values == from_class & reclassified_values == to_class, na.rm = TRUE)
  total_area_km2 <- reclassified_pixels * area_per_pixel
  
  return(total_area_km2)
}

# Calculation of area per timestep
reclassified_areas <- data.frame(
  year = c(1988, 1999, 2010, 2020, 2023),
  area_km2 = c(
    calculate_reclassified_area(ras1988_masked, ras1988_recl, which(categories == "Mangrove") - 1, which(categories == "Woodland") - 1, area_per_pixel),
    calculate_reclassified_area(ras1999_masked, ras1999_recl, which(categories == "Mangrove") - 1, which(categories == "Woodland") - 1, area_per_pixel),
    calculate_reclassified_area(ras2010_masked, ras2010_recl, which(categories == "Mangrove") - 1, which(categories == "Woodland") - 1, area_per_pixel),
    calculate_reclassified_area(ras2020_masked, ras2020_recl, which(categories == "Mangrove") - 1, which(categories == "Woodland") - 1, area_per_pixel),
    calculate_reclassified_area(ras2023_masked, ras2023_recl, which(categories == "Mangrove") - 1, which(categories == "Woodland") - 1, area_per_pixel)
  )
)

#########################################
# Save results as a CSV file
write.csv(reclassified_areas, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/reclassified_mangrove_area.csv", row.names = FALSE)

## Save rasters 
writeRaster(ras1988_recl, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/1988_treshholdadjusted.tif", overwrite=TRUE)
writeRaster(ras1999_recl, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/1999_treshholdadjusted.tif", overwrite=TRUE)
writeRaster(ras2010_recl, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/2010_treshholdadjusted.tif", overwrite=TRUE)
writeRaster(ras2020_recl, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/2020_treshholdadjusted.tif", overwrite=TRUE)
writeRaster(ras2023_recl, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/2023_treshholdadjusted.tif", overwrite=TRUE)

##################################################################################
## CANCLELING FORBIDDEN TRANSITIONS
##################################################################################
# Function to adjust rasters in order to prevent forbidden transitions
adjust_raster <- function(raster1, raster2, forbidden_from, forbidden_to) {
  raster1_values <- values(raster1)
  raster2_values <- values(raster2)
  
  # Ensure that both rasters have the same number of values
  if (length(raster1_values) != length(raster2_values)) {
    stop("Rasters do not have the same extent.")
  }
  
  # Avoid NA values
  valid_indices <- !is.na(raster1_values) & !is.na(raster2_values)

  # Replace forbidden transitions with the original value from raster1
  invalid_transition <- (raster1_values == forbidden_from & raster2_values == forbidden_to) & valid_indices
  raster2_values[invalid_transition] <- raster1_values[invalid_transition]
  
  raster2_adjusted <- raster2
  values(raster2_adjusted) <- raster2_values
  
  return(raster2_adjusted)
}

mangrove_val <- which(categories == "Mangrove") - 1
woodland_val <- which(categories == "Woodland") - 1
dryland_val <- which(categories == "Dryland") - 1

# Create Adjusted Rasters 
ras1999_adj <- adjust_raster(ras1988_recl, ras1999_recl, mangrove_val, woodland_val)
ras1999_adj <- adjust_raster(ras1988_recl, ras1999_adj, woodland_val, mangrove_val)
ras1999_adj <- adjust_raster(ras1988_recl, ras1999_adj, dryland_val, mangrove_val)
ras1999_adj <- adjust_raster(ras1988_recl, ras1999_adj, mangrove_val, dryland_val)

ras2010_adj <- adjust_raster(ras1999_adj, ras2010_recl, mangrove_val, woodland_val)
ras2010_adj <- adjust_raster(ras1999_adj, ras2010_adj, woodland_val, mangrove_val)
ras2010_adj <- adjust_raster(ras1999_adj, ras2010_adj, dryland_val, mangrove_val)
ras2010_adj <- adjust_raster(ras1999_adj, ras2010_adj, mangrove_val, dryland_val)

ras2020_adj <- adjust_raster(ras2010_adj, ras2020_recl, mangrove_val, woodland_val)
ras2020_adj <- adjust_raster(ras2010_adj, ras2020_adj, woodland_val, mangrove_val)
ras2020_adj <- adjust_raster(ras2010_adj, ras2020_adj, dryland_val, mangrove_val)
ras2020_adj <- adjust_raster(ras2010_adj, ras2020_adj, mangrove_val, dryland_val)

ras2023_adj <- adjust_raster(ras2020_adj, ras2023_recl, mangrove_val, woodland_val)
ras2023_adj <- adjust_raster(ras2020_adj, ras2023_adj, woodland_val, mangrove_val)
ras2023_adj <- adjust_raster(ras2020_adj, ras2023_adj, dryland_val, mangrove_val)
ras2023_adj <- adjust_raster(ras2020_adj, ras2023_adj, mangrove_val, dryland_val)

#########################################
# Saving of adjusted rasters 

writeRaster(ras1999_adj, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/ras1999_fin.tif", overwrite=TRUE)
writeRaster(ras2010_adj, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/ras2010_fin.tif", overwrite=TRUE)
writeRaster(ras2020_adj, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/ras2020_fin.tif", overwrite=TRUE)
writeRaster(ras2023_adj, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/ras2023_fin.tif", overwrite=TRUE)



###########################################################################################################################
## ANALYSIS ##
##################################################################################
## AREA CALCULATION OF PREVENTED TRANSITIONS ##
##################################################################################

# Function to calculate the prevented area for a specific transition type

calculate_prevented_transition <- function(baseline, reclassified, adjusted, from_val, to_val, area_per_pixel) {
  baseline_values <- values(baseline)
  reclass_values  <- values(reclassified)
  adjusted_values <- values(adjusted)
  
  # Count pixels where the original raster had 'from_val',
  # the reclassified raster has 'to_val' (i.e., the forbidden transition),
  # and the adjusted raster retained the original (baseline) value.
  prevented_pixels <- sum(baseline_values == from_val & reclass_values == to_val & adjusted_values == baseline_values, na.rm = TRUE)
  prevented_area   <- prevented_pixels * area_per_pixel
  return(prevented_area)
}

# Definition of intervals: for each interval, the baseline raster,
# the reclassified raster, and the resulting (adjusted) raster are defined.
intervals <- list(
  "1988-1999" = list(baseline = ras1988_recl, reclassified = ras1999_recl, adjusted = ras1999_adj),
  "1999-2010" = list(baseline = ras1999_adj, reclassified = ras2010_recl, adjusted = ras2010_adj),
  "2010-2020" = list(baseline = ras2010_adj, reclassified = ras2020_recl, adjusted = ras2020_adj),
  "2020-2023" = list(baseline = ras2020_adj, reclassified = ras2023_recl, adjusted = ras2023_adj)
)

# Initialize dataframe to store results
prevented_transitions <- data.frame(
  Interval = character(),
  Mangrove_to_Woodland_km2 = numeric(),
  Woodland_to_Mangrove_km2 = numeric(),
  Dryland_to_Mangrove_km2 = numeric(),
  Mangrove_to_Dryland_km2 = numeric(),
  stringsAsFactors = FALSE
)

# For each interval, calculate the four transitions
for(interval_name in names(intervals)) {
  baseline   <- intervals[[interval_name]]$baseline
  reclassified <- intervals[[interval_name]]$reclassified
  adjusted   <- intervals[[interval_name]]$adjusted
  
  prevented_m2w <- calculate_prevented_transition(baseline, reclassified, adjusted, mangrove_val, woodland_val, area_per_pixel)
  prevented_w2m <- calculate_prevented_transition(baseline, reclassified, adjusted, woodland_val, mangrove_val, area_per_pixel)
  prevented_d2m <- calculate_prevented_transition(baseline, reclassified, adjusted, dryland_val, mangrove_val, area_per_pixel)
  prevented_m2d <- calculate_prevented_transition(baseline, reclassified, adjusted, mangrove_val, dryland_val, area_per_pixel)
  
  prevented_transitions <- rbind(prevented_transitions, data.frame(
    Interval = interval_name,
    Mangrove_to_Woodland_km2 = prevented_m2w,
    Woodland_to_Mangrove_km2 = prevented_w2m,
    Dryland_to_Mangrove_km2 = prevented_d2m,
    Mangrove_to_Dryland_km2 = prevented_m2d
  ))
}

#########################################
# Save results as a CSV file
write.csv(prevented_transitions, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/prevented_transitions_fin.csv", row.names = FALSE)

##################################################################################
## CALCULATION OF TRANSITIONS PER TIME PERIOD ##
##################################################################################

get_transition_area <- function(raster1, raster2, from_class, to_class) {
  raster1_values <- values(raster1)
  raster2_values <- values(raster2)
  transition_pixels <- sum(raster1_values == from_class & raster2_values == to_class, na.rm = TRUE)
  return(transition_pixels)
}

transition_pairs <- expand.grid(from = categories, to = categories)

transition_counts <- data.frame(
  Category = apply(transition_pairs, 1, function(x) paste(x[1], "->", x[2])),
  `1988-1999` = NA,
  `1999-2010` = NA,
  `2010-2020` = NA,
  `2020-2023` = NA,
  `1988-2023` = NA
)

for (i in 1:nrow(transition_counts)) {
  from_class <- which(categories == transition_pairs$from[i]) - 1
  to_class <- which(categories == transition_pairs$to[i]) - 1
  transition_counts[i, 2] <- get_transition_area(ras1988_recl, ras1999_adj, from_class, to_class)
  transition_counts[i, 3] <- get_transition_area(ras1999_adj, ras2010_adj, from_class, to_class)
  transition_counts[i, 4] <- get_transition_area(ras2010_adj, ras2020_adj, from_class, to_class)
  transition_counts[i, 5] <- get_transition_area(ras2020_adj, ras2023_adj, from_class, to_class)
  transition_counts[i, 6] <- get_transition_area(ras1988_recl, ras2023_adj, from_class, to_class)
}

area_per_pixel <- 29.21314 * 29.21314 / 1e6
transition_counts[, 2:6] <- round(transition_counts[, 2:6] * area_per_pixel, 2)
  
#########################################
# Save results as a CSV file
write.csv(transition_counts, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/transition_counts_fin.csv", row.names = FALSE)

##################################################################################
## AREA CALCULATION PER CLASS AND YEAR ##
##################################################################################

calculate_area <- function(raster, categories, area_per_pixel) {
  class_areas <- data.frame(class = categories, area_km2 = 0)
  for (i in seq_along(categories)) {
    pixel_count <- sum(values(raster) == (i - 1), na.rm = TRUE)
    class_areas$area_km2[i] <- pixel_count * area_per_pixel
  }
  return(class_areas)
}

final_area_df <- bind_rows(
  calculate_area(ras1988_recl, categories, area_per_pixel) %>% mutate(year = 1988),
  calculate_area(ras1999_adj, categories, area_per_pixel) %>% mutate(year = 1999),
  calculate_area(ras2010_adj, categories, area_per_pixel) %>% mutate(year = 2010),
  calculate_area(ras2020_adj, categories, area_per_pixel) %>% mutate(year = 2020),
  calculate_area(ras2023_adj, categories, area_per_pixel) %>% mutate(year = 2023)
)
final_area_df <- final_area_df %>% dplyr::select(year, class, area_km2)

#########################################
# Save results as a CSV file
write.csv(final_area_df, "X:/XXXX/XXXX/XXX/XXXX_XXXXX/final_area_fin.csv", row.names = FALSE)
