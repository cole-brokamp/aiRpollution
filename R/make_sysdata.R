# # load in final models
# load('/Users/cole/Documents/Biostatistics/Elemental Land Use Models/final_models/final land use LM models.RData')
# load('/Users/cole/Documents/Biostatistics/Elemental Land Use Models/final_models/final land use RF models.RData')
#
# # load in source GIS files
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_NEI.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_census.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_landcoverdb.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_old_ECAT.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/TIGER_2002.RData')
# 
# library(raster)
# elevation.raster <- readAll(elevation.raster)
# landcover.raster <- readAll(landcover.raster)
# 
# 
# greenspace.raster <- raster('/Users/cole/Documents/Biostatistics/_GIS data/Greenspace NDVI rasters/ndvi2000_3735.tif')
# proj4string(greenspace.raster) <- CRS('+init=epsg:3735')
# greenspace.raster <- readAll(greenspace.raster)
#
#
# # make sysdata
# devtools::use_data(bus_lines,d.intersections,d.NEI,deprivation.shapes,elevation.raster,
#                    greenspace.raster,highway.traffic,interstate.traffic,landcover.raster,
#                    LU.final.lm.models,LU.final.rf.models,population.shapes,railroads,
#                    roads_1,roads_2,roads_3,roads_4,roads_5,
#                    internal=TRUE,overwrite=TRUE,compress='gzip')

# # make data for bootstrap models
# source('data_extraction_functions.R')
# source('fnts_used_by_predictPollution.R')
# save(list=ls(all.names=TRUE),file='everything_needed_for_predictions.RData')
