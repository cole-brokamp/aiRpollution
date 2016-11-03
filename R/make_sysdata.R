# # load in final models
# library(tidyverse)
#
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/model_build_nodep/final_lu_elemental_models.RData')
#
# # load in source GIS files
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_NEI.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_census.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_landcoverdb.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/LU_data_old_ECAT.RData')
# load('/Users/cole/Documents/Biostatistics/elemental_land_use_models/data_munge/TIGER_2002.RData')
#
# library(raster)
# greenspace.raster <- raster('/Users/cole/Documents/Biostatistics/_GIS data/Greenspace NDVI rasters/ndvi2000_3735.tif')
# proj4string(greenspace.raster) <- CRS('+init=epsg:3735')
# greenspace.raster <- readAll(greenspace.raster)
# elevation.raster <- readAll(elevation.raster)
# landcover.raster <- readAll(landcover.raster)
#
# # make sysdata
# devtools::use_data(d_final_el_models,bus_lines,d.intersections,d.NEI,deprivation.shapes,
#                    elevation.raster,greenspace.raster,highway.traffic,interstate.traffic,
#                    landcover.raster,population.shapes,
#                    railroads,roads_1,roads_2,roads_3,roads_4,roads_5,
#                    internal=TRUE,overwrite=TRUE,compress='gzip')
