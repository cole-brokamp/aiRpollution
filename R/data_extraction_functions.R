
#' Distance to closest
#'
#' Function which calculates the distance to a closest spatial object. Anything that works with rgeos::gDistance will work in this function. loc will be reprojected to the projection of the lines.shapefile and the units will be of that projection.
#' @param loc spatial object (with valid proj4string)
#' @param lines.shapefile spatial object for which to calculate minimum distance
#'
#' @return data.frame with distance; named based on lines.shapefile input (units are assumed to be meters; if feet back transform output by 0.3048006096 m/ft)
#' @export
#'

distanceToClosest <- function(loc,lines.shapefile) {
  loc <- sp::spTransform(loc,CRS(proj4string(lines.shapefile)))
  dist.to <- rgeos::gDistance(loc,lines.shapefile)
  out <- data.frame(ifelse(is.null(dist.to),NA,dist.to*0.3048006096))
  names(out) <- paste0('distance.to.',deparse(substitute(lines.shapefile)))
  return(out)
}

#' Calculate greenspace
#'
#' Uses the greenspace raster (ndvi2000_3735.tif) to extract average greenspace values
#' @param loc spatial object (with valid proj4string)
#' @param buffer.radius (radius of circle in which to calculate average NDVI)
#'
#' @return data.frame with value; named greenspace_buffer.radius
#' @export
#'
greenspace_static <- function(loc,buffer.radius=400) {
  # assumes greenspace.raster is avail and projected correctly
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(greenspace.raster)))
  greenspace <- data.frame(extract(greenspace.raster,loc,buffer=buffer.radius/0.3048006096,fun=mean)[[1]])
  names(greenspace) <- paste0('greenspace_',buffer.radius)
  return(greenspace)
}


# greenspace_static(ccaaps_locations[2, ])

elevation_static <- function(loc,buffer.radius=0) {
  # assumes elevation.raster is avail and projected correctly
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(elevation.raster)))
  if (buffer.radius == 0) {
    elevation <- data.frame(extract(elevation.raster,loc)[[1]])
    names(elevation) <- 'elevation'
    return(elevation)
  }
  if (buffer.radius > 0) {
    elevation <- data.frame(extract(elevation.raster,loc,buffer=buffer.radius/0.3048006096,fun=mean)[[1]])
    names(elevation) <- paste0('elevation_',buffer.radius)
    return(elevation)
  }
}


# elevation_static(ccaaps_locations[2, ],buffer.radius=0)

elevation_sd <- function(loc,buffer.radius=100) {
  # assumes elevation.raster is avail and projected correctly
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(elevation.raster)))
  elevation <- data.frame(extract(elevation.raster,loc,buffer=buffer.radius/0.3048006096,fun=sd)[[1]])
  names(elevation) <- paste0('elevation.sd_',buffer.radius)
  return(elevation)
}

# elevation_sd(ccaaps_locations[2, ],buffer.radius=100)

elevation_uphill <- function(loc,buffer.radius=100) {
  # assumes elevation.raster is avail and projected correctly
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(elevation.raster)))
  loc.elevation <- extract(elevation.raster,loc)[[1]]
  elevations <- na.omit(extract(elevation.raster,loc,buffer=buffer.radius/0.3048006096)[[1]])
  uphill.frac <- data.frame(sum(elevations > loc.elevation + 20) / length(elevations))
  names(uphill.frac) <- paste0('elevation.uphill_',buffer.radius)
  return(uphill.frac)
}

# elevation_uphill(ccaaps_locations[4, ],buffer.radius=1000)

elevation_downhill <- function(loc,buffer.radius=100) {
  # assumes elevation.raster is avail and projected correctly
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(elevation.raster)))
  loc.elevation <- extract(elevation.raster,loc)[[1]]
  elevations <- na.omit(extract(elevation.raster,loc,buffer=buffer.radius/0.3048006096)[[1]])
  downhill.frac <- data.frame(sum(elevations < loc.elevation - 20) / length(elevations))
  names(downhill.frac) <- paste0('elevation.downhill_',buffer.radius)
  return(downhill.frac)
}

# elevation_downhill(ccaaps_locations[4, ],buffer.radius=1000)


LandCover <- function(loc,buffer.radius=400) {
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(landcover.raster)))
  # landcover raster is projected in meters
  landcover <- extract(landcover.raster,loc,buffer=buffer.radius)[[1]]
  landcover <- factor(landcover,levels=landcover.raster@data@attributes[[1]]$BinValues)
  levels(landcover) <- landcover.raster@data@attributes[[1]]$Value
  # levels(landcover) <- c('water','developed.open','developed.low','developed.med',
                         # 'developed.high',rep('non.developed',10))
  land.cover.table <- as.data.frame(prop.table(table(landcover)))
  out <- data.frame(t(land.cover.table$Freq))
  names(out) <- paste0(land.cover.table$landcover,'_',buffer.radius)
  return(out)
}

# LandCover(ccaaps_locations[2, ])

interstateTruckTraffic <- function(loc,buffer.radius=400) {
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(interstate.traffic)))
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  inside.buffer <- rgeos::gCrosses(buffer,interstate.traffic,byid=TRUE) |
    rgeos::gContains(buffer,interstate.traffic,byid=TRUE)
  if (sum(inside.buffer) == 0) interstate.truck.total <- 0
  if (sum(inside.buffer) > 0) {
    interstate.truck.data <- interstate.traffic@data[inside.buffer,c('ADT_TRUCK','Name')]
    interstate.truck.total <- mean(interstate.truck.data[!interstate.truck.data$ADT_TRUCK==0,'ADT_TRUCK'],na.rm=TRUE)
    if(is.na(interstate.truck.total)) interstate.truck.total <- 0
  }
  out <- data.frame(interstate.truck.total)
  names(out) <- paste0('interstate.truck_',buffer.radius)
  return(out)
}

highwayTruckTraffic <- function(loc,buffer.radius=400) {
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(highway.traffic))) # set location to projection of interstate.lines shapefile
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  inside.buffer <- rgeos::gCrosses(buffer,highway.traffic,byid=TRUE) |
    rgeos::gContains(buffer,highway.traffic,byid=TRUE)
  if (sum(inside.buffer) == 0) highway.truck.total <- 0
  if (sum(inside.buffer) > 0) {
    highway.truck.data <- highway.traffic@data[inside.buffer,c('ADT_TRUCK','Name')]
    highway.truck.total <- mean(highway.truck.data[!highway.truck.data$ADT_TRUCK==0,'ADT_TRUCK'],na.rm=TRUE)
    if(is.na(highway.truck.total)) highway.truck.total <- 0
  }
  out <- data.frame(highway.truck.total)
  names(out) <- paste0('highway.truck_',buffer.radius)
  return(out)
}

intersection_count <- function(loc,buffer.radius=1000) {
  loc <- sp::spTransform(loc,CRS(proj4string(d.intersections)))
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  crop.buffer <- rgeos::gIntersection(buffer,d.intersections,byid=FALSE)
  if (is.null(crop.buffer)) out <- 0
  if (!is.null(crop.buffer)) out <- data.frame(x=nrow(crop.buffer@coords))
  names(out) <- paste0('intersection.count_',buffer.radius)
  return(out)
}

# intersection_count(ccaaps_locations[1, ],buffer.radius=200)

## length of lines

#' calculate the length of lines within a buffer radius
#'
#' @param loc sp object with coordinates and valid proj4string
#' @param lines.shapefile shapefile of lines to use in calculation
#' @param buffer.radius buffer radius in meters (assumes that lines.shapefile is projected to unit of feet)
#'
#' @return data.frame named based on buffer radius and lines shapefile
#' @export
linesLength <- function(loc,lines.shapefile,buffer.radius=100) {
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(lines.shapefile))) # set location to projection of lines.shapefile
  buffer <- rgeos::gBuffer(loc, width=buffer.radius/0.3048006096,quadsegs=1000)
  crop.buffer <- rgeos::gIntersection(buffer,lines.shapefile,byid=TRUE)
  if(is.null(crop.buffer)) length.total <- 0
  if(!is.null(crop.buffer)) length.total <- rgeos::gLength(crop.buffer,byid=FALSE)
  out <- data.frame(length.total*0.3048006096)
  names(out) <- paste0('lines.length_',deparse(substitute(lines.shapefile)),'_',buffer.radius)
  return(out)
}

# linesLength(ccaaps_locations[1, ],buffer.radius=100,lines.shapefile=roads_1)

## length of lines (no doubles)
linesLength_noDoubles <- function(lat,lon,lines.shapefile,buffer.radius=100) {
  suppressPackageStartupMessages(library(rgdal))
  loc <- sp::spTransform(loc,CRS(proj4string(lines.shapefile))) # set location to projection of lines.shapefile
  buffer <- rgeos::gBuffer(loc, width=buffer.radius/0.3048006096,quadsegs=1000)
  crop.buffer <- rgeos::gIntersection(buffer,lines.shapefile,byid=TRUE)
  if(is.null(crop.buffer)) length.total <- 0
  if(!is.null(crop.buffer)) {
    # extract coordinates from the crop.buffer lines
    crop.buffer.coords <- lapply(1:length(crop.buffer),function(x) crop.buffer@lines[[x]]@Lines[[1]]@coords)
    # extract individual lengths of all line fragments
    crop.buffer.lengths <- lapply(crop.buffer.coords,function(CB) {
      CB <- as.data.frame(CB)
      sp::coordinates(CB) <- c('x','y')
      sp::proj4string(CB) <- sp::CRS(proj4string(lines.shapefile))
      crop.buffer.distance.matrix <- rgeos::gDistance(CB,byid=TRUE)
      # extract one row above upper diagonal to get distances between all sequential points
      crop.buffer.distances <- mapply(x=1:(nrow(crop.buffer.distance.matrix)-1),
                                      y=2:nrow(crop.buffer.distance.matrix),
                                      function(x,y) crop.buffer.distance.matrix[x,y])
      return(crop.buffer.distances)
    })

    # take all lengths which differ by no more than 1e-6 feet
    crop.buffer.lengths <- unlist(crop.buffer.lengths)
    crop.buffer.lengths.unique <- unique(round(crop.buffer.lengths,6))
    length.total <- sum(crop.buffer.lengths.unique)
  }
  out <- data.frame(length.total*0.3048006096)
  names(out) <- paste0('lines.length.nd_',buffer.radius)
  return(out)
}


## census info

#' Extract the deprivation index
#'
#' Deprivation index derived from PC analysis of 8 different SES Census Tract variables from the 2010 ACS.
#' @param loc loc spatial object (with valid proj4string)
#' @param buffer.radius if set to 0, returns the dep index of the containing census tract, if set to > 0, returns the mean dep index for the census tracts that are at least partially contained in the buffer circle
#'
#' @return data.frame, named dep.index or dep.index_buffer.radius
#' @export
#'
depIndex <- function(loc,buffer.radius=0) {
  loc <- sp::spTransform(loc,CRS(proj4string(deprivation.shapes)))
  if (buffer.radius == 0) {
    loc.int <- sp::over(loc,deprivation.shapes)
    dep.index <- as.numeric(as.character(loc.int$dep.index))
    return(data.frame('dep.index'=dep.index))
  }
  if (buffer.radius > 0) {
    buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
    loc.int <- sp::over(buffer,deprivation.shapes,returnList=TRUE)
    dep.indicies <- as.numeric(as.character(loc.int[[1]]$dep.index))
    mean.dep.index <- data.frame(mean(dep.indicies,na.rm=TRUE))
    names(mean.dep.index) <- paste0('dep.index_',buffer.radius)
    return(mean.dep.index)
  }
}

# depIndex(ccaaps_locations[1, ],buffer.radius=0)

population <-  function(loc,buffer.radius=0) {
  loc <- sp::spTransform(loc,CRS(proj4string(population.shapes)))
  if (buffer.radius == 0) {
    loc.int <- sp::over(loc,population.shapes)
    pop <- as.numeric(as.character(loc.int$FXS001))
    return(data.frame('population'=pop))
  }
  if (buffer.radius > 0) {
    buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
    loc.int <- sp::over(buffer,population.shapes,returnList=TRUE)
    pop.densities <- as.numeric(as.character(loc.int[[1]]$FXS001)) / as.numeric(as.character(loc.int[[1]]$area))
    mean.pop.density <- data.frame(mean(pop.densities,na.rm=TRUE))
    names(mean.pop.density) <- paste0('population.density_',buffer.radius)
    return(mean.pop.density)
  }
}

# population(ccaaps_locations[1, ],buffer.radius=2000)

nei_count <- function(loc,buffer.radius=10000,element='PM25') {
  element.name <- grep(element,names(d.NEI@data),value=TRUE)
  d.NEI.subset <- d.NEI[!is.na(d.NEI@data[ ,element.name]), ]
  loc <- sp::spTransform(loc,CRS(proj4string(d.NEI.subset)))
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  crop.buffer <- rgeos::gIntersection(buffer,d.NEI.subset,byid=FALSE)
  if (is.null(crop.buffer)) out <- 0
  if (!is.null(crop.buffer)) out <- data.frame(x=nrow(crop.buffer@coords))
  names(out) <- paste0('NEI.count_',buffer.radius)
  return(out)
}

# nei_count(ccaaps_locations[1, ],buffer.radius=10000,element='PM25')

nei_dist <- function(loc,element='PM25') {
  element.name <- grep(element,names(d.NEI@data),value=TRUE)
  d.NEI.subset <- d.NEI[!is.na(d.NEI@data[ ,element.name]), ]
  loc <- sp::spTransform(loc,CRS(proj4string(d.NEI.subset)))
  dist <- rgeos::gDistance(loc,d.NEI.subset,byid=FALSE)
  out <- data.frame('NEI.dist'=dist * 0.3048006096)
  return(out)
}

# nei_dist(ccaaps_locations[1, ],element='PM25')

nei_emissions_total <- function(loc,buffer.radius=100000,element='PM25') {
  element.name <- grep(element,names(d.NEI@data),value=TRUE)
  d.NEI.subset <- d.NEI[!is.na(d.NEI@data[ ,element.name]), ]
  loc <- sp::spTransform(loc,CRS(proj4string(d.NEI.subset)))
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  intersects.flag <- as.vector(rgeos::gIntersects(buffer,d.NEI.subset,byid=c(FALSE,TRUE)))
  crop.buffer <- d.NEI.subset[intersects.flag, ]
  if (nrow(crop.buffer@data) == 0) out <- 0
  if (! nrow(crop.buffer@data) == 0) {
    emissions <- crop.buffer@data[ ,element.name]
    out <- data.frame(sum(emissions))
  }
  names(out) <- paste0('NEI.emissions.total_',buffer.radius)
  return(out)
}

# nei_emissions_total(ccaaps_locations[1, ],buffer.radius=10000,element='PM25')

nei_emissions_mean <- function(loc,buffer.radius=100000,element='PM25') {
  element.name <- grep(element,names(d.NEI@data),value=TRUE)
  d.NEI.subset <- d.NEI[!is.na(d.NEI@data[ ,element.name]), ]
  loc <- sp::spTransform(loc,CRS(proj4string(d.NEI.subset)))
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  intersects.flag <- as.vector(rgeos::gIntersects(buffer,d.NEI.subset,byid=c(FALSE,TRUE)))
  crop.buffer <- d.NEI.subset[intersects.flag, ]
  if (nrow(crop.buffer@data) == 0) out <- 0
  if (! nrow(crop.buffer@data) == 0) {
    emissions <- crop.buffer@data[ ,element.name]
    out <- data.frame(mean(emissions,na.rm=TRUE))
  }
  names(out) <- paste0('NEI.emissions.mean_',buffer.radius)
  return(out)
}

# nei_emissions_mean(ccaaps_locations[1, ],buffer.radius=1000,element='PM25')

nei_emissions_dist <- function(loc,buffer.radius=100000,element='PM25') {
  element.name <- grep(element,names(d.NEI@data),value=TRUE)
  d.NEI.subset <- d.NEI[!is.na(d.NEI@data[ ,element.name]), ]
  loc <- sp::spTransform(loc,CRS(proj4string(d.NEI.subset)))
  buffer <- rgeos::gBuffer(loc,width=buffer.radius/0.3048006096,quadsegs=1000)
  crop.buffer <- rgeos::gIntersection(buffer,d.NEI.subset,byid=FALSE)
  if (is.null(crop.buffer)) out <- 0
  if (! is.null(crop.buffer)) {
    intersects.flag <- as.vector(rgeos::gIntersects(buffer,d.NEI.subset,byid=c(FALSE,TRUE)))
    crop.buffer2 <- d.NEI.subset[intersects.flag, ]
    emissions <- crop.buffer2@data[ ,element.name]
    dists <- as.vector(rgeos::gDistance(loc,crop.buffer2,byid=c(FALSE,TRUE)))*0.3048006096
    out <- sum(emissions/dists)
  }
  names(out) <- paste0('NEI.emissions.dist_',buffer.radius)
  return(out)
}

# nei_emissions_dist(ccaaps_locations[1, ],buffer.radius=10000,element='PM25')
