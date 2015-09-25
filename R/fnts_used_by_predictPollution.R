## functions
predictorToFunctionCall <- function(final.model.predictor.name) {
  final.model.predictor.name <- gsub('`','',final.model.predictor.name)
  predictor.short.name <- gsub("[0-9]{3,5}",'',final.model.predictor.name)
  fnt_arg <- switch(predictor.short.name,
                    distance.to.roads_1 = list(distanceToClosest,roads_1),
                    distance.to.roads_2 = list(distanceToClosest,roads_2),
                    distance.to.roads_3 = list(distanceToClosest,roads_3),
                    distance.to.roads_4 = list(distanceToClosest,roads_4),
                    distance.to.roads_5 = list(distanceToClosest,roads_5),
                    distance.to.railroads = list(distanceToClosest,railroads),
                    lines.length_roads1_ = list(linesLength,roads_1),
                    lines.length_roads2_ = list(linesLength,roads_2),
                    lines.length_roads3_ = list(linesLength,roads_3),
                    lines.length_roads4_ = list(linesLength,roads_4),
                    lines.length_roads5_ = list(linesLength,roads_5),
                    lines.length_railroads_ = list(linesLength,railroads),
                    interstate.truck_ = interstateTruckTraffic,
                    highway.truck_ = highwayTruckTraffic,
                    intersection.count_50 = intersection_count,
                    intersection.count_ = intersection_count,
                    lines.length_bus_ = list(linesLength,bus_lines),
                    elevation = elevation_static,
                    elevation_ = elevation_static,
                    elevation.sd_ = elevation_sd,
                    elevation.uphill_ = elevation_uphill,
                    elevation.downhill_ = elevation_downhill,
                    population = population,
                    population.density_ = population,
                    dep.index_ = depIndex,
                    greenspace_ = greenspace_static,
                    open.water_ = LandCover,
                    developed.low_ = LandCover,
                    developed.med_ = LandCover,
                    developed.high_ = LandCover,
                    developed.open_ = LandCover,
                    barren_ = LandCover,
                    deciduous.forest_ = LandCover,
                    evergreen.forest_ = LandCover,
                    mixed.forest_ = LandCover,
                    shrub_ = LandCover,
                    grassland_ = LandCover,
                    pasture_ = LandCover,
                    crops_ = LandCover,
                    woody.wetlands_ = LandCover,
                    herbaceuous.wetlands_ = LandCover,
                    'PM25.PRI:dist' = list(nei_dist,'PM25'),
                    'PM25.PRI:count_' = list(nei_count,'PM25'),
                    'PM25.PRI:emissions.total_' = list(nei_emissions_total,'PM25'),
                    'PM25.PRI:emissions.mean_' = list(nei_emissions_mean,'PM25'),
                    'PM25.PRI:emissions.dist_' = list(nei_emissions_dist,'PM25'),
                    'PM10.PRI:dist' = list(nei_dist,'PM10'),
                    'PM10.PRI:count_' = list(nei_count,'PM10'),
                    'PM10.PRI:emissions.total_' = list(nei_emissions_total,'PM10'),
                    'PM10.PRI:emissions.mean_' = list(nei_emissions_mean,'PM10'),
                    'PM10.PRI:emissions.dist_' = list(nei_emissions_dist,'PM10'),
                    'Nickel.Compounds:dist' = list(nei_dist,'Nickel'),
                    'Nickel.Compounds:count_' = list(nei_count,'Nickel'),
                    'Nickel.Compounds:emissions.total_' = list(nei_emissions_total,'Nickel'),
                    'Nickel.Compounds:emissions.mean_' = list(nei_emissions_mean,'Nickel'),
                    'Nickel.Compounds:emissions.dist_' = list(nei_emissions_dist,'Nickel'),
                    'Lead.Compounds:dist' = list(nei_dist,'Lead'),
                    'Lead.Compounds:count_' = list(nei_count,'Lead'),
                    'Lead.Compounds:emissions.total_' = list(nei_emissions_total,'Lead'),
                    'Lead.Compounds:emissions.mean_' = list(nei_emissions_mean,'Lead'),
                    'Lead.Compounds:emissions.dist_' = list(nei_emissions_dist,'Lead'),
                    'Manganese.Compounds:dist' = list(nei_dist,'Manganese'),
                    'Manganese.Compounds:count_' = list(nei_count,'Manganese'),
                    'Manganese.Compounds:emissions.total_' = list(nei_emissions_total,'Manganese'),
                    'Manganese.Compounds:emissions.mean_' = list(nei_emissions_mean,'Manganese'),
                    'Manganese.Compounds:emissions.dist_' = list(nei_emissions_dist,'Manganese'))
  if (is.list(fnt_arg)) return(list('fnt'=fnt_arg[[1]],'arg'=fnt_arg[[2]]))
  if (! is.list(fnt_arg)) return(list('fnt'=fnt_arg,'arg'=NULL))
}

# predictorToFunctionCall('`PM25.PRI:dist`')
# predictorToFunctionCall('developed.high_1000')

predictorToBufferRadii <- function(final.model.predictor.name) {
  br <- stringr::str_extract(final.model.predictor.name,"[0-9]{3,5}")
  return(as.numeric(br))
}

# predictorToBufferRadii('`PM25.PRI:dist`')
# predictorToBufferRadii('developed.high_1000')

lu_data_gen <- function(loc=sample.locs,final.model.predictor.name) {
  nm <- final.model.predictor.name
  fnt_arg_out <- predictorToFunctionCall(nm)
  buf.r <- predictorToBufferRadii(nm)
  if (is.na(buf.r)) {
    if (is.null(fnt_arg_out$arg)) val.out <-
        fnt_arg_out[['fnt']](loc)
    if (! is.null(fnt_arg_out$arg)) val.out <-
        fnt_arg_out[['fnt']](loc,fnt_arg_out[['arg']])
  } else {
    if (is.null(fnt_arg_out$arg)) val.out <-
        fnt_arg_out[['fnt']](loc,buffer.radius=buf.r)
    if (! is.null(fnt_arg_out$arg)) val.out <-
        fnt_arg_out[['fnt']](loc,buffer.radius=buf.r,fnt_arg_out[['arg']])
  }
  if (sum(grepl('landcover',deparse(fnt_arg_out[['fnt']]))) > 1) val.out <- val.out[ ,nm]
  names(val.out) <- nm
  return(val.out)
}

# lu_data_gen(loc=sample.loc,final.model.predictor.name='`PM25.PRI:dist`')
# lu_data_gen(loc=sample.loc,final.model.predictor.name='developed.high_1000')

all_lu_data_gen <- function(loc,final.model.predictor.names,prog.bar=FALSE) {
  if(prog.bar) out <- pbapply::pbsapply(final.model.predictor.names,lu_data_gen,loc=loc)
  if(!prog.bar) out <- sapply(final.model.predictor.names,lu_data_gen,loc=loc)
  names(out) <- final.model.predictor.names
  return(data.frame(out,check.names=FALSE))
}

# all_lu_data_gen(loc=sample.loc,final.model.predictor.names=c('`PM25.PRI:dist`','developed.high_1000'))

