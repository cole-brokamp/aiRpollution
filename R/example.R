## example
sample.loc <- data.frame('x'=-84.5371597,'y'=39.1603015)
coordinates(sample.loc) <- c('x','y')
sp::proj4string(sample.loc) <- CRS("+init=epsg:4326")
XX <- predictPollution(loc=sample.loc,element='TRAP',model.type='rf')
