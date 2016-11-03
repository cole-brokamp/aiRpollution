#' predict pollution exposure
#'
#' This function is an implementation of the elemental PM exposure (random forest and regression) models developed for the Cincinnati Children's Asthma and Air Pollution Study (CCAAPS).  The underlying functions are not available to the user.  Not meant to be a generalizable package, its sole purpose is to generate exposure estimates for the Cincinnati area. It will return NA if any of the necessary predictors are not available for the location.
#' @param loc the location for which to estimate the concentration (must be a spatial object and have a valid proj4string)
#' @param element element for which to predict the concentration (one of "Cu", "Fe", "Zn", "S", "Ni", "V", "Si", "K", "Pb", "Mn", "Al", "TRAP", "PM25")
#' @param prog.bar logical, show a progress bar?
#' @export
#' @examples
#' library(sp)
#' sample.loc <- data.frame('x'=-84.5371597,'y'=39.1603015)
#' coordinates(sample.loc) <- c('x','y')
#' proj4string(sample.loc) <- CRS("+init=epsg:4326")
#' predictPollution_LURF(loc=sample.loc,element='TRAP')
predictPollution_LURF <- function(loc,element) {
  stopifnot(element %in% d_final_el_models$element)
  final.model <- d_final_el_models[d_final_el_models$element==element,'LURF'][[1]][[1]]
  final.model.predictors <- row.names(randomForest::importance(final.model))
  new.pred.data <- all_lu_data_gen(loc=loc,final.model.predictor.names=final.model.predictors,
                                   prog.bar=FALSE)
  names(new.pred.data) <- sapply(names(new.pred.data),function(x) gsub('`','',x))
  if (complete.cases(new.pred.data)) {
    out <- predict(final.model,newdata=new.pred.data)
    out <- exp(out)
  }
  if (! complete.cases(new.pred.data)) out <- NA
  names(out) <- element
  return(out)
}
