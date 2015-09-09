#' predict pollution exposure
#'
#' This function is an implementation of the elemental PM exposure (random forest and regression) models developed for the Cincinnati Children's Asthma and Air Pollution Study (CCAAPS).  The underlying functions are not available to the user.  Not meant to be a generalizable package, its sole purpose is to generate exposure estimates for the Cincinnati area.
#' @param loc the location for which to estimate the concentration (must be a spatial object and have a valid proj4string)
#' @param element element for which to predict the concentration (one of "Cu", "Fe", "Zn", "S", "Ni", "V", "Si", "K", "Pb", "Mn", "Al", "TRAP", "PM25")
#' @param model.type either "rf" for random forest or "lm" for regression
#' @param progress.bar logical, show a progress bar?
#' @export
#' @examples
#' sample.loc <- data.frame('x'=-84.5371597,'y'=39.1603015)
#' coordinates(sample.loc) <- c('x','y')
#' sp::proj4string(sample.loc) <- CRS("+init=epsg:4326")
#' predictPollution(loc=sample.loc,element='TRAP',model.type='rf')

predictPollution <- function(loc,element,model.type,progress.bar=TRUE) {
  stopifnot(model.type %in% c('rf','lm'))
  stopifnot(element %in% names(LU.final.lm.models))
  final.model <- switch(model.type,
                        rf = LU.final.rf.models[[element]],
                        lm = LU.final.lm.models[[element]])
  final.model.predictors <- switch(model.type,
                                   rf = row.names(randomForest::importance(final.model)),
                                   lm = names(coef(final.model))[-1])
  new.pred.data <- all_lu_data_gen(loc=loc,final.model.predictor.names=final.model.predictors,
                                   progress.bar=progress.bar)
  out <- predict(final.model,newdata=new.pred.data)
  out <- exp(out)
  names(out) <- element
  return(out)
}
