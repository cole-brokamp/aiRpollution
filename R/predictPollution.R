predictPollution <- function(loc,element,model.type) {
  stopifnot(model.type %in% c('rf','lm'))
  stopifnot(element %in% names(LU.final.lm.models))
  final.model <- switch(model.type,
                        rf = LU.final.rf.models[[element]],
                        lm = LU.final.lm.models[[element]])
  final.model.predictors <- switch(model.type,
                                   rf = row.names(importance(final.model)),
                                   lm = names(coef(final.model))[-1])
  new.pred.data <- all_lu_data_gen(loc=loc,final.model.predictor.names=final.model.predictors)
  out <- predict(final.model,newdata=new.pred.data)
  out <- exp(out)
  names(out) <- element
  return(out)
}
