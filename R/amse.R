#' aMSE
#'
#' Function for calculating age-stratified subject-specific mean squared error
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#' @param time.var time variable (e.g. age) used when calculating nMSE and wMSE (default is in months)
#' @param breaks vector indicating location of age cutoffs for calculating MSE per group (default is in months)
#'
#' @return None
#'
#' @import dplyr reshape
#'
#' @export
amse = function(observed="observed", predicted="pred", id.var="id", time.var="time", breaks=c(0,6,12,18,24), data){

  data$breaks <- cut(dat[[time.var]], breaks=breaks)
  mse.result <- summarise(group_by(data, id, breaks), mse = mean((observed-pred)^2))

  return(mse.result)

}
