#' aMSE
#'
#' Function for calculating age-stratified subject-specific mean squared error
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#' @param time.var time variable (e.g. age)
#' @param breaks vector indicating location of age cutoffs for calculating MSE per group (default is in months)
#'
#' @return data.frame with id, time group, and subject-specific MSE estimates
#'
#' @import dplyr reshape
#'
#' @references Grigsby MR, Di J, Leroux A, Zipunnikov V, Xiao L, Crainiceanu C, Checkley W. Novel metrics for growth model selection. Emerging themes in epidemiology. 2018 Dec;15(1):4.
#'
#'
#' @export
amse = function(observed="observed", predicted="pred", id.var="id", time.var="time", breaks=c(0,6,12,18,24), data){

  data$breaks <- cut(dat[[time.var]], breaks=breaks)
  mse.result <- summarise(group_by(data, id, breaks), mse = mean((observed-pred)^2))

  return(mse.result)

}
