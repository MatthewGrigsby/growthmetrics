#' wMSE
#'
#' Function for weighting MSE, nMSE, or aMSE by the specified variable for weights.
#'
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#' @param time.var time variable (e.g. age) used when calculating nMSE and wMSE
#' @param weight.var Variable used to weight MSE or nMSE. This should be a vector of values that will be used to divide subject-specific MSE estimates by. An example could be using subject-specific growth trajectories (i.e. weighting individuals with slowest growth).
#'
#' @return None
#'
#' @export
wmse = function (id.var="id", time.var="time", weight.var="weights", type="mse", data){

  id<-unique(data[[id.var]])
  data$t<-data[[time]]

  count=0
  wmse.list<-NULL

  for (k in id){
    count=count+1
    current.mat=subset(data,id==k)
    if (type==mse){a = current.mat[[true]]/current.mat[[weight.var]]}
    if (type==nmse){a = current.mat[[true]]/current.mat[[weight.var]]}
    wmse = mean(a)
    wmse.list[count]=wmse
  }

  wmse.result<-data.frame(id=id, wmse=wmse.list)
  return(wmse.result)

}
