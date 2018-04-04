#' wMSE
#'
#' Function for weighting MSE, nMSE, or aMSE by the specified variable for weights.
#'
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#' @param weight.var Variable used to weight MSE or nMSE. This should be a vector of values that will be used to divide subject-specific MSE estimates by. An example could be using subject-specific growth trajectories (i.e. weighting individuals with slowest growth).
#' @param type Type of MSE estimate used as denominator. Default is nMSE but can be se to standard MSE.
#'
#' @return data.frame with id and subject-specific weighted MSE or nMSE estimates
#'
#' @references Grigsby MR, Di J, Leroux A, Zipunnikov V, Xiao L, Crainiceanu C, Checkley W. Novel metrics for growth model selection. Emerging themes in epidemiology. 2018 Dec;15(1):4.
#'
#' @export
wmse = function (observed="observed", predicted="pred", id.var="id", weight.var="weights", type="nmse", data){

  count=0
  wmse.list<-NULL

  for (k in unique(data[[id.var]])){
    count=count+1
    current.mat=subset(data,id==k)
    if (type=="nmse"){
      a = ((current.mat[[observed]] - current.mat[[predicted]])^2 /(current.mat[[observed]])^2)/current.mat[[weight.var]]
      }
    if (type=="mse"){
      a = ((current.mat[[observed]]-current.mat[[predicted]])^2) /current.mat[[weight.var]]
      }
    wmse = mean(a)
    wmse.list[count]=wmse
  }

  wmse.result<-data.frame(id=id, wmse=wmse.list)
  return(wmse.result)

}
