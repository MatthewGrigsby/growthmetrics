#' wMSE
#'
#' Function for weighting MSE, nMSE, or aMSE by the specified variable for weights.
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
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
    if (type==wmse){a = current.mat[[true]]/current.mat[[weight.var]]}
    wmse = mean(a)
    wmse.list[count]=wmse
  }

  wmse.result<-data.frame(id=id, wmse=wmse.list)
  return(wmse.result)

}
