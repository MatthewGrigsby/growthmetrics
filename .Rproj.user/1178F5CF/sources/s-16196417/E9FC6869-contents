#' MSE
#'
#' Function for calculating subject-specific mean squared error
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @export
mse = function (observed="height", id.var="id", predicted="pred", time.var="time", data){

  id<-unique(data[[id.var]])
  data$t<-data[[time.var]]

  count=0
  mse.list<-NULL

    for (k in id){
      count=count+1
      current.mat=subset(data,id==k)
      mse= mean((current.mat$true-current.mat$pred)^2)
      mse.list[count]=mse
    }

  mse.result<-data.frame(id=id, mse=mse.list)
  return(mse.result)

}
