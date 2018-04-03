#' aMSE
#'
#' Function for calculating age-stratified mean squared error
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @export
amse = function(observed="height", id.var="id", predicted="pred", time.var="time", data){

  id<-unique(data[[id.var]])

  count=0
  mse6<-NULL
  mse12<-NULL
  mse18<-NULL
  mse24<-NULL

  for (k in id){

    count=count+1
    current.mat=subset(data,id==k)

    age.6 = subset(current.mat, time.var<=6)
    age.12 = subset(current.mat, time.var>6 & time.var<=12)
    age.18 = subset(current.mat, time.var>12 & time.var<=18)
    age.24 = subset(current.mat, time.var>18)

    mse.6= mean((age.6$true-age.6$pred)^2)
    mse.12= mean((age.12$true-age.12$predicted)^2)
    mse.18= mean((age.18$true-age.18$predicted)^2)
    mse.24= mean((age.24$true-age.24$predicted)^2)

    mse6[count] = mse.6
    mse12[count] = mse.12
    mse18[count] = mse.18
    mse24[count] = mse.24

  }

  mse.result<-data.frame(id=id, mse.6=mse6, mse.12=mse12, mse.18=mse18, mse.24=mse24)
  return(mse.result)

}