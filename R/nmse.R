#' nMSE
#'
#' Function for calculating subject-specific location-normalized mean squared error
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @export
nmse = function (observed="height", id.var="id", predicted="pred", time.var="time", data){

  id<-unique(data[[id]])
  data$t<-data[[time]]

  count=0
  nmse.list<-NULL

  for (k in id){

    count=count+1
    current.mat=subset(data,id==k)
    a = (current.mat$observed - current.mat$predicted)^2/(current.mat$observed)^2
    nmse = mean(a)
    nmse.list[count]=nmse

  }

  nmse.result <- data.frame(id=id, nmse=nmse.list)
  return(nmse.result)

}
