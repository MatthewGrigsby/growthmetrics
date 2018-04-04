#' nMSE
#'
#' Function for calculating subject-specific location-normalized mean squared error
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#' @param time.var time variable (e.g. age) used when calculating nMSE and wMSE
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
