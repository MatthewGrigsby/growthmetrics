#' nMSE
#'
#' Function for calculating subject-specific location-normalized mean squared error
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#'
#' @return data.frame with id and subject-specific location-normalized MSE estimates
#'
#' @references Grigsby MR, Di J, Leroux A, Zipunnikov V, Xiao L, Crainiceanu C, Checkley W. Novel metrics for growth model selection. Emerging themes in epidemiology. 2018 Feb;15(1):4.
#'
#' @export
nmse = function (observed="observed", predicted="pred", id.var="id", data){

  count=0
  nmse.list<-NULL

  for (k in unique(data[[id.var]])){

    count=count+1
    current.mat=subset(data,id==k)
    a = (current.mat[[observed]] - current.mat[[predicted]])^2/(current.mat[[observed]])^2
    nmse = mean(a)
    nmse.list[count]=nmse

  }

  nmse.result <- data.frame(id=id, nmse=nmse.list)
  return(nmse.result)

}
