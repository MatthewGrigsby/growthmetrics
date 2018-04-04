#' MSE
#'
#' Function for calculating subject-specific mean squared error
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#'
#' @return data.frame with id and subject-specific MSE estimates
#'
#' @references Grigsby MR, Di J, Leroux A, Zipunnikov V, Xiao L, Crainiceanu C, Checkley W. Novel metrics for growth model selection. Emerging themes in epidemiology. 2018 Feb;15(1):4.
#'
#' @export
mse = function (observed="observed", predicted="pred", id.var="id", data){

  id<-unique(data[[id.var]])
  data$t<-data[[time.var]]

  count=0
  mse.list<-NULL

    for (k in id){
      count=count+1
      current.mat=subset(data,id==k)
      mse= mean((current.mat[[observed]]-current.mat[[predicted]])^2)
      mse.list[count]=mse
    }

  mse.result<-data.frame(id=id, mse=mse.list)
  return(mse.result)

}
