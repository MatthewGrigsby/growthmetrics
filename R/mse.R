#' MSE
#'
#' Function for calculating subject-specific mean squared error
#'
#' @param observed observed growth values (e.g. height or weight)
#' @param predicted predicted values from models fitted to observed data
#' @param id.var variable that identifies individual subjects
#'
#' @return None
#'
#' @export
mse = function (observed="observed", id.var="id", predicted="pred", time.var="time", data){

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
