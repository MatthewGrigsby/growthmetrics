#' DLM
#'
#'DLM uses more information than BENDY. While BENDY uses Y[i,1] and Y[i,t*] for dynamic prediction,
#'DLM includes data Y[i,1:t*] which contains all the known history before time t* for a subject i. The DLM
#'model fit is done at each point t* + j.
#'
#' @param data A matrix of values with each row representing an individual and each column representing measurements at each time point (e.g. column 1 is time point 1, etc.). Measurements are assumed to be taken at the same time intervals for every participant.
#' @param time A vector of times. This equal the number of columts in the matrix provided (e.g. 0 through 15 months for the example dataset)
#' @param hist.lngth The length of known history for the observed process. We use leave one-curve out cross validation for prediction.
#'
#' @return A matrix of predictions with rows representing each individual and columts representing predictions for each time point.
#'
#' @import mgcv refund
#'
#' @references Ivanescu AE, Crainiceanu CM, Checkley W. Dynamic child growth prediction: A comparative methods approach. Statistical Modelling. 2017 Dec;17(6):468-93.
#'
#'
#' @export
dlm <- function(data, months=c(0:15), hist.lngth=7){
  #DLM
  for(i in 1:n){
    for(j in 1:(length(month)-hist.lgth)){
      data.DLM<-data.frame(Y[-i,hist.lgth+j], cbind(Y[-i,1:hist.lgth]) )
      names(data.DLM)<-c("y.DLM",paste("y",c(1:hist.lgth),sep=""))
      fit.DLM<-lm(y.DLM~., data=data.DLM)
      new.data.DLM<-data.frame(rbind(Y[i,1:hist.lgth]))
      names(new.data.DLM)<-c(paste("y",c(1:hist.lgth),sep=""))
      Y.predict.DLM[i,j]<-predict(fit.DLM, newdata=new.data.DLM)
    }
  }
  return(Y.predict.DLM)
}
