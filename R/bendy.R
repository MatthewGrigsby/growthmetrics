#' BENDY
#'
#' Dynamic predictions with BENDY are obtained for each subject at each time point t* + j. For all subjects
#'except subject i we consider data Y[-i,1] and Y[-i,t*], corresponding to first and last HAZ data from the
#'known HAZ history, as predictive data for BENDY. We use all except the i-th subject to obtain the BENDY
#'model fit, because we perform the leave one-curve out cross validation for prediction. The BENDY model fit
#'is done n times to account for dynamic prediction for all n subjects.
#'The lm function in R is used to fit the BENDY model. For each dynamic prediction, a BENDY model fit is
#'used. Model fit and prediction are needed for each j for dynamic prediction at times t* + j .
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
bendy <- function(data, time=c(0:15), hist.lngth=7){

  Y=as.matrix(data)
  n=nrow(Y)
  time=months
  hist.lgth<-hist.lngth
  Y.predict.BENDY<-matrix(nrow=n,ncol=length(month)-hist.lgth)

  #BENDY
  for(i in 1:n){
    for(j in 1:(length(month)-hist.lgth)){
      data.BENDY<-data.frame(Y[-i,hist.lgth+j], Y[-i,1],Y[-i,hist.lgth])
      names(data.BENDY)<-c("y.BENDY",paste("y",c(1,hist.lgth),sep=""))
      fit.BENDY<-lm(y.BENDY~., data=data.BENDY)
      new.data.BENDY<-data.frame(Y[i,1],Y[i,hist.lgth])
      names(new.data.BENDY)<-c(paste("y",c(1,hist.lgth),sep=""))
      Y.predict.BENDY[i,j]<-predict(fit.BENDY, newdata=new.data.BENDY)
    }
  }

  return(Y.predict.BENDY)

}
