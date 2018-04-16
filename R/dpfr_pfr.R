#' DPFR using pfr
#'
#'DPFR uses penalized functional regression to incorporate all the history of Y up to time t* for subject i in
#'a scalar-on-function regression. At each t* + j the response for subject i is the scalar Y[i,t*+j] and the
#'functional predictor data consists of Y[i,1:t*]. We show how to obtain DPFR dynamic prediction using
#'the function pfr from the refund R package and the gam function from the mgcv R package.
#'
#'DPFR can use pfr for model fitting. The function pfr from the refund R package directly takes on a scalar
#'response Y[i,t*+j] and a functional predictor Y[i,1:t*].
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
dpfr_pfr <- function(data, months=c(0:15), hist.lngth=7){

  Y=as.matrix(data)
  n=nrow(Y)
  month=months
  hist.lgth<-hist.lngth
  Y.predict.DPFR_pfr<-matrix(nrow=n,ncol=length(month)-hist.lgth)

  #DPFR using pfr
  for(i in 1:n){
    for(j in 1:(length(month)-hist.lgth)){
      y.DPFR<-Y[-i,hist.lgth+j]
      x.DPFR<-as.matrix(cbind(Y[-i,1:hist.lgth]))
      fit.DPFR<-pfr(y.DPFR~lf(x.DPFR, k=4, bs="ps",argvals=as.vector(cbind(month[1:hist.lgth]
      ))))
      Y.predict.DPFR_pfr[i,j]<-predict(fit.DPFR, newdata=list(x.DPFR=as.matrix(cbind(t(Y[i,1:hist.lgth])))),type="response")
    }
  }
  return(Y.predict.DPFR_pfr)
}
