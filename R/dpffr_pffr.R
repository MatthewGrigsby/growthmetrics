#' DPFFR using pffr
#'
#'DPFFR considers a functional response Y[i,(t*+1):length(month)] for each subject i. The predictor,
#'Y[i,1:t*], is also functional, which makes the approach a dynamic function-on-function regression. We
#'provide the implementation of DPFFR with the pffr function from the refund R package and the gam function
#'from the mgcv R package.
#'
#'When using pffr from the refund R package, the user directly refers to the functional response
#'Y[i,(t*+1):length(month)] and functional predictor Y[i,1:t*]. The response and predictor data are
#'stored as matrices.
#'
#' @param data A matrix of values with each row representing an individual and each column representing measurements at each time point (e.g. column 1 is time point 1, etc.). Measurements are assumed to be taken at the same time intervals for every participant.
#' @param time A vector of times. This equal the number of columts in the matrix provided (e.g. 0 through 15 months for the example dataset).
#' @param hist.lngth The length of known history for the observed process. We use leave one-curve out cross validation for prediction (hist.lgth is 7 for the example dataset).
#'
#' @return A matrix of predictions with rows representing each individual and columts representing predictions for each time point.
#'
#' @import mgcv refund
#'
#' @references Ivanescu AE, Crainiceanu CM, Checkley W. Dynamic child growth prediction: A comparative methods approach. Statistical Modelling. 2017 Dec;17(6):468-93.
#'
#'
#' @export
dpffr_pffr <- function(data=HAZ, time=0:15, hist.lngth=7){

  Y=as.matrix(data)
  n=nrow(Y)
  Y.predict.DPFFR_pffr<-matrix(nrow=n,ncol=length(month)-hist.lgth)

  #DPFFR using pffr
  for(i in 1:n){
    ymat<-Y[-i,(hist.lgth+1):length(month)]
    X<-Y[-i,1:hist.lgth]
    t.vec<-month[(hist.lgth+1):length(month)]
    s.vec<-month[1:hist.lgth]
    data1<-list()
    data1$ymat<-ymat
    data1$X<-X
    data1$t.vec<-t.vec
    data1$s.vec<-s.vec
    fit.DPFFR_pffr<-pffr(ymat~ff(X,xind=s.vec), yind=t.vec, data=data1)
    data2<-list()
    data2$X<-as.matrix(t(Y[i,1:hist.lgth]))
    Y.predict.DPFFR_pffr[i,]<-predict(fit.DPFFR_pffr, newdata=data2)
  }
  return(Y.predict.DPFFR_pffr)
}
