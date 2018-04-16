#' DPFR using gam
#'
#'DPFR uses penalized functional regression to incorporate all the history of Y up to time t* for subject i in
#'a scalar-on-function regression. At each t* + j the response for subject i is the scalar Y[i,t*+j] and the
#'functional predictor data consists of Y[i,1:t*]. We show how to obtain DPFR dynamic prediction using
#'the function pfr from the refund R package and the gam function from the mgcv R package.
#'
#'Another option for fitting DPFR instead of using pfr, is using the gam function from the mgcv R package. To use gam, there is a
#'step that involves arranging the data in some specific form prior to calling the gam function for DPFR model
#'fitting.
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
dpfr_gam <- function(data, months=c(0:15), hist.lngth=7){

  Y=as.matrix(data)
  n=nrow(Y)
  month=months
  hist.lgth<-hist.lngth
  Y.predict.DPFR_gam<-matrix(nrow=n,ncol=length(month)-hist.lgth)

  #DPFR using gam
  for(i in 1:n){
    for(j in 1:(length(month)-hist.lgth)){
      y.s<-Y[-i,hist.lgth+j]
      X <- Y[-i,1:hist.lgth]
      Lmat <- X[rep(1:(n-1),each=1),]
      sngrid=hist.lgth
      smat <- matrix(month[1:hist.lgth], nrow=n-1, nc=sngrid, byrow=TRUE)
      data.DPFR<-list()
      data.DPFR$y.s<-y.s
      data.DPFR$smat<-smat
      data.DPFR$Lmat<-Lmat
      fit.dpfr <- gam(y.s~ s(smat,by=Lmat,bs="ps",k=4),data=data.DPFR ,method="REML")
      smat.new <- matrix(month[1:hist.lgth], nrow=1, nc=sngrid, byrow=TRUE)
      X.new <- as.matrix(t(Y[i,1:hist.lgth]),nrow=1)
      Lmat.new <-  t(X.new[rep(1:1,each=1),])
      data.DPFR.new<-list()
      data.DPFR.new$smat<-smat.new
      data.DPFR.new$Lmat<-Lmat.new
      DPFR.gam<-predict(fit.dpfr,data.DPFR.new,se=TRUE)
      Y.predict.DPFR_gam[i,j]<-DPFR.gam$fit
    }
  }
  return(Y.predict.DPFR_gam)
}
