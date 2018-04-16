#' DPFFR using gam
#'
#'DPFFR considers a functional response Y[i,(t*+1):length(month)] for each subject i. The predictor,
#'Y[i,1:t*], is also functional, which makes the approach a dynamic function-on-function regression. We
#'provide the implementation of DPFFR with the pffr function from the refund R package and the gam function
#'from the mgcv R package.
#'
#'Using gam from the mgcv R package is an option for DPFFR. When using gam for DPFFR there is a required
#'step of having the functional response Y[i,(t*+1):length(month)] in vector format. Additional steps are
#'needed to generate the required data format for gam. Details are shown below.
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
dpffr_gam <- function(data=HAZ, time=0:15, hist.lngth=7){

  Y=as.matrix(data)
  n=nrow(Y)
  Y.predict.DPFFR_gam<-matrix(nrow=n,ncol=length(month)-hist.lgth)

  #DPFFR using gam
  for(i in 1:n){
    Y.v<-Y[,(hist.lgth+1):length(month)]
    X<-Y[,1:hist.lgth]
    yvec <- as.vector(t(Y.v[-i,]))
    t<-month[(hist.lgth+1):length(month)]
    s<-month[1:hist.lgth]
    by=1
    tngrid=length(t)
    sngrid=length(s)
    tmat <- matrix(t, nrow=(n-1)*tngrid, nc=sngrid,byrow=FALSE)
    smat <- matrix(s, nrow=(n-1)*tngrid, nc=sngrid, byrow=TRUE)
    LX <- X[-i,]
    Lmat <- LX[rep(1:(n-1),each=tngrid),]
    tvec <- matrix(t, nrow=(n-1)*tngrid, nc=1,byrow=FALSE)
    data.DPFFR<-list()
    data.DPFFR$yvec<-yvec
    data.DPFFR$tvec<-tvec
    data.DPFFR$tmat<-tmat
    data.DPFFR$smat<-smat
    data.DPFFR$Lmat<-Lmat
    fit.DPFFR <- gam(yvec ~ s(tvec,bs="ps",k=4)+te(tmat,smat,by=Lmat, bs="ps"),method="REML")
    tmat.new <- matrix(t, nrow=(1)*tngrid, nc=sngrid,byrow=FALSE)
    smat.new <- matrix(s, nrow=(1)*tngrid, nc=sngrid, byrow=TRUE)
    L <- matrix(by, ncol=length(s), nrow=1)
    LX.new <- L*X[i,]
    Lmat.new <- LX.new[rep(1:(1),each=tngrid),]
    tvec.new <- matrix(t, nrow=(1)*tngrid, nc=1,byrow=FALSE)
    data.DPFFR.new<-list()
    data.DPFFR.new$tvec<-tvec.new
    data.DPFFR.new$tmat<-tmat.new
    data.DPFFR.new$smat<-smat.new
    data.DPFFR.new$Lmat<-Lmat.new
    predict.GAM<-predict(fit.DPFFR,data.DPFFR.new,se=TRUE)
    Y.predict.DPFFR_gam[i,]<-predict.GAM$fit
  }
  return(Y.predict.DPFFR_gam)
}
