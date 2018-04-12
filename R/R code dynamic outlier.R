
library(mgcv)
library(refund)


dta.y<-read.table(file.choose(),sep=",",header=FALSE)
Y=as.matrix(dta.y)
n=nrow(Y)

month=0:15
hist.lgth<-7

Y.predict.BENDY<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
Y.predict.DLM<-matrix(nrow=n,ncol=length(month)-hist.lgth)
Y.predict.DPFR<-matrix(nrow=n,ncol=length(month)-hist.lgth)  
Y.predict.DPFFR<-matrix(nrow=n,ncol=length(month)-hist.lgth) 


z_score.BENDY<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
z_score.DLM<-matrix(nrow=n,ncol=length(month)-hist.lgth)
z_score.DPFR<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
z_score.DPFFR<-matrix(nrow=n,ncol=length(month)-hist.lgth) 


Fit.haz.pca=fpca.sc(Y,var=TRUE, simul=FALSE,nbasis=6)
VarE<-Fit.haz.pca$sigma2


#BENDY
for(i in 1:n){
for(j in 1:(length(month)-hist.lgth)){
data.BENDY<-data.frame(Y[-i,hist.lgth+j], Y[-i,1],Y[-i,hist.lgth])
names(data.BENDY)<-c("y.BENDY",paste("y",c(1,hist.lgth),sep=""))
fit.BENDY<-lm(y.BENDY~., data=data.BENDY)
new.data.BENDY<-data.frame(Y[i,1],Y[i,hist.lgth])
names(new.data.BENDY)<-c(paste("y",c(1,hist.lgth),sep=""))
pred.BENDY<-predict(fit.BENDY, newdata=new.data.BENDY,se.fit=TRUE)
Y.predict.BENDY[i,j]<-pred.BENDY$fit
V_z_BENDY<-VarE+(pred.BENDY$se.fit)^2
z_score.BENDY[i,j]<-(Y[i,hist.lgth+j]-Y.predict.BENDY[i,j])/sqrt(V_z_BENDY)
}}




#DLM
for(i in 1:n){
for(j in 1:(length(month)-hist.lgth)){
data.DLM<-data.frame(Y[-i,hist.lgth+j], cbind(Y[-i,1:hist.lgth]) )
names(data.DLM)<-c("y.DLM",paste("y",c(1:hist.lgth),sep=""))
fit.DLM<-lm(y.DLM~., data=data.DLM)
new.data.DLM<-data.frame(rbind(Y[i,1:hist.lgth]))
names(new.data.DLM)<-c(paste("y",c(1:hist.lgth),sep=""))
pred.DLM<-predict(fit.DLM, newdata=new.data.DLM,se.fit=TRUE)
Y.predict.DLM[i,j]<-pred.DLM$fit
V_z_DLM<-VarE+(pred.DLM$se.fit)^2
z_score.DLM[i,j]<-(Y[i,hist.lgth+j]-Y.predict.DLM[i,j])/sqrt(V_z_DLM)
}}






#DPFR 
for(i in 1:n){
for(j in 1:(length(month)-hist.lgth)){
y.DPFR<-Y[-i,hist.lgth+j]
x.DPFR<-as.matrix(cbind(Y[-i,1:hist.lgth]))
fit.DPFR<-pfr(y.DPFR~lf(x.DPFR, k=4, bs="ps",argvals=as.vector(cbind(month[1:hist.lgth]
))))

Y.predict.DPFR[i,j]<-predict(fit.DPFR,newdata=list(x.DPFR=as.matrix(cbind(t(Y[i,1:hist.lgth])))),type="response") 


predict.DPFR.LP<-predict(fit.DPFR, newdata=list(x.DPFR=as.matrix(cbind(t(Y[i,1:hist.lgth])))),type='lpmatrix')

predVar.DPFR<-predict.DPFR.LP%*%fit.DPFR$Vp%*%t(predict.DPFR.LP)
VarTOT.DPFR<-VarE+diag(predVar.DPFR)
z_score.DPFR[i,j]<-(Y[i,hist.lgth+j]-Y.predict.DPFR[i,j])/sqrt(VarTOT.DPFR)

}}



#DPFFR 
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
Y.predict.DPFFR[i,]<-predict.GAM$fit

predict.LP<-predict(fit.DPFFR,data.DPFFR.new,type="lpmatrix")
predVar<-predict.LP%*%fit.DPFFR$Vp%*%t(predict.LP)
VarF<-VarE+diag(predVar)
z_score.DPFFR[i,]<-(Y[i,((hist.lgth+1):length(month))]-Y.predict.DPFFR[i,])/sqrt(VarF)

}



par(mfrow=c(2,2))

plot(((hist.lgth+1):length(month)), z_score.BENDY[1,],col="light gray",ylim=c(-5,5),type="l",ylab="BENDY z-score",xlab="month"); 
for(i in 1:n){
	lines(((hist.lgth+1):length(month)),z_score.BENDY[i,],col="light gray")
}

plot(((hist.lgth+1):length(month)), z_score.DLM[1,],col="light gray",ylim=c(-5,5),type="l",ylab="DLM z-score",xlab="month"); 
for(i in 1:n){
	lines(((hist.lgth+1):length(month)),z_score.DLM[i,],col="light gray")
}

plot(((hist.lgth+1):length(month)), z_score.DPFR[1,],col="light gray",ylim=c(-5,5),type="l",ylab="DPFR z-score",xlab="month"); 
for(i in 1:n){
	lines(((hist.lgth+1):length(month)),z_score.DPFR[i,],col="light gray")
}

plot(((hist.lgth+1):length(month)), z_score.DPFFR[1,],col="light gray",ylim=c(-5,5),type="l",ylab="DPFFR z-score",xlab="month"); 
for(i in 1:n){
	lines(((hist.lgth+1):length(month)),z_score.DPFFR[i,],col="light gray")
}
