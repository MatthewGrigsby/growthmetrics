


library(mgcv)
library(refund)


dta.y<-read.table(file.choose(),sep=",",header=FALSE)
Y=as.matrix(dta.y)
n=nrow(Y)

month=0:15
hist.lgth<-7

Y.predict.BENDY<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
Y.predict.DLM<-matrix(nrow=n,ncol=length(month)-hist.lgth)
Y.predict.DPFR_gam<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
Y.predict.DPFR_pfr<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
Y.predict.DPFFR_gam<-matrix(nrow=n,ncol=length(month)-hist.lgth) 
Y.predict.DPFFR_pffr<-matrix(nrow=n,ncol=length(month)-hist.lgth) 


#BENDY
for(i in 1:n){
for(j in 1:(length(month)-hist.lgth)){
data.BENDY<-data.frame(Y[-i,hist.lgth+j], Y[-i,1],Y[-i,hist.lgth])
names(data.BENDY)<-c("y.BENDY",paste("y",c(1,hist.lgth),sep=""))
fit.BENDY<-lm(y.BENDY~., data=data.BENDY)
new.data.BENDY<-data.frame(Y[i,1],Y[i,hist.lgth])
names(new.data.BENDY)<-c(paste("y",c(1,hist.lgth),sep=""))
Y.predict.BENDY[i,j]<-predict(fit.BENDY, newdata=new.data.BENDY)
}}


#DLM
for(i in 1:n){
for(j in 1:(length(month)-hist.lgth)){
data.DLM<-data.frame(Y[-i,hist.lgth+j], cbind(Y[-i,1:hist.lgth]) )
names(data.DLM)<-c("y.DLM",paste("y",c(1:hist.lgth),sep=""))
fit.DLM<-lm(y.DLM~., data=data.DLM)
new.data.DLM<-data.frame(rbind(Y[i,1:hist.lgth]))
names(new.data.DLM)<-c(paste("y",c(1:hist.lgth),sep=""))
Y.predict.DLM[i,j]<-predict(fit.DLM, newdata=new.data.DLM)
}}


#DPFR using pfr
for(i in 1:n){
for(j in 1:(length(month)-hist.lgth)){
y.DPFR<-Y[-i,hist.lgth+j]
x.DPFR<-as.matrix(cbind(Y[-i,1:hist.lgth]))
fit.DPFR<-pfr(y.DPFR~lf(x.DPFR, k=4, bs="ps",argvals=as.vector(cbind(month[1:hist.lgth]
))))
Y.predict.DPFR_pfr[i,j]<-predict(fit.DPFR,        newdata=list(x.DPFR=as.matrix(cbind(t(Y[i,1:hist.lgth])))),type="response") 
}}


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
}}


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


