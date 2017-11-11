##################
## Preprocessiong
##################

library(lubridate)  # to deal with Dates in R
library(lattice)
setwd("~/Desktop/374 project")

save("ptrain",file="ptrain.Rdata")
dept.names = sort(unique(mytrain$dept))
k=4; # plot the dept with name dept.names[k], k=1, .., 81. 

pdf(file="dept=4, each store.pdf",height=6,width=9)
day1="2010-02-05"
tmp=subset(mytrain, dept==dept.names[k])
tmp$wk_index = as.numeric(difftime(tmp$date, day1, units="weeks"))
xyplot(sales ~ wk_index | store, tmp, par.strip.text=list(cex=0.75), pch=".", 
       strip = strip.custom(strip.names = FALSE, strip.levels = TRUE), 
       main=paste("Dept = ", dept.names[k]))
dev.off()

zbmtrain = mytrain[mytrain$store==23,]
zbmtrain$weeklabel = as.numeric(round(difftime(zbmtrain$date,zbmtrain$date[1],units="week"))+1)
ptrain = zbmtrain[,!colnames(zbmtrain)%in%c("store","size","type")]
save("ptrain",file="ptrain.Rdata")
ptrain$logsales = log(ptrain$sales+299)

############
## outliers
############

i = 2
plot(ptrain[ptrain$date==ptrain$date[i],"logsales"]~ptrain[ptrain$date==ptrain$date[i],"dept"])
dept3 = ptrain[ptrain$dept==3,]
salesbar = aggregate(ptrain$logsales,by=list(date=ptrain$date),mean)

######################
### test and training
######################

pdept.names = unique(ptrain$dept)
dept.len = c()
for(i in 1:length(pdept.names)){
  dept.len = c(dept.len, nrow(ptrain[ptrain$dept==pdept.names[i],]))
}
outlier = cbind(pdept.names,dept.len)
write.csv(outlier,file="outlier.txt",row.names=F)

ptrain = ptrain[! ptrain$dept %in% pdept.names[which(dept.len <= 60)],]

save("ptrain",file="ptrain.Rdata") #delete outliers

pdept.names = unique(ptrain$dept)
dept.len = c()
for(i in 1:length(pdept.names)){
  dept.len = c(dept.len, nrow(ptrain[ptrain$dept==pdept.names[i],]))
}
dept.len

###################################################
#### randomly take 1/10 data to form test dataset
###################################################

trainset = c()
testset = c()
for(i in 1:length(pdept.names)){
  x = ptrain[ptrain$dept==pdept.names[i],]
  nx = nrow(x)
  t = sample(1:nx,size=round(nx/10),replace=F)
  trainset = rbind(trainset, x[-t,])
  testset = rbind(testset, x[t,])
}

save(trainset,file="trainset.Rdata")
save(testset,file="testset.Rdata")
load("testset.Rdata")
load("trainset.Rdata")

##################
## additive model
##################

library(gam)
require(mgcv)

gamtrain1 = gam(logsales ~ s(temp) + s(fuel) + s(cpi) + s(unemp) + as.factor(dept) + 
                  as.factor(month) + as.factor(wk) + (temp):as.factor(dept) + 
                  (fuel):as.factor(dept) + (cpi):as.factor(dept) + (unemp):as.factor(dept) + 
                  (fuel):as.factor(month) + (cpi):as.factor(month) + (unemp):as.factor(month), 
                data = trainset)

gamtest = predict(gamtrain1,testset)
error.gam = gamtest - testset$logsales
RMSLE.gam = sqrt(t(error.gam)%*%error.gam/length(error.gam))
cat("RMSLE.gam =",RMSLE.gam)

###########################
### local linear regression
###########################

library(locfit)
loctrain = locfit(logsales~(temp)+(unemp)+(cpi)+(dept),deg=1,data=trainset,alpha=c(0.1,0.1,0.1,0.1,0.1),kern="gauss",maxk=10000)
loctest = predict(loctrain,testset)
error.loc = loctest - testset$logsales
RMSLE.loc = sqrt(t(error.loc)%*%error.loc/length(error.loc))
cat("RMSLE.loc =",RMSLE.loc)

##################
## linear model
##################

trainlm = lm(logsales~temp + fuel + cpi + unemp + as.factor(dept) +as.factor(year) + as.factor(month) + 
          as.factor(holiday)+
          temp*as.factor(dept)+fuel*as.factor(dept) + cpi*as.factor(dept) + unemp*as.factor(dept)+ 
          temp*as.factor(year) + fuel*as.factor(year) + cpi*as.factor(year) + unemp*as.factor(year) +
          temp*as.factor(month) + fuel*as.factor(month) + cpi*as.factor(month) + unemp*as.factor(month) +
          temp*as.factor(holiday) 
        ,  data=trainset )

lmtest = predict(trainlm,testset)
error.lm = lmtest - testset$logsales
RMSLE.lm = sqrt(t(error.lm)%*%error.lm/length(error.lm))
cat("RMSLE.lm =",RMSLE.lm)

train.lm2 = lm(formula = logsales ~ temp + fuel + cpi + unemp + as.factor(dept) + 
                 as.factor(month) + as.factor(wk) + temp:as.factor(dept) + 
                 fuel:as.factor(dept) + cpi:as.factor(dept) + unemp:as.factor(dept) + 
                 fuel:as.factor(month) + cpi:as.factor(month) + unemp:as.factor(month), 
               data = trainset)
lmtest2 = predict(train.lm2,testset)
error.lm2 = lmtest2 - testset$logsales
RMSLE.lm2 = sqrt(t(error.lm2)%*%error.lm2/length(error.lm2))
cat("RMSLE.lm2 =",RMSLE.lm2)

####################################
## confidence band of linear model
####################################

a=predict(train.lm2,trainset[which(trainset$dept==1),],interval="prediction",level=0.95)
FF=as.vector(a[,1])
LL=as.vector(a[,2])
UU=as.vector(a[,3])
x=trainset$weeklabel[trainset$dept==1]
y=trainset$logsales[trainset$dep==1]

plot(x,FF,ylim=c(min(LL),max(UU)),"n")
polygon(c(x,rev(x)),c(LL,rev(UU)),col="grey")
lines(x,FF,ylim=c(min(LL),max(UU)),"l",col="darkred",lwd=2)

A=cbind(x,y,FF,LL,UU)
save(A,file="data/A.Rdata")
load("data/A.Rdata")
p2=ggplot(data.frame(cbind(x,FF)),aes(x=x,y=FF))+
  geom_ribbon(data=data.frame(x), aes(x, ymin=LL, ymax=UU),fill="skyblue",inherit.aes=F, alpha=0.8)+geom_point( )+geom_line()+
  geom_point(aes(x=x,y=y),color="red")+ xlab("week")+ylab("logsales")
p2

################################
##### test data confidence band
################################

b = predict(trainlm,testset[which(testset$dept==1),],interval="prediction",level=0.95)
FFb = as.vector(b[,1])
LLb = as.vector(b[,2])
UUb = as.vector(b[,3])
xb = testset$weeklabel[testset$dept==1]
yb = testset$logsales[testset$dep==1]
p2=ggplot(data.frame(cbind(xb,FFb)),aes(x=xb,y=FFb))+
  geom_ribbon(data=data.frame(xb), aes(xb, ymin=LLb, ymax=UUb),fill="skyblue",inherit.aes=F, alpha=0.8)+geom_point( )+geom_line()+
  geom_point(aes(x=xb,y=yb),color="red")+ xlab("week")+ylab("logsales")
p2

##################################
##### CI of additive model 
##################################

B = 200
lendep1 = length(which(trainset$dept==1))
yhat.train = matrix(0, nrow=lendep1, ncol=B)
RMSLE.boot = numeric(B)
for(b in 95:B){
  nn = sample(1:nrow(trainset),replace=TRUE)
  X = trainset[nn,]
  gamboot = gam(logsales ~ s(temp) + s(fuel) + s(cpi) + s(unemp) + as.factor(dept) + 
        as.factor(month) + as.factor(wk) + (temp):as.factor(dept) + 
        (fuel):as.factor(dept) + (cpi):as.factor(dept) + (unemp):as.factor(dept) + 
        (fuel):as.factor(month) + (cpi):as.factor(month) + (unemp):as.factor(month), 
      data = X)
  yhat.train[,b] = predict(gamboot, trainset[which(trainset$dept==1),])
  yhat.test = predict(gamboot,testset)
  error.boot = yhat.test - testset$logsales
  RMSLE.boot[b] = sqrt(t(error.boot)%*%error.boot/length(error.boot))
}

save(yhat.train,file="data/yhat.train.Rdata")
save(RMSLE.boot,file="data/RMSLE.boot.Rdata")
load("data/yhat.train.Rdata")
load("data/RMSLE.boot.Rdata")


library(ggplot2)
p = ggplot(NULL,aes(RMSLE.boot)) + geom_histogram(aes(RMSLE.boot),fill="grey",color="black",binwidth=0.001)
p

nn = length(which(trainset$dept==1))
ylow = apply(yhat.train,1,quantile,(0.025/nn))
yup = apply(yhat.train,1,quantile,(1-0.025/nn))
xx = trainset[trainset$dept==1,]$weeklabel
yy = trainset[trainset$dept==1,]$logsales
yp = predict(gamtrain1,trainset)[trainset$dept==1]
B = data.frame(xx,yy,yp,ylow,yup)
save(B,file="data/B.Rdata")

pci = ggplot(B,aes(x=xx,y=yy))+
  geom_ribbon(aes(xx, ymin=ylow, ymax=yup),fill="skyblue",inherit.aes=F, alpha=0.8)+
  geom_point( )+geom_line(aes(x=xx,y=yp),color="red")+
  geom_point(aes(x=xx,y=yp),color="red")+ xlab("week")+ylab("logsales")
pci


##### kernel regression

# factor distance 0 or 1

fhat = function(X1,x1,X2,x2,y,h){
  dist1sq = rowSums((sweep(as.matrix(X1),2,as.matrix(x1)))^2)
  dist2sq = rowSums(2*(sweep(as.matrix(X2),2,as.matrix(x2))!=0))
  dist = sqrt(dist1sq + dist2sq)
  up = sum(dnorm(dist/h)*y)
  down = sum(dnorm(dist/h))
  return(up/down)
}

colnames(trainset)
X1 = trainset[,c("temp","fuel","cpi","unemp","weeklabel")]
X2 = cbind(as.numeric(trainset$dept),as.numeric(trainset$year),as.numeric(trainset$month),
     as.numeric(trainset$wk),as.numeric(trainset$holiday),as.numeric(trainset$isholiday),
     as.numeric(trainset$yday),as.numeric(trainset$mday))
y = trainset[,"logsales"]

h = 0.04

cv = c()
for(k in 1:length(h)){
  cvscore = c()
  for(i in 1:nrow(trainset)){
    x1 = trainset[i,c("temp","fuel","cpi","unemp","weeklabel")]
    x2 = cbind(as.numeric(trainset$dept[i]),as.numeric(trainset$year[i]),as.numeric(trainset$month[i]),
               as.numeric(trainset$wk[i]),as.numeric(trainset$holiday[i]),as.numeric(trainset$isholiday[i]),
               as.numeric(trainset$yday[i]),as.numeric(trainset$mday)[i])
    X1i = (as.matrix(X1))[-i,]
    X2i = (as.matrix(X2))[-i,]
    yi = y[-i]
    cvscore = c(cvscore, (y[i]-fhat(X1i,x1,X2i,x2,yi,h[k])^2))
  }
  cv = c(cv,mean(cvscore))
}

yhat = c()
for(i in 1:nrow(testset)){
  x1 = testset[i,c("temp","fuel","cpi","unemp","weeklabel")]
  x2 = cbind(as.numeric(testset$dept[i]),as.numeric(testset$year[i]),as.numeric(testset$month[i]),
            as.numeric(testset$wk[i]),as.numeric(testset$holiday[i]),as.numeric(testset$isholiday[i]),
            as.numeric(testset$yday[i]),as.numeric(testset$mday)[i])
  yhat = c(yhat, fhat(X1,x1,X2,x2,y,h))
}
error.ker =  testset$logsales - yhat
RMSLE.ker = sqrt(t(error.ker)%*%error.ker/length(error.ker))
cat("RMSLE.ker =",RMSLE.ker)

pdf("data/coef_additive.pdf",height=6,width=6)
par(mfrow=c(2,2))
plot(gamtrain1)
dev.off()
