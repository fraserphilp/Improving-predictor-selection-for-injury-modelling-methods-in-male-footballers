###Call libraries#####  

pkgs <- list("glmnet", "doParallel", "foreach", "pROC","ggplot2","MASS","elasticnet","lqa",
             "psych","caret","nnet","foreign","data.table","plyr","dplyr","mice","corrgram")
lapply(pkgs, require, character.only = T)
###Call data####
set.seed(2)
setwd("C:/Users/keele university/Documents/R/Fraser")
d<-read.csv("ASS.csv",header=TRUE)
d<-as.data.frame(d[,])
View(d)
head(d)
dim(d)
d<- d[1:2784,c(25,26)]
y<-d
 is.na(d)
 sapply(d, function(x) sum(is.na(x)))
 sapply(d, function(x) mean(x))
 is.na(d)
 sapply(d, function(x) sum(is.na(x)))
 sapply(d, function(x) mean(x))
 md.pattern(d[,c("Yo.Yo.fitness.score","Cumulative.Match...Training")])
 imp<-mice(data = d[,c("Yo.Yo.fitness.score","Cumulative.Match...Training")], seed = 22)
 print(imp)
 head(imp$imp$Yo.Yo.fitness.score)
 stripplot(imp,Yo.Yo.fitness.score~Cumulative.Match...Training|.imp, pch=20)
 dat.imp<-complete(imp)
 head(dat.imp, n=30)
 head(d[,Yo.Yo.fitness.score], n=30)
 head(dat.imp[is.na(d$Yo.Yo.fitness.score)==T,], n=30)
 head(d[is.na(d$Yo.Yo.fitness.score)==T,"Yo.Yo.fitness.score"], n=10)
 fit.bmi<-with(data=imp,exp=lm(Yo.Yo.fitness.score~d$Cumulative.Match...Training))
 fit.bmi
 
 summary(lm(Yo.Yo.fitness.score~d$Cumulative.Match...Training, data=d))
 est.p<-pool(fit.bmi)
 print(est.p)
 summary(lm(Yo.Yo.fitness.score~d$Cumulative.Match...Training, data=d))
 coefs<-matrix(unlist(lapply(fit.bmi$analyses, coef)), nrow=5, ncol=4, byrow=T)
 
 #plot the coefficients from each of the different rounds of imputation to see the variability in the
 #results
 plot(coefs[1,-1], ylim=c(-8, 8), xaxt="n",cex=1.5, pch=20, ylab="beta")
 axis(1, at=1:10,labels=names(fit.bmi$analyses[[1]]$coef[-1]))
 cols=2:5
 for(i in 1:dim(coefs)[1]-1){
   points(coefs[i+1,-1], col=cols[i], cex=1.5, pch=20)
 }
 title(main="Estimated Betas from Each Imputed Regression Model for BMI Outcome")
 
#pairs(d[,10:18], pch = 20,col=2)###
 set.seed(2)
 setwd("C:/Users/keele university/Documents/R/Fraser")
 d<-read.csv("ff2.csv",header=TRUE)
 dim(d)
 d<-as.data.frame(d[1:2784,-c(29,30,31)])
 head(d)
 
 ggplot(d, aes(Cumulative.Match...Training,y1)) + geom_point(aes(color=y1)) + 
   scale_x_continuous("Cumulative.Match...Training ", breaks = seq(0,0.35,0.05))+
   scale_y_continuous("Injury_status", breaks = seq(0,50, by = 15000))+
   theme_bw() + labs(title="Scatterplot") + facet_wrap( ~ y1)
### plot the correlation matrix####
 x<-d
corrgram(x, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram") 
#count(d,"Artificial.turf.3G")
###Checking the multi_collinearity level using the Varaince inflation factor###
fit1<- lm(SKF~ï..Height+Weight+ Time.in.activity +Match+ Training+ Conditioning +Sandastro +Grass+ Artificial.turf.3G+
          Previous.injuries+ Inseason.injuries+Cumulative.match.volume +Cumulative.match.Grass.volume +total.all..training.
          +total.training.volume...excluding.futsal.and.conditioning.+ total.training.Artificial.turf.3G.volume...excluding.futsal.and.conditioning.
          + total.training.futsal.volume+ total.training.Grass.volume..excluding.futsal.and.conditioning.,data=d)
summary(fit1)
library(car)
vif(fit1)

y<-as.matrix(d[,28])
#y<-factor(y,levels = 1:3,labels = c("smimple","mild","severe"))
plot(y)
x<-as.matrix(d[, c(1:27)])
#########################
setwd("C:/Users/keele university/Documents/R/Fraser")
d<-read.csv("fff2.csv" )
d<-as.data.frame(d[1:2784,-c(29,30,31)])
dim(d)
head(d,6)
plot(table(d$y1), xlab = "Number of physician office visits",
     ylab = "Frequency", axes = FALSE)
axis(2)
axis(1, at = 0:18 * 5, labels = FALSE)
axis(1, at = 0:9 * 10)          
dim(d)
y<-as.matrix(d[,27])
x<-as.matrix(d[,1:26])


#x<-as.data.frame(x)
x.train<-x
y.train<-y
########
fit<- glmnet(x, y,alpha=0.5,family="poisson")
plot(fit)
fitcv<- cv.glmnet(x.train, y.train,alpha=0.5,family="poisson")
 plot(fitcv)
 fitcv$lambda
 mean(fitcv$lambda)
 coef<-coef(fitcv)
 plot(fitcv, xvar = "norm", label = TRUE,colors=T)
 plot(fitcv, xvar = "lambda", label = TRUE)
 lambda.min<-fitcv$lambda.min
 lambda.1se<-fitcv$lambda.1se
coefmin<-coef(fitcv,s=lambda.min)
coef1se<-coef(fitcv,s=lambda.1se)
plot(coefmin)
plot(coef1se)
fit<- glmnet(x.train, y.train,alpha=0.5,lambda = 0.2046253,family="poisson")

  plot(fit$beta)
 d<-as.data.frame(d[,c(5,7,8,9,11,12,22,25,27,28)])
## with simple inflation (no regressors for zero component)

d<-within(d,{x5<-factor(x5)
 
x7<-factor(x7)
x8<-factor(x8)
x9<-factor(x9)

})     
summary(d)
n=2784
train<- sample(1:n,1*n)
d<-d[train,]
d1t<-d[-train,]
dim(d)
ggplot(d, aes(y1)) + geom_histogram() + scale_x_log10()
 
summary(d)
dim(d)
library(pscl)

summary(m1 <- zeroinfl(y1 ~ x2+ï..x1+x3+x4+x5+x6+x7+x8+x9+x13+x14+x11+x17+x20+x22+x23+x24+x25+x26+x27|
                      x2+ï..x1+x3+x4+x5+x6+x7+x8+x9+x13+x14+x11+x17+x20+x22+x23+x24+x25+x26+x27, data =d )) 

summary(m1 <- zeroinfl(y1 ~x5+x7+x27+x8+x9+x11+x12+x22+x25+x7|x5, data = d))
exp(cbind(C0=coef(m1),confint(m1)))
library(boot)
mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 8, lower.tail = FALSE)
summary(p1 <- glm(y1 ~x5+x7+x8+x9+x11+x12+x22+x25+x27, family = poisson, data = d))
vuong(p1, m1)
dput(coef(m1, "count"))
dput(coef(m1, "zero"))
f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(y1 ~ x7+x8+x9+x12+x22+x25+x27|x5,data = data[i, ],start = list(count = c(0.745, 0.043, -0.120, -0.57, 0.190, -0.0003, 
                                       -0.0003,0.124), zero = c(4.842, -1.268)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}
set.seed(10)
res1 <- boot(d, f, R = 10, parallel = "snow", ncpus = 8)


## print results
res1
## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9,11,13,15,17,19), function(i) {
  out <- boot.ci(res1, index = c(i, i + 1), type = "perc")
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
              
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

# compare with normal based approximation
confint(m1)
#exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9), function(i) {
  out <- boot.ci(res1, index = c(i, i + 1), type = "perc", h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]
              ))
}))

## add row names
#row.names(expparms) <- names(coef(m1))
## print results
expparms
####### excluded unimportant variables#####]
summary(m1 <- zeroinfl(y1 ~ x9+x12+x25+x27|x5, data = d))
library(boot)
mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 8, lower.tail = FALSE)
summary(p1 <- glm(y1 ~x5+x9+x12+x22+x25+x27, family = poisson, data = d))
vuong(p1, m1)
dput(coef(m1, "count"))
dput(coef(m1, "zero"))
f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(y1 ~  x9+x12+x25+x27|x5,data = data[i, ],
         start = list(count = c(0.6627 , -0.355 , 0.21830, 
       -0.000208 , 0.11246 ), zero = c(4.8032, -1.2302)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}
set.seed(10)
res1 <- boot(d, f, R = 10, parallel = "snow", ncpus = 8)


## print results
res1
## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9,11,13), function(i) {
  out <- boot.ci(res1, index = c(i, i + 1), type = "perc")
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]))
  
}))

## add row names
row.names(parms) <- names(coef(m1))
## print results
parms

# compare with normal based approximation
confint(m1)
#exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9,11,13), function(i) {
  out <- boot.ci(res1, index = c(i, i + 1), type = "perc", h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5]
  ))
}))










                  
