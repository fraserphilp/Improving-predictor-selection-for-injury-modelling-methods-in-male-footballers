###Call libraries#####  

pkgs <- list("glmnet", "doParallel", "foreach", "pROC","ggplot2","MASS","elasticnet","lqa","car",
             "psych","caret","nnet","foreign","data.table","plyr","dplyr","mice","corrgram","reader","pscl","devtools","AMAZonn")
lapply(pkgs, require, character.only = T)
###Call data####
set.seed(2)
setwd("C:/Users/keele university/Documents/R/Fraser")
d<-read.csv("ASS.csv",header=TRUE)
d<-as.data.frame(d[,])
View(d)
head(d)
dim(d)

impute<- mice(d[,],m=6)
print(impute)
dim(impute)
da1<-complete(impute,2)
stripplot(impute,pch=20,cex=1)


table(d$Conditioning)
d<- d[1:2784,c(25,26)]
y<-d
 is.na(d)
 sapply(d, function(x) sum(is.na(x)))
 sapply(d, function(x) mean(x))
 is.na(d)
 sapply(d, function(x) sum(is.na(x)))
 sapply(d, function(x) mean(x))
 md.pattern(d[,c("Yo.Yo.fitness.score","Cumulative.Match...Training")])
 imp<-mice(data = d[,c(1:27)], seed = 22)
 print(imp)
 head(imp$imp$Yo.Yo.fitness.score)
 stripplot(imp,Yo.Yo.fitness.score~Cumulative.Match...Training|.imp, pch=20)
 dat.imp<-complete(imp)
 
#pairs(d[,10:18], pch = 20,col=2)###
 set.seed(2)
 fff2 <- read_csv("~/R/Fraser/fff2.csv")
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
vif(fit1)
y<-as.matrix(d[,28])
#y<-factor(y,levels = 1:3,labels = c("smimple","mild","severe"))
plot(y)
x<-as.matrix(d[, c(1:27)])
#########################
d<-read_csv("~/R/Fraser/fff2.csv")
d<-read.csv("fff2.csv",header=TRUE)
dim(d)
head(d,6)
summary(d)
## test the distribution ofa data###
apply(d,2,table)
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
library(mpath)
library(zic)
library(pscl)

###Full ZIP model with all predictor variables###

m1 <- zeroinfl(y1~ï..x1+x2+x3+x4+x5+x6+x7+x9+x10+x11+x12+x13+x14+x15 +x17+x18+x19+x21+x22+x26  
               |ï..x1+x2+x3+x4+x5+x6+x7+x9+x10+x11+x12+x14+x15+x17+x18+x19+x21+x22+x26, data = d, dist = "poisson")
 summary(m1)

 cat("loglik of zero-inflated model", logLik(m1))
 cat("BIC of zero-inflated model", AIC(m1, k = log(dim(d)[1])))
 cat("AIC of zero-inflated model", AIC(m1))

####Backward stepwise variable selection with significance level alpha=0.01##

fitbe <- be.zeroinfl(m1, data = d, dist = "poisson",
                     alpha = 0.015, trace = FALSE)
 summary(fitbe)
 cat("loglik of zero-inflated model with backward selection",
     logLik(fitbe))
 cat("BIC of zero-inflated model with backward selection",
     AIC(fitbe, k = log(dim(d)[1])))
 cat("AIC of zero-inflated model", AIC(fitbe ))
####Compute enet estimates###
 fit.lasso <- zipath(y1 ~ . | ., data = d, family = "poisson",
                    nlambda = 10, lambda.zero.min.ratio = 0.0011, 
                    maxit.theta = FALSE, trace = FALSE,standardize=TRUE,
                    penalty = "enet", rescale = TRUE)

minBic <- which.min(BIC(fit.lasso))

coef(fit.lasso, minBic)
fit.lasso$lambda.count
#cat("theta estimate", fit.lasso$theta[minBic])
cat("lambda.count", fit.lasso$lambda.count[minBic])
###Compute standard errors of coefficients of LASSO:##
se(fit.lasso, minBic, log = FALSE)
AIC(fit.lasso)[minBic]
BIC(fit.lasso)[minBic]
logLik(fit.lasso)[minBic]
########        x############

####Compute log-likelihood value via 10-fold cross-validation####
n <- dim(d)[1]
K <- 10

set.seed(197)
 foldid <- split(sample(1:n), rep(1:K, length = n))
 fitcv <- cv.zipath(y1 ~ . | ., data = d,
                    family = "poisson", nlambda=10, maxit.em = 300,
                      penalty = "enet",rescale = FALSE, foldid = foldid)
 summary(fitcv)
 cat("cross-validated loglik", max(fitcv$cv))

minBic1 <- which.min(BIC(fitcv))
coef(fitcv, minBic)
AIC(fitcv)[minBic]
BIC(fit.lasso)[minBic]
  

tmp <- zipath(y1 ~ . | ., data = d,
              family = "poisson",
              lambda.zero.min.ratio = 0.1,
              maxit = 1, maxit.em = 1,  theta.fixed = FALSE,
              penalty = "mnet")

                                        
minBic <- which.min(BIC(tmp))

coef(fit.lasso, minBic)
fit.lasso$lambda.count
#cat("theta estimate", fit.lasso$theta[minBic])
cat("lambda.count", fit.lasso$lambda.count[minBic])
###Compute standard errors of coefficients of LASSO:##
se(fit.lasso, minBic, log = FALSE)
AIC(fit.lasso)[minBic]
BIC(fit.lasso)[minBic]
logLik(fit.lasso)[minBic]

m1 <- zeroinfl(y1 ~ x | ., data = d, dist = "negbin")
m1 <- pscl::zeroinfl(y1 ~x2+x3+x4+x5+x6+x7+x9+x10+x11+x12+x13+x14+x15 +x17+x18+x19+x21+x22+x26  
                     |x2+x3+x4+x5+x6+x7+x9+x10+x11+x12+x14+x15+x17+x18+x19+x21+x22+x26 , data =d)

summary(m1)
######
fm_zip <- zipath(y1 ~ . | ., data = d, nlambda=10,family = "poisson")
plot(residuals(fm_zip) ~ fitted(fm_zip))
fm_zip_predict <- predict(object=fm_zip$fit, which=fm_zip$lambda.count,
                           model=c("full"))
coef(fm_zip, model = "count")
coef(fm_zip, model = "zero")
summary(fm_zip)
logLik(fm_zip)

pred <- predict(fm_zip)
observedcalasses<-as.data.frame(d$y1)
observedcalasses<-(ifelse(observedcalasses> 0.5, "1", "0"))
predicted.classes <-(ifelse(pred> 0.2, "1", "0"))
predicted.classes<-data.frame(predicted.classes)
predicted.classes<-as.matrix(subset(predicted.classes,  select = c(X0.0099)))
dim(observedcalasses)
table<-table(predicted.classes,observedcalasses)
confusionMatrix(table,positive="1" )


predicted.classes<-predicted.classes[,1:10]
p <- function(x){ifelse(x > 0.5, "1", "0")}
predicted.classes<-apply(pred,2,p)

predicted.classes %>% select_if(is.numeric)
predicted.classes<-predicted.classes[1:2784,1]
predicted.classes <-(ifelse(predicted.classes > 0.5, "1", "0"))
observedcalasses<-d$y1
table<-table(predicted.classes,observedcalasses)
confusionMatrix(table,positive="1" )
######




fit<- glmnet(x, y,alpha=0.5,family="poisson")
coef(fit.lasso, minBic)
plot(fit)
fitcv<- cv.glmnet(x.train, y.train,alpha=0.5,family="poisson",fold = 5)
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

   m1 <- pscl::zeroinfl(y1 ~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11+x12+x13+x14+x15 +x17+x18+x19+x21+x22+x26  
                     |x1+x2+x3+x4+x5+x6+x7+x9+x10+x11+x12+x14+x15+x17+x18+x19+x21+x22+x26 , data =d)
  summary(m1)
 summary(m1 <- pscl::zeroinfl(y1 ~.|x1+x2+x3+x4, data =d )) 

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

summary(m1 <- zeroinfl(y1 ~ .|., data =d )) 
summary(m1 <- zeroinfl(y1 ~x5+x7+x8+x9+x11+x12+x22+x25+x7|x5, data = d))
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


