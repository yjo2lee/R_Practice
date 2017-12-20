## 로지스틱 회귀모형을 이용한 기업 부도예측

#방법1) glm (Generalized Linaer Model), bestglm 페키지 이용
#방법2) optimx / numDeriv  페키지 이용
#방법3) optim with hessian=T option 이용

## 방법 1   장점  정형화된 분석 용이
## 방법 2,3 장점 비정형/비선형 모형으로 확장 용이

####  remove clear all objects

ls()      # list all objects 
rm(list=ls()) # remove all
ls()


#### 엑셀.csv 파일로 변환후 자료입력 read .csv(.txt) files to R

dat<-read.csv(file.choose(), header=T) 

# 적절한 .csv 자료파일을  찾아서 선택 read bankruptcy.csv to R

## dat<-read.table(file.choose(), header=T) .txt file

head(dat)
str(dat) ##  data structure 파악 (자료 종류,크기, 변수명, 변수유형,...)

## 변수명 변경 rename variables 

names(dat)<-c("x1","x2","x3","x4","y")
head(dat)

#### 산점도 행렬  보기
attach(dat)

plot(dat)
scatter.smooth(x1,y)

detach(dat)




#### 방법1)  일반화선형모형을 이용하여 적합  use  binomial glm

mylogit<- glm(y~x1+x2+x3+x4,data=dat,family=binomial(link = "logit"))

summary(mylogit)

## full model y~.  최대모형  (x1,x2,x3,x4)
## null model y~1  최소모형    no x!!

names(mylogit)


## AIC 기준 최적변수선택 Best Subset Selection for glm
##  bestglm 페키지 설치 install.packages(bestglm)

library(bestglm)

BestAIC<- bestglm(dat,IC="AIC",family=binomial)

names(BestAIC)

BestAIC             ## best model p=2 ; (x1,x2)
BestAIC$Subsets     ## best model for each p=0,1,..,m

str(BestAIC$Subsets) 


## AICp plot

p<-c(0:4)
AICp<- BestAIC$Subsets[,7]
plot(p,AICp, main="AIC v.s. p",type="l")


BestAIC$BestModels ## best 5 models


## 최적모형 적중율 계산 
## predict yi = 1 if fitted values pi >0.5

names(mylogit)

y1<-ifelse(mylogit$fitted.values>0.5,1,0) 

attach(dat)

table(y1,y) ## hit ratio 적중률계산

detach(dat)




##  로그우도함수 최적화를 이용하여 직접  MLE 구하는방법
##   optimx / optim 페키지 활용법

####  Maximize Log-Likelihood function : lnL(b) 

## 이항 로지스틱 모형의 로그우도함수 정의 
## Define binomial: -log-likelihood function f(b)= -logL(b) 

attach(dat) # 자료 변수 활성화

f<-function(data,b) {                   
        
        p<-plogis( b[5]+b[1]*x1+b[2]*x2+b[3]*x3+b[4]*x4 ) # mean vector
        
        with( data, -sum( log(  dbinom(y,1,p) )  ) # expression
        ) 
        
}

#### 방법2)  optimx / numDeriv package 이용법
#### Install optimx

library(optimx) 

##  Minimize : -lnL(b) using  optimx with initial values (0,0,0,0,0)

result2 <- optimx( par = c(0,0,0,0,0), f, data = dat, 
                   control=list(all.methods=TRUE, save.failures=TRUE, trace=0)
)    

summary(result2)

## output of optimx


####  Fisher Information matrix : J = -DDlnL(a,b) :

str(result2) ## 자료구조 파악
names(result2)


library(numDeriv) ## 수치미분 페키지 설치 Install numDeriv package

logL<- -result2$value[1]            ## maximized log-likelihood
b <- as.matrix( result2[1,c(1:5)] )  ## extract MLE as a vector
AIC<- -2*logL+2*length(b)            ## AIC = -2*lnL+2*p

grad(f,data= dat,x =b)      ## gradient vector : -DlnL(a,b)

J<-hessian(f,data=dat,x = b)## Fisher Information matrix :J = -DDlnL(a,b) 

V<-solve(J)                 ## Variance-Covariance matrix of MLE V=J^-1
se<-sqrt(diag(V))           ## se of MLE

## Summary

glm.summary<-list(logL,AIC,b,se,J,V)

glm.summary


##### 방법3) optim 페키지의  hessian=T 옵션 이용법 
#   Minimize : -log-likelihood ; -lnL(b)

### method = c(“Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, "Brent")
## choose  method "BFGS" with smallest : -logL

result3 <- optim( par = rep(0,5), f, data = dat, 
                  method= "BFGS", hessian=T
)        

result3


## output of optim




## Hessian/ MLE / se /AIC

H<-result3$hessian   ## J Fisher Information matrix
U<-solve(H)          ## Variance-Covariance matrix of MLE
b1<-result3$par      ## parameter estimares MLE
se1<-sqrt(diag(U))   ## se.(b)
logL1<- result3$value ## -logL
AIC<-2*result3$value+2*length(b1) ## AIC =-2lnL+2p

logit.summary<-list(U,b1,se1,logL1,AIC)
logit.summary

### summary of logistic regression

detach(dat)

