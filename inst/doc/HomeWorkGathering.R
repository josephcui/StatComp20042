## ----women, r,echo=FALSE------------------------------------------------------

plot(women)
abline(lm(weight~height, data=women), col='blue')

## ----results='asis', echo=FALSE,fig2, fig.height = 3, fig.width = 7, fig.align = "center"----
knitr::kable(head(swiss))

## -----------------------------------------------------------------------------
a=2;b=2
##1.产生10000个随机数，并求逆
u <- runif(10000)
x <- b/(1-u)^(1/a)
##2.绘制直方图和函数曲线，进行对比
hist(x, prob=TRUE,main=expression(f(x)==2/(1-x)^(1/2)),breaks = 100,xlim = c(0,100))
y <- seq(0,100,1)
lines(y, 8/y^3)

## -----------------------------------------------------------------------------
##1.产生一个随机数数据框，每一列代表一组随机数
n = 100000
randomDf <- as.data.frame(matrix(runif(n*3)*2-1,nrow=n,ncol=3))
names(randomDf) <- c('U1','U2','U3')
##2.自定义函数，构造取数逻辑
DeliverFun <- function(randomDf,c1,c2,c3) {
  deliverNum <- ifelse(abs(randomDf[c3])>=max(abs(randomDf[c1]),abs(randomDf[c2])),randomDf[c2],randomDf[c3])
  return(deliverNum)
}
##3.取数，并绘制直方图
DeliverNums <- apply(randomDf,1,DeliverFun,c1='U1',c2='U2',c3='U3')
hist(DeliverNums, main = expression(f(x) == 3(1-x^2)/4), prob=T)

## -----------------------------------------------------------------------------
##1.通过逆方法生成样本
u <- runif(1000)
x <- 2/(1-u)^(1/4)-2
##2.绘制直方图和函数曲线图
hist(x,prob=T,breaks = 100,main = expression(f(x)==64/(y+2)^5))
y = seq(0,100,0.1)
lines(y,64/(y+2)^5)

## -----------------------------------------------------------------------------
# 生成满足U(0,pi/3)的随机数
n <- 1000000
randNums <- runif(n, 0, pi/3)

# 用样本均值去估计期望
resOfMC <- mean(sin(randNums)*pi/3)

# 用数学分析方法得到的结果
resOfAnalytic <- 0.5

# 打印
print(round(resOfMC,4))
print(round(resOfAnalytic,4))

## -----------------------------------------------------------------------------
# 生成满足U(0,1)的随机数
n <- 1000000
randNums <- runif(n)

# Simple MC
y1 <- exp(1)^randNums
meanOfSimpleMC <- mean(y1)
varOfSimpleMC <- var(y1)

# Antithetic MC
y2 <- 1/2*(exp(1)^randNums+exp(1)^(1-randNums))
meanOfAntitheticMC <- mean(y2)
varOfAntitheticMC <- var(y2)

# Empirical  percent reduction in variance of theta_hat.
empReduceOfVar <- (varOfSimpleMC - varOfAntitheticMC)/varOfSimpleMC

# Theoretical percent reduction in variance of theta_hat.
TheoReduceOfVar <- (1/4*exp(1)^2-exp(1)/2 - 1/4)/(-0.5 *exp(1)^2+2*exp(1)-1.5)

# print
print(round(meanOfSimpleMC,5))
print(round(meanOfAntitheticMC,5)) 
print(round(empReduceOfVar,5))
print(round(TheoReduceOfVar,5))

## -----------------------------------------------------------------------------
# x轴
xaxis <- seq(1,10,0.1)
# f
f <- exp(-xaxis^2/2)*xaxis^2/sqrt(pi*2)
# f1
f1 <- exp(0.5)*xaxis*exp(-xaxis^2/2)
#f2
f2 <- 9/xaxis^10

#作图
plot(xaxis,f,type='l')
lines(xaxis,f1,col='red')
lines(xaxis,f2,col='blue')


## -----------------------------------------------------------------------------
# 满足U(0,1)的样本
u <- runif(10000)

# 逆方法求f1的样本
sampleOfF1 <- sqrt(1-2*log(1-u))

#importance MC using f1
resultOfF1 <- sampleOfF1/sqrt(2*pi)/exp(0.5)
meanOfF1 <- mean(resultOfF1)
varOfF1 <- var(resultOfF1)

# 逆方法求f2的样本
sampleOfF2 <- (1-u)^(-1/9)

# importance MC using f2
resultOfF2 <- sampleOfF2^2 *exp(-sampleOfF2^2/2)/sqrt(2*pi)/(9/sampleOfF2^10)
meanOfF2 <- mean(resultOfF2)
varOfF2 <- var(resultOfF2)

# print
print(paste0('variance of f1 is ',round(varOfF1,4),";","variance of f2 is ",round(varOfF2,4)))


## -----------------------------------------------------------------------------
# number of replicates
numOfRep <- 1000


# 利用f(x)的分布函数求出分位点函数
quantileFunc <- function(alpha) {
  -log(1-(1-exp(-1)) * alpha)
}


# stratified方法分层数
Strata <- numeric(5)

# 用于对照importance sampling和stratified importance sampling的矩阵
comparisonMatrix <- matrix(0, 100, 2)

# 函数g(x)
g <- function(x) {
  exp(1)^(-x)/(1+x^2)*(x<1)*(x>0)
}

# 自定义函数：用于生成服从f(x)或者5f(x)的分布的样本
samplingFromF <- function(num, min, max, par) {
  # 使用逆方法生成服从f(x)分布的随机数
  u <- runif(num, min=min, max=max)
  -log(exp(-min)-(1-exp(-1)) * u/par)
}

# 计算积分
for (i in 1:100) {


  # importance方法得到的结果存入矩阵中
  comparisonMatrix[i, 1] <- mean((1-exp(-1))/(samplingFromF(numOfRep,0,1,par=1)^2+1))

  # 使用statified importance方法
  Strata[1] <- mean((1-exp(-1))/(samplingFromF(numOfRep/5,0,quantileFunc(0.2),5)^2+1))/5
  Strata[2] <- mean((1-exp(-1))/(samplingFromF(numOfRep/5,quantileFunc(0.2),quantileFunc(0.4),5)^2+1))/5
  Strata[3] <- mean((1-exp(-1))/(samplingFromF(numOfRep/5,quantileFunc(0.4),quantileFunc(0.6),5)^2+1))/5
  Strata[4] <- mean((1-exp(-1))/(samplingFromF(numOfRep/5,quantileFunc(0.6),quantileFunc(0.8),5)^2+1))/5
  Strata[5] <- mean((1-exp(-1))/(samplingFromF(numOfRep/5,quantileFunc(0.8),quantileFunc(1),5)^2+1))/5
  comparisonMatrix[i, 2] <- sum(Strata)
}


# 对比均值
apply(comparisonMatrix,2,mean)

# 对比方差
apply(comparisonMatrix,2, var)


## -----------------------------------------------------------------------------
mu <- 0
sigma <- 1

n <- 20
alpha <- 0.05

x <- rlnorm(n,mu,sigma)
## 区间上界
UCL <- mean(log(x))-qnorm(alpha/2)*sigma/sqrt(n)
## 区间下界
DCL <- mean(log(x))-qnorm(1-alpha/2)*sigma/sqrt(n)
print(paste0("[",DCL,"  ,  ",UCL,"]"))

## -----------------------------------------------------------------------------
mu <- 0
sigma <- 1

n <- 20
alpha <- 0.05

k <- 0
for (i in 1:1000) {
  x <- rlnorm(n,mu, sigma)
  ## 区间上界
  UCL <- mean(log(x))-qnorm(alpha/2)*sigma/sqrt(n)
  ## 区间下界
  DCL <- mean(log(x))-qnorm(1-alpha/2)*sigma/sqrt(n)
  ## 若mu在区间中则k加1，否则加0
  if (mu<UCL && mu>DCL) {
    k=k+1
  }
}
alphaHat <- 1- k/1000
print(alphaHat)

## -----------------------------------------------------------------------------
n <- 20
alpha <- 0.05

k <- 0
for (i in 1:1000) {
  x <- rchisq(n, df=2)

  UCL = mean(x)-sd(x)*qt(alpha/2, df=n-1)/sqrt(n)
  DCL = mean(x)-sd(x)*qt(1-alpha/2, df=n-1)/sqrt(n)

  if (UCL >2 && DCL < 2) {
    k <- k+1
  }
}

print(k/1000)

## -----------------------------------------------------------------------------
## 取每次抽样的样本容量为20，置信系数取0.05，定出拒绝域.
n <- 20
r <- qnorm(0.975,0,sqrt(6*(n-2)/(n+1)/(n+3)))

## 函数：计算样本偏度系数
sk <- function(x) {
  xbar <- mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  return( m3/m2^1.5)
}


## 取Beta分布的参数变化范围为1：100
theta <- c(seq(1,100,20),seq(101,1012,100))


## 对每个theta[i]，计算功效
rate <- numeric(length(theta))
for (i in 1:length(theta)) {
  e <- theta[i]
  k <- 0
  for (j in 1:10000) {
    sampleFromBeta <- rbeta(n,e,e)
    ### 样本落在拒绝域，k<-k+1
    if (abs(sk(sampleFromBeta))>r){
      k <- k+1
    }
  }
  rate[i] <- k/10000

}

## 作功效图
plot(theta,rate,type='b',ylim=c(0,1))
abline(h=0.05,col='red',lty=3)

## -----------------------------------------------------------------------------
## r代码基本同上
n <- 20
r <- qnorm(0.975,0,sqrt(6*(n-2)/(n+1)/(n+3)))


sk <- function(x) {
  xbar <- mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  return( m3/m2^1.5)
}

theta <- c(seq(1,10,1),seq(11,100,10))

rate <- numeric(length(theta))
for (i in 1:length(theta)) {
  e <- theta[i]
  k <- 0
  for (j in 1:10000) {
    sampleFromBeta <- rt(n,e)
    if (abs(sk(sampleFromBeta))>r){
      k <- k+1
    }
  }
  rate[i] <- k/10000

}
plot(theta,rate,type='b')
abline(h=0.05,col='red',lty=3)

## -----------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 1.5

maxout <- function(sampleX,sampleY) {
  centerX <- sampleX-mean(sampleX)
  centerY <- sampleY-mean(sampleY)

  outX <- sum(centerX>max(centerY)) + sum(centerX < min(centerY))
  outY <- sum(centerY>max(centerX)) + sum(centerY < min(centerX))
  return(max(outX,outY))
}

m <- 10000

nList <- c(10,30,100)

pRejectOfCF <- numeric(length(nList))

for (index in 1:length(nList)) {
  k <- 0
  for (i in 1:m){
    x <- rnorm(nList[index],0,sigma1)
    y <- rnorm(nList[index],0,sigma2)

    if (maxout(x,y) >5) {
      k <- k+1
    }
  }
  pRejectOfCF[index] <- k/m
}


## -----------------------------------------------------------------------------
m <- 10000

sigma1 <- 1
sigma2 <- 1.5


nList <- c(10,30,100)
pRejectOfF <- numeric(length(nList))

for (index in 1:length(nList)) {

  n <- nList[index]

  ## F分布的分位数
  r1 <- qf(0.055/2,n-1,n-1)
  r2 <- qf(1-0.055/2,n-1,n-1)

  k <- 0
  for (i in 1:m) {
    x <- rnorm(n,0,sigma1)
    y <- rnorm(n,0,sigma2)

    fStat <- var(x)/var(y)

    if (fStat > r2 | fStat < r1) {
      k <- k+1
    }
  }

  pRejectOfF[index] <- k/m
}


## -----------------------------------------------------------------------------
compareDF <- data.frame(Count_Five=pRejectOfCF, F_Test=pRejectOfF)
print(compareDF)

## -----------------------------------------------------------------------------
## 维度
d <- 2

##  总体分布
mu <- c(0, 1)
Sigma <- matrix(c(1, .8, .8, 2), nrow = 2, ncol = 2)

## 函数：生成多元正态r.v.
## 函数：生成多元正态r.v.
rmvn.eigen <- function(n, mu, Sigma) {
  dim <- length(mu)
  ev <- eigen(Sigma, symmetric = TRUE)
  lamb <- ev$values
  V <- ev$vectors
  C <- V %*% diag(sqrt(lamb)) %*% t(V)
  Z <- matrix(rnorm(n*dim), nrow = n, ncol = d)
  X <- Z %*% C + matrix(mu, n, dim, byrow = TRUE)
  return(X)
}

## reject area
r <- qchisq(1-0.05,df=d*(d+1)*(d+2)/6)

sampleSize <- c(10,20,30,50,100,500)

m <- 10000

pReject <- numeric(length(sampleSize))

for (i in 1:length(sampleSize)) {


  n <- sampleSize[i]

  b <- replicate(m,
                       {
                         ## generate samples
                          x <- rmvn.eigen(n,mu,Sigma)
                          # compute sample mean
                          xbar <- apply(x,2,mean)
                          # x Centralization
                          x[,1] <- x[,1]-xbar[1]
                          x[,2] <- x[,2]-xbar[2]

                          # compute MLE of cor
                          sumOfCox <- sum(x[,1]*x[,2])
                          sumList <-apply(x,2,function(x) sum(x^2))
                        corHat<-matrix(c(sumList[1],sumOfCox,sumOfCox,sumList[2]),
                                      nrow=2,ncol=2)/n
                          solveMatrix <- solve(corHat)

                          # compute  b
                          sum((x %*% solveMatrix %*% t(x))^3)/n/n
                       })
  pReject[i] <- mean(as.integer(n*b/6>r))

}

## 打印不同n对应的拒绝概率
for (index in 1:length(pReject)) {

  print(paste0("n=",sampleSize[index],"reject prob is " ,pReject[index]))

}


## -----------------------------------------------------------------------------
## 维度
d <- 2

## 函数：生成多元正态r.v.
rmvn.eigen <- function(n, mu, Sigma) {
  dim <- length(mu)
  ev <- eigen(Sigma, symmetric = TRUE)
  lamb <- ev$values
  V <- ev$vectors
  C <- V %*% diag(sqrt(lamb)) %*% t(V)
  Z <- matrix(rnorm(n*dim), nrow = n, ncol = d)
  X <- Z %*% C + matrix(mu, n, dim, byrow = TRUE)
  return(X)
}

## 总体分布
mu <- c(0,0)
Sigma1 <- matrix(c(1,0.8,0.8,2), nrow=2, ncol=2)
Sigma2 <- matrix(c(10,0.5,0.5,200), nrow=2,ncol=2)

## 卡方分布的分位数用于划定拒绝域
r <- qchisq(1-0.1,df=d*(d+1)*(d+2)/6)

## epsilon，作为横轴
epsilon <- c(seq(0,0.15,0.01), seq(0.15,1,0.05))

## pwr,功效，作为纵轴
pwr <- numeric(length(epsilon))

## 每个epsilon，模拟m次
m <- 2500

## samle size
n <- 100

## main loop
for (i in 1:length(epsilon)) {

  # e
  e <- epsilon[i]


  # simulate
  b <- replicate(m,
            {
              # generate samples
              sampleIndex <- sum(sample(c(0,1),replace=T,size=n,prob=c(e,1-e)))

              if (sampleIndex == 0) {
                x <- rmvn.eigen(n,mu,Sigma2)
              } else if (sampleIndex == n) {
                x <- rmvn.eigen(n,mu,Sigma1)
              } else{
                samplesWithCorSigma1 <- rmvn.eigen(sampleIndex,mu,Sigma1)
                samplesWithCorSigma2 <- rmvn.eigen(n-sampleIndex,mu,Sigma2)
                x <- rbind(samplesWithCorSigma1,samplesWithCorSigma2)
              }

              # compute sample mean
              xbar <- apply(x,2,mean)

              # x Centralization
              x[,1] <- x[,1]-xbar[1]
              x[,2] <- x[,2]-xbar[2]

              # compute MLE of cor
              sumOfCox <- sum(x[,1]*x[,2])
              sumList <-apply(x,2,function(x) sum(x^2))
              corHat <- matrix(c(sumList[1],sumOfCox,sumOfCox,sumList[2]),nrow=2,ncol=2)/n
              solveMatrix <- solve(corHat)

              # compute  b
              sum((x %*% solveMatrix %*% t(x))^3)/n/n}
            )

  pwr[i] <- mean(as.integer(n*b/6>r))
}


# plot
plot(epsilon, pwr, type="b",xlab=bquote(epsilon))
abline(h=0.1, lty=3,col='red')

se <- sqrt(pwr*(1-pwr)/ m)

lines(epsilon, pwr+se,lty=3)
lines(epsilon, pwr-se,lty=3)

## ----warning=FALSE------------------------------------------------------------
# import
library(bootstrap)

# Original
thetaHat <- cor(law$LSAT ,law$GPA)

n <- nrow(law)
theta.jack <- numeric(n)

# Jackknife
for (i in 1:n) {
  law.jack <- law[-i,]
  theta.jack[i] <- cor(law.jack$LSAT, law.jack$GPA)
}

# bias and se of Jack estimate
biasOfJack <- (n-1)*mean(theta.jack-thetaHat)
seOfJack <- sqrt((n-1)* mean((theta.jack-mean(theta.jack))^2))

# print
print(paste0("bias:",biasOfJack,"   ","se:",seOfJack))


## ----warning=FALSE------------------------------------------------------------
library(boot)

stat <- function(x, i) {
  mean(x[i,1])
}

obj <- boot(aircondit,statistic = stat,R=2000)


boot.ci(obj, type=c("basic","norm","perc","bca"))


## -----------------------------------------------------------------------------
library(bootstrap)

n <- nrow(scor)

# 定义求给定矩阵theta值的函数
theta <- function(x) {
  eigenVal <- eigen(cov(x))$values
  eigenVal[1]/sum(eigenVal)
}

# 计算thetaHat
thetaHat <- theta(scor)

# jackknife方法
thetaJack <- numeric(n)
for (i in 1:n) {
  thetaJack[i] <- theta(scor[-i,])
}

# 计算估计量的偏差和标准差
biasOfJack <- (n-1) * (mean(thetaJack)-thetaHat)
seOfJack <- sqrt((n-1) * mean((thetaJack-mean(thetaJack))^2))

print(paste0("bias is ",biasOfJack,", std is ",seOfJack))

## ----warning=F----------------------------------------------------------------
library(DAAG)
attach(ironslag)

n <- length(magnetic)

index =1:n

e1 <-e2<-e3<-e4<- NULL

# n为奇书，则将index随机分成[n/2]+1组，其中[n/2]组各有2个元素，1个组有1个元素
while (length(index) >=1){

  # sampling the test set.
  if (length(index)==1) {
    leaveOut <- index
  }
  else{
  leaveOut <- sample(index,size=2,replace = FALSE)
  }

  # tranning set
  x <- chemical[-leaveOut]
  y <- magnetic[-leaveOut]


  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[leaveOut]
  e1<- append(e1,magnetic[leaveOut] - yhat1)

  J2 <- lm(y ~ x+I(x^2))
  yhat2 <- J2$coef[1]+J2$coef[2]*chemical[leaveOut]+J2$coef[3]*chemical[leaveOut]^2
  e2<- append(e2,magnetic[leaveOut] - yhat2)

  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[leaveOut]
  yhat3 <- exp(logyhat3)
  e3 <-append(e3,magnetic[leaveOut] - yhat3)

  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[leaveOut])
  yhat4 <- exp(logyhat4)
  e4 <-append(e4,magnetic[leaveOut] - yhat4)

  index <- index[!(index %in% leaveOut)]

}


print(c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2)))


## ----warning=FALSE------------------------------------------------------------
# function: return the number of extreme data
MaxOutNum <- function(x, y) {
  X <- x -mean(x)
  Y <- y -mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}


m<- 100000

# 两组样本，容量均为20，且方差相等，countfive方法检验
n <- 20

x1 <- rnorm(n)
x2 <- rnorm(n)
tests <- replicate(m, expr = {
    x <- rnorm(n)
    y <- rnorm(n)
    MaxOutNum(x, y)
})

RateOfMaxOutExceed5 <- 1-cumsum(table(tests))[5]/m
print(paste0("The rate that maxout exceed 5 is ",RateOfMaxOutExceed5))

## ----warning=FALSE------------------------------------------------------------
# function：return 1 if the number of extreme data >=5
MaxOut <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(as.integer(max(c(outx, outy))>5))
}


# sample size
n1 <- 10
n2 <- 30
k <- 1:((n1+n2)/2)

# replicate
m<- 100000

rep <- numeric(m)

# function: return rate of maxout which exceed 5.
tests <- function(sd1, sd2){
  for (i in 1:m) {
    x <- rnorm(n1,mean=0,sd=sd1)
    y <- rnorm(n2,mean=0,sd=sd2)

    # equal sample size
    # select 10 data from y and add them to x.
    y_to_x <- sample(y,size = 10,replace = F)
    x <- c(x,y_to_x)
    y <- setdiff(y,y_to_x)

    # maxout
    rep[i] <- MaxOut(x,y)
}
mean(rep)
}


tests(sd1=1, sd2=1)
tests(sd1=1,sd2=2)
tests(sd1=1,sd2=10)

## ----warning=FALSE------------------------------------------------------------
library(RANN)
library(energy)
library(boot)
library(Ball)
library(mvtnorm)
library(MASS)


Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}



m <- 1e3; k<-3; p<-2; mu <- 0.5; set.seed(12345)
n1 <- n2 <- 50; R<-999; n <- n1+n2; N = c(n1,n2)
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

## ----warning=FALSE------------------------------------------------------------
mu1 <- mu2 <-  c(0,0)
Sigma1 <- matrix(c(1,0,0,1),nrow=2)
Sigma2 <- matrix(c(1,0,0,3),nrow=2)



p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- mvrnorm(n1,mu1,Sigma1)
  y <- mvrnorm(n2,mu2,Sigma2)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.va
}


alpha <- 0.1
pow <- colMeans(p.values<alpha)
print(pow)

## ----warning=FALSE------------------------------------------------------------
mu1 <-c(0,0) ;mu2 <-  c(0.5,0)
Sigma1 <- matrix(c(1,0,0,1),nrow=2)
Sigma2 <- matrix(c(1,0,0,3),nrow=2)



p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- mvrnorm(n1,mu1,Sigma1)
  y <- mvrnorm(n2,mu2,Sigma2)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.va
}


alpha <- 0.1
pow <- colMeans(p.values<alpha)
print(pow)

## ----warning=FALSE------------------------------------------------------------
mu1 <- mu2 <-  c(0,0)
Sigma1 <- matrix(c(1,0,0,1),nrow=2)
Sigma2 <- matrix(c(1,0,0,3),nrow=2)


# Function:Generate the mixture distribution of two  normal distribution
bimvrnorm <- function (n,e,mu1,mu2,Sigma1,Sigma2) {
  sampleIndex <- sum(sample(c(0,1),replace=T,size=n,prob=c(e,1-e)))

  if (sampleIndex == 0) {
    x <- mvrnorm(n,mu2,Sigma2)
  } else if (sampleIndex == n) {
    x <- mvrnorm(n,mu1,Sigma1)
  } else{
    samplesWithCorSigma1 <- mvrnorm(sampleIndex,mu1,Sigma1)
    samplesWithCorSigma2 <- mvrnorm(n-sampleIndex,mu2,Sigma2)
    x <- rbind(samplesWithCorSigma1,samplesWithCorSigma2)
  }

  return(x)
}


p.values <- matrix(NA,m,3)
for(i in 1:m){
  x<- rmvt(n1, sigma = diag(2), df = 1)
  y <- bimvrnorm(n2,0.4,mu1,mu2,Sigma1,Sigma2)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.va
}

alpha <- 0.1
pow <- colMeans(p.values<alpha)
print(pow)

## ----warning=FALSE------------------------------------------------------------
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x<- rmvt(n1, sigma = diag(2), df = 1)
  y<- rmvt(n2, sigma = diag(2), df = 3)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.va
}

alpha <- 0.1
pow <- colMeans(p.values<alpha)
print(pow)

## ----warning=FALSE------------------------------------------------------------
mu1 <- mu2 <-  c(0,0)
mu3 <- c(0,4)
Sigma1 <- matrix(c(1,0,0,1),nrow=2)
Sigma2 <- matrix(c(1,0,0,3),nrow=2)


p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- bimvrnorm(n1,0.4,mu1,mu2,Sigma1,Sigma2)
  y <- bimvrnorm(n2,0.4,mu1,mu3,Sigma1,Sigma2)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.va
}

alpha <- 0.1
pow <- colMeans(p.values<alpha)
print(pow)

## ----warning=FALSE------------------------------------------------------------
n1 <- 30
n2 <- 70

mu1 <- mu2 <-  c(0,0)
Sigma1 <- matrix(c(1,0,0,1),nrow=2)
Sigma2 <- matrix(c(1,0,0,3),nrow=2)



p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- mvrnorm(n1,mu1,Sigma1)
  y <- mvrnorm(n2,mu2,Sigma2)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,R=999,seed=i*12345)$p.va
}


alpha <- 0.1
pow <- colMeans(p.values<alpha)
print(pow)


## -----------------------------------------------------------------------------
set.seed(233)

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(abs(x[i-1])-abs(y)))
      x[i] <- y
    else {
        x[i] <- x[i-1]
        k <- k + 1
      }
  }
  return(list(x=x, k=k))
}



N <- 2000
sigma <- c(.05, 0.5, 2, 16)
x0 <- 10
rw1 <- rw.Metropolis(sigma[1], x0, N=N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)

result_table <- data.frame(Sigma=sigma,reject_rate = c(rw1$k/N, rw2$k/N, rw3$k/N, rw4$k/N))


print(result_table)


plot(c(1:N),rw1$x,type='l',xlab='sigma=0.05',ylab='X')
plot(c(1:N),rw2$x,type='l',xlab='sigma=0.5',ylab='X')
plot(c(1:N),rw3$x,type='l',xlab='sigma=2',ylab='X')
plot(c(1:N),rw4$x,type='l',xlab='sigma=16',ylab='X')
mtext("Random walk by different sigma", side = 3, line = -3, outer = TRUE)

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)
  B <- n * var(psi.means)
  psi.w <- apply(psi, 1, "var")
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n + (B/n)
  r.hat <- v.hat / W
  return(r.hat)
}


rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(abs(x[i-1])-abs(y)))
      x[i] <- y
    else {
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(x)
}


sigma <- 2
k <- 4
n <- 15000
b <- 1000

# 初始值
x0 <- c(-20, -10, 10, 20)


X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma,x0[i],n)

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))
#plot psi for the four chains

for (i in 1:k)
  plot(psi[i, (b+1):n], type="l",
       xlab=i, ylab=bquote(psi))

#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
options(digits=22)
# 函数S_{k-1}
s_k_minus_one = function(a,k){
  result =1-pt(sqrt(a^2 * (k - 1) / (k - a^2)), df = k-1)
  return(result)
}

# 函数S_k
s_k = function(a,k) {
 result = 1-pt(sqrt(a^2 * k / (k + 1 - a^2)), df = k)
 return(result)
}


k_list = c(4:25,100,500,1000)
root = numeric(length(k_list))

for (i in 1:length(k_list)) {
  eps = 1e-4
  ## 要求根的函数
  k <- k_list[i]

  f = function(a){
    return(s_k(a,k)-s_k_minus_one(a,k))
  }
  root[i]=uniroot(f, interval = c(eps, 2))$root
}
print(root)

## -----------------------------------------------------------------------------

n<-15

#记录每次迭代产生的三个参数
re<-matrix(rep(0,n*3),nrow=n)
R<-numeric(n)

# 初始值和阈值
re[1,1]<-0.1
re[1,2]<-0.1
re[1,3]<-0.8
e<-1e-8
k=1
step<-sqrt((re[k,1])^2+(re[k,1])^2+(re[k,1])^2)

# 观测值
n_A = 444
n_B = 132
n_OO = 361
n_AB = 63



while(step>e){
  p<-re[k,1]
  q<-re[k,2]
  r<-re[k,3]

  R[k]<-n_A*log(p^2+2*p*r)+n_B*log(q^2+2*q*r)+722*log(r)+n_AB*log(2*p*q)

  X<-n_A+n_AB+n_A*p^2/(p^2+2*p*r)
  Y<-n_B+n_AB+n_B*q^2/(q^2+2*q*r)
  Z<-2*n_OO+n_A+n_B-n_B*q^2/(q^2+2*q*r)-n_A*p^2/(p^2+2*p*r)

  ## 更新参数
  re[k+1,1]<-X/(X+Y+Z)
  re[k+1,2]<-Y/(X+Y+Z)
  re[k+1,3]<-Z/(X+Y+Z)

  step<-sqrt((re[k+1,1]-re[k,1])^2+(re[k+1,2]-re[k,2])^2+(re[k+1,3]-re[k,3])^2)

  k<-k+1
}

print("p,r,q is ")
print(re[1:(k-1),])
print(paste0('Final para is ','p=',re[k-1,1],'  q=',re[k-1,2],'   r=',re[k-1,3]))

## -----------------------------------------------------------------------------
print(R)
plot(R[1:(k-1)],col='blue',xlab="iter_time",ylab="loglikelyhood",type='b')

## -----------------------------------------------------------------------------
# formulas
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

#使用lapply函数
la <- lapply(formulas, lm, data=mtcars)

#使用loop
lp <- vector("list", length(formulas))
len<-seq_along(formulas)
for (i in len){
  lp[[i]] <- lm(formulas[[i]], data=mtcars)
}

print("The results of lapply:")
print(la)
print("The results of loop:")
print(lp)

## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# sapply()和匿名函数:
sapply(trials, function(x) x[["p.value"]])
# 不使用匿名函数:
sapply(trials, "[[", "p.value")

## ----warning=FALSE------------------------------------------------------------
tryList <- list(iris, mtcars, cars)

# lapply
lapply(tryList, function(x) vapply(x, mean, numeric(1)))

lmapply <- function(X, FUN, FUN.VALUE, simplify = FALSE){
  out <- Map(function(x) vapply(x, FUN, FUN.VALUE), X)
  if(simplify == TRUE){return(simplify2array(out))}
  out
}

lmapply(tryList, mean, numeric(1))

## -----------------------------------------------------------------------------
set.seed(233)
rw_R <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= exp(abs(x[i-1])-abs(y)))
      x[i] <- y 
    else {
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(list(x=x, k=k))
}

## -----------------------------------------------------------------------------
library(inline)

rw_C_code <- '
  
  using namespace Rcpp;
  #include <math.h>;

  // n and thin are SEXPs which the Rcpp::as function maps to C++ vars
  int N   = as<int>(n);
  double x = as<double>(thin);
  double sigma = as<double>(s);
  
  NumericMatrix mat(N, 2);
  
  RNGScope scope;         // Initialize Random number generator


  mat(0,0) = x;
  double k =0;
  for (int i=1;i<=N;i++){
      double u = ::Rf_runif(0,1);
      double y = ::Rf_rnorm(mat(i-1,0),sigma);
      if (u <= exp(abs(mat(i-1,0))-abs(y))){
        mat(i,0) = y;
      }
      else {
        mat(i,0) = mat(i-1,0);
        k = k+1;
      }
  }
  mat(0,1) = k;
  
  return mat;
'
# Compile and Load
rw_C <- cxxfunction(signature(n="int", thin = "double", s = "double"),
                         rw_C_code, plugin="Rcpp")

## -----------------------------------------------------------------------------
result_R <- rw_R(sigma=0.5,x0=10,N=2000)$x
result_C <- rw_C(n=2000,thin=10,s=0.5)[,1]

plot(c(1:2000),result_R,type='l',col='black',xlab='sigma=0.5',ylab='X',lty=1)
lines(c(1:2000),result_C,type='l',col='red',lty=2)
legend(1500, 9, legend=c("R","C++"),col=c("black","red"), lty=1:2, cex=0.8)

## -----------------------------------------------------------------------------
N <- 2000
sigma <- c(.05, 0.5, 2, 16)
x0 <- 10
rw_R1 <- rw_R(sigma[1], x0, N)
rw_R2 <- rw_R(sigma[2], x0, N)
rw_R3 <- rw_R(sigma[3], x0, N)
rw_R4 <- rw_R(sigma[4], x0, N)

rw_C1 <- rw_C(N,x0,sigma[1])
rw_C2 <- rw_C(N,x0,sigma[2])
rw_C3 <- rw_C(N,x0,sigma[3])
rw_C4 <- rw_C(N,x0,sigma[4])

result_table <- data.frame(Sigma=sigma,reject_rate_R = c(rw_R1$k/N, rw_R2$k/N, rw_R3$k/N, rw_R4$k/N),reject_rate_C = c(rw_C1[1,2]/N,rw_C2[1,2]/N,rw_C3[1,2]/N,rw_C4[1,2]/N))
print(result_table)

## -----------------------------------------------------------------------------
library(microbenchmark)
ts <- microbenchmark(rw.R=rw_R(2,10,2000),
               rw.C=rw_C(2000,10,2))
summary(ts)[,c(1,3,5,6)]

