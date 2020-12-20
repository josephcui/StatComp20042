## ----eval=FALSE---------------------------------------------------------------
#  NumericVector perceptronC(NumericVector w,double b, double l_rate,NumericMatrix feature, NumericVector target){
#    bool judge = true;
#    while(judge){
#      int wrong_cnt =0;
#      for (int j=0;j<feature.nrow();j++){
#        if(target[j] * (dot(feature.row(j),w)+b) <= 0) {
#          w = w + l_rate*target[j]*feature.row(j);
#          b = b + l_rate*target[j];
#          wrong_cnt = wrong_cnt+1;
#        }
#      }
#      if(wrong_cnt==0){
#        judge = false;
#      }
#    }
#    w.insert(0,b);
#    return(w);
#  }

## ----eval=FALSE---------------------------------------------------------------
#  perceptronR<- function(w,b,l_rate,feature,target){
#    judge=TRUE
#    while(judge){
#      wrong_cnt <- 0
#      for(i in 1:nrow(feature)){
#        if(target[i] * ((feature[i,] %*% w)+b) <=0) {
#          w <- w + l_rate*target[i]*feature[i,]
#          b <- b + l_rate*target[i]
#          wrong_cnt = wrong_cnt+1
#        }
#      }
#      if (wrong_cnt ==0){
#        judge=FALSE
#      }
#  
#    }
#    re<- list(w,b)
#    return(re)
#  }

## ----fig.width=7--------------------------------------------------------------
library(ggplot2)
library(StatComp20042)
data(iris_trim)
dataPlot <- as.data.frame(iris_trim)
dataPlot$category_factor = as.factor(dataPlot$category)

ggplot(data=dataPlot) + geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=category_factor))

## -----------------------------------------------------------------------------
# set initial values and learning rate
w = c(0,0);b=0;l_rate=0.001;
# set the input data:feature and target
f = iris_trim[,c(1,2)]
t = iris_trim[,5]
# executon algorithm
resultR <- perceptronR(w,b,l_rate,f,t)
resultC <- perceptronC(w,b,l_rate,f,t)
# show
dfShow = data.frame(d =c(0,0),w1=c(0,0),w2=c(0,0))
dfShow[1,]=array(resultC)
dfShow[2,] = array(c(resultR[[2]],resultR[[1]]))
knitr::kable(dfShow)

## ----fig.width=7--------------------------------------------------------------
ggplot(data=dataPlot) +
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=category_factor))+
  geom_abline(intercept = -resultC[1]/resultC[3], slope = -resultC[2]/resultC[3], color="red")

## -----------------------------------------------------------------------------
library(microbenchmark)
tm <- microbenchmark(
  resultR = perceptronR(w,b,l_rate,f,t),
  resultC = perceptronC(w,b,l_rate,f,t)
)
knitr::kable(summary(tm)[,c(1,3,5,6)])

## -----------------------------------------------------------------------------
# fit
gnb_fit <- function(train) {
  train_X = train[,-ncol(train)]
  train_y = train[,ncol(train)]
  tar <- unique(train_y)
  prior_list <- NULL
  std_matrix <- NULL
  mean_matrix <- NULL
  for (i in 1:length(tar)) {
    prior <- sum(train_y==tar[i])/length(train_y)
    tem <- train_X[train_y==tar[i],]
    std_list <- apply(tem,2,function(x) sqrt(var(x)))
    mean_list <- apply(tem,2,mean)
    prior_list <- append(prior_list,prior)
    std_matrix <- cbind(std_matrix,std_list)
    mean_matrix <- cbind(mean_matrix,mean_list)
    names(tar)[i] <- paste0('category_',i)
    names(prior_list)[i] <- paste0('prior_prob_category_',i)
    colnames(std_matrix)[i] <- paste0('std_under_category_',i)
    colnames(mean_matrix)[i] <- paste0('mean_under_category_',i)

  }
  return(list('Category'=tar,'Prior Distribution'=prior_list,'Mean Matrix'=mean_matrix,'Std Matrix'=std_matrix))
}


# predict
gnb_pre <- function(test_array,gnb_list){
  cat_num <- length(gnb_list$`Prior Distribution`)
  prob_list = numeric(cat_num)
  for (i in 1:cat_num) {
    prior <- gnb_list$`Prior Distribution`[i]
    con_prob <- 1
    for (j in 1:length(test_array)){
      tem_mean <- gnb_list$`Mean Matrix`[j,i]
      tem_std <- gnb_list$`Std Matrix`[j,i]
      con_prob <- con_prob * gaussian_probability(test_array[j],tem_mean,tem_std)
    }
    prob_list[i] <- prior * con_prob
  }
  index <- match(max(prob_list),prob_list)
  if (length(index)>1) {
    index <- index[sample(1:length(index),1)]
  }
  return(gnb_list$`Category`[index])
}


## -----------------------------------------------------------------------------
# Data preprocessing
train_and_test <- train_test_split(raw_data=iris_trim, train_size=0.8, raw_seed=112)
train <- train_and_test[[1]]
test <- train_and_test[[2]]
# Fit gnb model
gnb_list = gnb_fit(train)
print(gnb_list)
# Predict
tf <- c(5,3.5,1.5,0.2)
print(gnb_pre(tf, gnb_list))
pre_category = NULL
for (i in 1:nrow(test)){
  pre_category <- append(pre_category,gnb_pre(test[i,-5],gnb_list))
}
re <- cbind(test,pre_category)
print(re[,c(ncol(re)-1,ncol(re))])

