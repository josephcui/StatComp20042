#' @import Rcpp
#' @useDynLib StatComp20042
NULL


#' @title Computing hyperplanes for binary classification problems
#'
#' @param w wight vector
#' @param b bias
#' @param l_rate learning rate
#' @param feature character matrix
#' @param c category
#'
#' @return parameters of hyperplane
#' @export
#'
#' @examples
#' \dontrun{
#' data(iris_trim)
#' w = c(0,0)
#' b=0
#' f = iris_trim[,c(1,2)]
#' t = iris_trim[,5]
#' l_rate =0.001
#' perceptronR(w,b,l_rate,f,t)
#' }
perceptronR<- function(w,b,l_rate,feature,c){
  judge=TRUE
  while(judge){
    wrong_cnt <- 0
    for(i in 1:nrow(feature)){
      if(c[i] * ((feature[i,] %*% w)+b) <=0) {
        w <- w + l_rate*c[i]*feature[i,]
        b <- b + l_rate*c[i]
        wrong_cnt = wrong_cnt+1
      }
    }
    if (wrong_cnt ==0){
      judge=FALSE
    }

  }
  re<- list(w,b)
  return(re)
}
















#' @title Split data to training data set and test data set
#'
#' @param raw_data data to be splited
#' @param train_size proportion of training set
#' @param raw_seed random seed
#'
#' @return training set data and test set data
#' @export
#'
#' @examples
#' \dontrun{
#' train_test_split(raw_data=iris_trim, train_size=0.8, raw_seed=112)
#' }
train_test_split <- function(raw_data,train_size,raw_seed){
  set.seed(raw_seed)
  sample <- sample.int(n = nrow(raw_data), size = floor(train_size*nrow(raw_data)), replace = F)
  train <- raw_data[sample,]
  test <- raw_data[-sample, ]
  return(list(train,test))
}






#' Title
#'
#' @param x a instance
#' @param mu mean
#' @param sigma std
#'
#' @return probabilty denstiny
#' @export
#'
gaussian_probability <- function(x,mu,sigma){
  return(1/sqrt(2*pi)/sigma*exp(-(x-mu)^2/2/sigma^2))
}







#' @title Fit a gaussian naive bayes model.
#'
#' @param train A matrix composed of column vectors. The last column is the category and the former columns are features.
#'
#' @return A list composed of prior distribution,a matrix of mean values and a matrix of std values.
#' @export
#'
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




#' @title  Predict category of test data using gaussian naive bayes model.
#' @import stats
#' @param test_array A feature vector to be predicted
#' @param gnb_list the return value of gnb_fit model
#'
#' @return the category of test_array
#' @export
#'
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






