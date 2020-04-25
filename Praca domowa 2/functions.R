# ------
# packages

library(OpenML)

library(ggplot2)
library(naniar)
library(patchwork)

library(imputeMissings)
library(softImpute)
library(missRanger)
library(mice)
library(VIM)
library(Amelia)

library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3viz)

#devtools::install_github("jabiru/tictoc")
library(tictoc)
library(stringr)

# ------
# functions

# ---
# load_raw function

# takes dataset id and returns a list of length of 2:
# - name - name of the dataset
# - dataset - processed dataset
# - target - target variable

load_raw <- function(id) {
  dirs <- list.dirs(path='./datasets_raw', full.names=TRUE)
  dir <- dirs[grep(paste0('./datasets_raw/openml_dataset_', id), dirs)]
  result <- inner_load_raw(dir)
  
  return(result)
}

inner_load_raw <- function(dir) {
  wd <- getwd()
  setwd(dir)
  
  surogate_env=new.env(parent=.BaseNamespaceEnv)
  attach(surogate_env)
  source('code.R', surogate_env)
  name <- surogate_env$data_name
  dataset <- surogate_env$dataset
  target <- surogate_env$target_column
  
  setwd(wd)
  
  return(list(name=name, dataset=dataset, target=target))
}

# ---
# load_imputed functions

# takes dataset id and set ('train' or 'test') returns an imputed dataset (train or test)

load_imputed <- function(id, package) {
  result <- inner_load_imputed(id, package)
  train <- result$train
  test <- result$test
  result_raw <- load_raw(id)
  name <- result_raw$name
  target <- result_raw$target
  
  return(list(name=name, train=train, test=test, target=target))
}

inner_load_imputed <- function(id, package) {
  train <- read.csv(paste0('datasets_imputed/', package, '/', id, '_train.csv'))
  test <- read.csv(paste0('datasets_imputed/', package, '/', id, '_test.csv'))
  
  return(list(train=train, test=test))
}

# ---
# impute functions

# takes clean dataset with missing values (additional arguments must have default values) and returns an imputed dataset

fix_names <- function(dataset) {
  names(dataset) <- str_replace_all(names(dataset), pattern=' ', replacement='')
  colnames(dataset) <- make.names(colnames(dataset), unique=TRUE)
  
  return(dataset)
}

impute_basic <- function(dataset) {
  dataset <- fix_names(dataset)
  
  dataset_imputed <- data.frame(imputeMissings::impute(dataset, method='median/mode'))
  
  return(dataset_imputed)
}

impute_missRanger <- function(dataset) {
  dataset <- fix_names(dataset)
  
  dataset_imputed <- missRanger(dataset, maxiter=5)
  
  return(dataset_imputed)
}

impute_VIM_knn <- function(dataset) {
  dataset <- fix_names(dataset)
  
  dataset_imputed <- kNN(dataset, imp_var=FALSE, trace=TRUE)
  
  return(dataset_imputed)
}

impute_VIM_hotdeck <- function(dataset) {
  dataset <- fix_names(dataset)
  
  dataset_imputed <- hotdeck(dataset, imp_var=FALSE)
  
  return(dataset_imputed)
}

impute_softImpute_mode <- function(dataset) {
  dataset <- fix_names(dataset)
  
  factors <- unlist(lapply(dataset, is.factor))
  num_imp <- softImpute::complete(dataset[!factors], softImpute(dataset[!factors], trace=TRUE, type='svd'))
  fact_imp <- imputeMissings::impute(dataset[factors], method='median/mode')
  dataset_imputed <- cbind(fact_imp, num_imp)
  
  return(dataset_imputed)
}

# takes really looong time...
impute_mice <- function(dataset) {
  names(dataset) <- str_replace_all(names(dataset), pattern = " ", replacement = "")
  colnames(dataset) <- make.names(colnames(dataset), unique = T)
  missings <- is.na(dataset)
  return(mice::complete(mice(data=dataset, nnet.MaxNWts=3000, diagnostics=FALSE, remove_collinear=FALSE, method='pmm', where=missings, printFlag=TRUE)))
}


impute_amelia <- function(dataset){
  factors <- colnames(dataset[unlist(lapply(dataset, is.factor))])
  imp <- amelia(dataset, ords = factors, parallel = 'multicore', m=1, incheck = FALSE)
  return(as.data.frame(imp$imputations))
}

# ---
# split_and_impute function

# takes:
# - id - id of dataset to split and impute
# - imputer - impute function
# - train_size - size (0-1) of train set (default value is 0.8)
# - save - whether to save imputed datasets to datasets_imputed
# returns:
# - train - imputed train dataset
# - test - imputed test dataet
# - time - total imputation time

split_and_impute <- function(id, imputer, package, train_size=0.8, save=TRUE) {
  load_result <- load_raw(id)
  dataset <- load_result$dataset
  target <- load_result$target

  train_set <- sample(nrow(dataset), 0.8*nrow(dataset))
  test_set <- setdiff(seq_len(nrow(dataset)), train_set)
  
  tic('imputation')
  
  train_dataset <- imputer(dataset[train_set, !(colnames(dataset) %in% c(target))])
  tarcol1 <- data.frame(dataset[train_set, target])
  colnames(tarcol1) <- target
  train_dataset <- cbind(train_dataset, tarcol1)

  test_dataset <- imputer(dataset[test_set, !(colnames(dataset) %in% c(target))])
  tarcol2 <- data.frame(dataset[test_set, target])
  colnames(tarcol2) <- target
  test_dataset <- cbind(test_dataset, tarcol2)
  
  times <- toc()
  
  imputation_time <- times$toc-times$tic
  
  if(save) {
    write.csv(x=train_dataset, file=paste0('./datasets_imputed/', package, '/', id, '_train.csv'),row.names=FALSE)
    write.csv(x=test_dataset, file=paste0('./datasets_imputed/', package, '/', id, '_test.csv'),row.names=FALSE)
  }
  
  return(list(train=train_dataset, test=test_dataset, time=imputation_time[['elapsed']]))
}

# ---
# train_and_test function

# takes:
# - train - imputed train dataset without missing values
# - test - imputed test dataset without missing values
# - learner - a classification learner from mlr3learners (must return probabilities)
# - target - name of target variable in dataset
# - postive - label of positive class (defalut value is '1')
# - folds - number of fold used for crossvalidation (default value is 5)
# - title - title for crossvalidation metrics plot (default value is no title)
# returns:
# - cv_plot - plot of ROC, AUC, BACC, MCC achieved during crossvalidation stage
# - mean_auc - mean AUC achieved during crossvalidation stage
# - mean_bacc - mean BACC achieved during crossvalidation stage
# - mean_mcc - mean MCC achieved during crossvalidation stage
# - test_auc - AUC achievied druing test stage
# - test_bacc - BACC achievied druing test stage
# - test_mmc - MCC achievied druing test stage
# - learner - learner trained on whole train set

train_and_test <- function(train, test, learner, target, positive='1', folds=5, title='') {
  # crossvalidation
  resampling <- rsmp("cv", folds=folds)
  task_cv <- TaskClassif$new(id='task_cv', backend=train, target=target, positive=positive)
  
  rr <- resample(task_cv, learner, resampling, store_models=TRUE)
  
  aucs <- rr$score(msr('classif.auc'))
  mean_auc <- mean(aucs$classif.auc)
  label <- paste('Mean AUC:\n', round(mean(aucs$classif.auc), 2), '±', round(sd(aucs$classif.auc), 2))
  p1 <- autoplot(rr, type='roc') +
    labs(title='Receiver operating characteristic') +
    xlab('Specifity') +
    ylab('Sensivity') +
    annotate(geom="label", x=0.5, y=0.1, label=label, fill='white', size=4.5, alpha=.5) +
    theme_light() +
    theme(
      legend.position = 'none',
      plot.title=element_text(hjust=0.5)
    )
  
  baccs <- rr$score(msr('classif.bacc'))
  mean_bacc <- mean(baccs$classif.bacc)
  baccs$iteration <- factor(baccs$iteration, levels=baccs$iteration)
  label <- paste('Mean BACC:\n', round(mean(baccs$classif.bacc), 2), '±', round(sd(baccs$classif.bacc), 2))
  p2 <- ggplot(baccs, aes(x=0, y=classif.bacc)) +
    geom_boxplot(outlier.alpha=0) +
    geom_point(aes(colour=iteration), size=2.5, alpha=.75) +
    lims(x=c(-1, 1), y=c(0, 1)) +
    labs(title='Balanced accuracy', colour='Fold') +
    xlab('') +
    ylab('') +
    annotate(geom="label", x=0, y=0.1, label=label, fill='white', size=4.5, alpha=.5) +
    theme_light() +
    theme(
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      legend.position = 'none',
      plot.title=element_text(hjust=0.5)
    )
  
  mccs <- rr$score(msr('classif.mcc'))
  mean_mcc <- mean(mccs$classif.mcc)
  mccs$iteration <- factor(mccs$iteration, levels=mccs$iteration)
  label <- paste('Mean MCC:\n', round(mean(mccs$classif.mcc), 2), '±', round(sd(mccs$classif.mcc), 2))
  p3 <- ggplot(mccs, aes(x=0, y=classif.mcc)) +
    geom_boxplot(outlier.alpha=0) +
    geom_point(aes(colour=iteration), size=2.5, alpha=.75) +
    lims(x=c(-1, 1), y=c(0, 1)) +
    labs(title='Matthews correlation coefficient', colour='Fold') +
    xlab('') +
    ylab('') +
    annotate(geom='label', x=0, y=0.1, label=label, fill='white', size=4.5, alpha=.5) +
    theme_light() +
    theme(
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      plot.title=element_text(hjust=0.5)
    )
  
  temp <- p2 + p3
  plot <- p1 + temp + plot_annotation(title=title)
  
  # testing on test set
  task_train <- TaskClassif$new(id='task_train', backend=train, target=target, positive=positive)
  learner$train(task_train)
  
  task_predict <- TaskClassif$new(id='task_predict', backend=test, target=target, positive=positive)
  prediction <- learner$predict(task_predict)
  
  test_auc <- prediction$score(msr('classif.auc'))
  test_bacc <- prediction$score(msr('classif.bacc'))
  test_mcc <- prediction$score(msr('classif.mcc'))
  
  # returning all values
  return(list(cv_plot=plot, mean_auc=mean_auc, mean_bacc=mean_bacc, mean_mcc=mean_mcc,
              test_auc=test_auc[['classif.auc']], test_bacc=test_bacc[['classif.bacc']], test_mcc=test_mcc[['classif.mcc']],
              learner=learner))
}


# ------
# example
