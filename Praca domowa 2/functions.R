# ------
# packages

library(OpenML)

library(ggplot2)
library(visdat)
library(naniar)
library(patchwork)

library(imputeMissings)
library(missForest)
library(VIM)
library(mice)
library(missMDA)
library(missRanger)

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
# load function

# takes dataset id and returns a list of length of 2:
# - name - name of the dataset
# - dataset - processed dataset

load <- function(id) {
  dirs <- list.dirs(path='./datasets', full.names=TRUE)
  dir <- dirs[grep(paste0('./datasets/openml_dataset_', id), dirs)]
  
  result <- inner_load(dir)
  
  return(result)
}

inner_load <- function(dir) {
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
# impute functions

# takes clean dataset with missing values (additional arguments must have default values) and returns an imputed dataset

impute_basic <- function(dataset) {
  return(data.frame(impute(dataset, method='median/mode')))
}

# alternatywny missforest działajacy dla danych wielowymiarowych:
impute_missRanger <- function(dataset){
  imputed <- missRanger(dataset, maxiter = 5)
  return(imputed)
}

impute_VIM_knn <- function(dataset) {
  return(kNN(dataset, imp_var=FALSE))
}

impute_VIM_hotdeck <- function(dataset) {
  return(hotdeck(dataset, imp_var=FALSE))
}

impute_mice <- function(dataset) {
  # jeden ze zbiorów ma spację w nazwie
  names(dataset)<-str_replace_all(names(dataset), pattern = " ", replacement = "")
  missings <- is.na(dataset)
  return(mice::complete(mice(dataset, nnet.MaxNWts = 3000, diagnostics = FALSE, remove_collinear = FALSE, method = 'pmm', where = missings)))
}

impute_missMDA <- function(dataset) {
  nb <- estim_ncpFAMD(dataset, ncp.max = 0, nbsim = 10, method.cv = 'Kfold')
  imputed_df <- imputeFAMD(dataset, ncp = nb$ncp)
  result_df <- imputed_df$completeObs
  return(result_df)
}

# nie chce działać dla zbiorów: 6332, 40536, 41278, z powodu zbyt dużej liczby poziomów
impute_missforest <- function(dataset) {
  data.imp <- missForest(dataset)
  return(data.imp$ximp)
}


# dla id = 6332 rzuca błąd: Error in 1L:ncol(Y) : argument of length 0
impute_VIM_irmi <- function(dataset) {
  return(irmi(dataset, imp_var=FALSE))
}

# ---
# train_and_test function

# takes:
# - dataset - clean dataset with missing values
# - imputer - a function from 'impute functions' section
# - learner - a classification learner from mlr3learners (must return probabilities)
# - target - name of target variable in dataset
# - postive - label of positive class (defalut value is '1')
# - folds - number of fold used for crossvalidation (default value is 5)
# - train_size - size (0-1) of train set (default value is 0.8)
# - title - title for crossvalidation metrics plot (default value is no title)
# returns:
# - train_dataset_imputation_time - time it took to impute train set with imputer function
# - test_dataset_imputation_time - time it took to impute test set with imputer function
# - cv_plot - plot of ROC, AUC, BACC, MCC achieved during crossvalidation stage
# - mean_auc - mean AUC achieved during crossvalidation stage
# - mean_bacc - mean BACC achieved during crossvalidation stage
# - mean_mcc - mean MCC achieved during crossvalidation stage
# - test_auc - AUC achievied druing test stage
# - test_bacc - BACC achievied druing test stage
# - test_mmc - MCC achievied druing test stage
# - learner - learner trained on whole train set

train_and_test <- function(dataset, imputer, learner, target, positive='1', folds=5, train_size=0.8, title='') {
  # train/test split
  train_set <- sample(nrow(dataset), 0.8*nrow(dataset))
  test_set <- setdiff(seq_len(nrow(dataset)), train_set)
  
  # imputation of train/test sets with imputer function (each set is imputed individually)
  tic('train dataset imputation')
  train_dataset <- imputer(dataset[train_set, !(colnames(dataset) %in% c(target))])
  times <- toc()
  train_dataset_imputation_time <- times$toc-times$tic
  tarcol1 <- data.frame(dataset[train_set, target])
  colnames(tarcol1) <- target
  train_dataset <- cbind(train_dataset, tarcol1)
    
  tic('test dataset imputation')
  test_dataset <- imputer(dataset[test_set, !(colnames(dataset) %in% c(target))])
  times <- toc()
  test_dataset_imputation_time <- times$toc-times$tic
  tarcol2 <- data.frame(dataset[test_set, target])
  colnames(tarcol2) <- target
  test_dataset <- cbind(test_dataset, tarcol2)
  
  # crossvalidation
  resampling <- rsmp("cv", folds=folds)
  task_cv <- TaskClassif$new(id='task_cv', backend=train_dataset, target=target, positive=positive)
  
  rr <- resample(task_cv, learner, resampling, store_models=TRUE)
  
  aucs <- rr$score(msr('classif.auc'))
  mean_auc <- mean(aucs$classif.auc)
  label <- paste('Mean AUC: ', round(mean(aucs$classif.auc), 2), '±', round(sd(aucs$classif.auc), 2))
  p1 <- autoplot(rr, type='roc') +
    labs(title='Receiver operating characteristic') +
    xlab('Specifity') +
    ylab('Sensivity') +
    annotate(geom="label", x=0.5, y=0, label=label, fill='white') +
    theme_light() +
    theme(
      legend.position = 'none',
      plot.title=element_text(hjust=0.5)
    )
  
  baccs <- rr$score(msr('classif.bacc'))
  mean_bacc <- mean(baccs$classif.bacc)
  baccs$iteration <- factor(baccs$iteration, levels=baccs$iteration)
  label <- paste('Mean BACC: ', round(mean(baccs$classif.bacc), 2), '±', round(sd(baccs$classif.bacc), 2))
  p2 <- ggplot(baccs, aes(x=0, y=classif.bacc)) +
    geom_boxplot(outlier.alpha=0) +
    geom_point(aes(colour=iteration), size=2.5, alpha=.75) +
    lims(x=c(-1, 1), y=c(0, 1)) +
    labs(title='Balanced accuracy', colour='Fold') +
    xlab('') +
    ylab('') +
    annotate(geom="label", x=0, y=0, label=label, fill='white') +
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
  label <- paste('Mean MCC: ', round(mean(mccs$classif.mcc), 2), '±', round(sd(mccs$classif.mcc), 2))
  p3 <- ggplot(mccs, aes(x=0, y=classif.mcc)) +
    geom_boxplot(outlier.alpha=0) +
    geom_point(aes(colour=iteration), size=2.5, alpha=.75) +
    lims(x=c(-1, 1), y=c(0, 1)) +
    labs(title='Matthews correlation coefficient', colour='Fold') +
    xlab('') +
    ylab('') +
    annotate(geom='label', x=0, y=0, label=label, fill='white') +
    theme_light() +
    theme(
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      plot.title=element_text(hjust=0.5)
    )
  
  temp <- p2 + p3
  plot <- p1 + temp + plot_annotation(title=title)
  
  # testing on test set
  task_train <- TaskClassif$new(id='task_train', backend=train_dataset, target=target, positive=positive)
  learner$train(task_train)
  
  task_predict <- TaskClassif$new(id='task_predict', backend=test_dataset, target=target, positive=positive)
  prediction <- learner$predict(task_predict)
  
  test_auc <- prediction$score(msr('classif.auc'))
  test_bacc <- prediction$score(msr('classif.bacc'))
  test_mcc <- prediction$score(msr('classif.mcc'))
  
  # returning all values
  return(list(train_dataset_imputation_time=train_dataset_imputation_time[['elapsed']],
              test_dataset_imputation_time=test_dataset_imputation_time[['elapsed']],
              cv_plot=plot, mean_auc=mean_auc, mean_bacc=mean_bacc, mean_mcc=mean_mcc,
              test_auc=test_auc[['classif.auc']], test_bacc=test_bacc[['classif.bacc']], test_mcc=test_mcc[['classif.mcc']],
              learner=learner))
}


# ------
# example

#name <- load(1590)$dataset
#gg_miss_var(name)
#target <- load(188)$target
#pos=dataset[1, target]
#dataset[, target] <- factor(unlist(lapply(dataset[, target], function(x) ifelse(x==pos, 1, 0))), levels=c(1, 0))
#learner <- lrn('classif.ranger', predict_type='prob')
#result <- train_and_test(dataset, imputer=impute_basic, learner=learner, target=target, positive='1', title=paste0(name, ' + basic (median/mode)'))
