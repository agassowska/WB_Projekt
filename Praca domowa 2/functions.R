# ---
# packages

library(OpenML)

library(dplyr)
library(purrr)
library(reshape2)

library(ggplot2)
library(cowplot)
library(patchwork)
library(visdat)
library(naniar)

library(missMDA)

library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(mlr3viz)

# devtools::install_github("jabiru/tictoc")
library(tictoc)

# ---
# functions

# ---
# load functions

# takes no arguments and returns a clean dataset with missing values

load_eucalyptus <- function() {
  # config
  set.seed(1)
  source <- 'openml'
  
  
  # download data
  list_all_openml_dataset <- listOMLDataSets()
  
  openml_id <- 188L
  data_name <- list_all_openml_dataset[list_all_openml_dataset[, 'data.id']==openml_id, 'name']
  
  dataset_openml <- getOMLDataSet(data.id=openml_id)
  dataset_raw <- dataset_openml$data
  target_column <- dataset_openml$target.features
  
  
  # preprocessing
  ## cleaning types of columns, removing columns etc.
  dataset <- dataset_raw %>%
    # transform Latitude from degrees and minutes to degrees with fractions
    mutate(Latitude=-as.numeric(substr(Latitude, 1, 2))-as.numeric(substr(Latitude, 5, 6))/60) %>%
    # cast Sp to unordered factor
    mutate(Sp=factor(Sp, levels=unique(Sp), ordered=F)) %>%
    # change unrealistic Latitude values to NAs
    mutate(Latitude=ifelse(Latitude<(-60), NA, Latitude)) %>%
    # change unrealistics DBH values to NAs
    mutate(DBH=ifelse(DBH>100, NA, DBH)) %>%
    # transform problem to binary classification
    mutate(Utility=factor(ifelse(Utility %in% c('best', 'good'), 1, 0), levels=c(1, 0), ordered=F)) %>%
    # drop some columns
    select(-Abbrev, -Rep, -Locality, -Map_Ref)
  
  return(dataset)
}

# ---
# impute functions

# takes clean dataset with missing values (additional arguments must have default values) and returns an imputed dataset

impute_missMDA <- function(dataset, nbsim=5) {
  nb <- estim_ncpFAMD(dataset, nbsim=nbsim)
  res.comp <- imputeFAMD(dataset, nb$ncp)
  return(res.comp$completeObs)
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
  train_dataset <- imputer(dataset[train_set, ])
  times <- toc()
  train_dataset_imputation_time <- times$toc-times$tic
    
  tic('test dataset imputation')
  test_dataset <- imputer(dataset[test_set, ])
  times <- toc()
  test_dataset_imputation_time <- times$toc-times$tic
  
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

# ---
# example

#eucalyptus <- load_eucalyptus()
#vis_dat(eucalyptus)
#learner <- lrn('classif.ranger', predict_type='prob')
#result <- train_and_test(eucalyptus, imputer=impute_missMDA, learner=learner, target='Utility', positive='1', title='eucalyptus + missMDA')
#result$cv_plot
