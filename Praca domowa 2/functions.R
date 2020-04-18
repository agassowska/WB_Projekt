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

impute_missMDA <- function(dataset) {
  tic('estimation')
  nb <- estim_ncpFAMD(dataset)
  toc()
  tic('imputation')
  res.comp <- imputeFAMD(dataset, ncp=nb$ncp)
  toc()
  return(res.comp$completeObs)
}

# ---
# other functions

train_and_test <- function(dataset, target, positive, folds=5, title='') {
  resampling <- rsmp("cv", folds=folds)
  learner <- lrn('classif.ranger', predict_type='prob')
  task <- TaskClassif$new(id='task', backend=dataset, target=target, positive=positive)
  
  rr <- resample(task, learner, resampling, store_models=TRUE)
  
  aucs <- rr$score(msr('classif.auc'))
  mean_auc <- mean(aucs$classif.auc)
  label <- paste('Mean AUC: ', round(mean(aucs$classif.auc), 2), '±', round(sd(aucs$classif.auc), 2))
  p1 <- autoplot(rr, type='roc') +
    labs(title='Receiver operating characteristic') +
    xlab('Specifity') +
    ylab('Sensivity') +
    annotate(geom="label", x=.85, y=0, label=label, fill='white') +
    theme_light() +
    theme(
      legend.position = 'none'
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
      legend.position = 'none'
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
    annotate(geom="label", x=0, y=0, label=label, fill='white') +
    theme_light() +
    theme(
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank()
    )
  
  temp <- p2 + p3
  plot <- p1 + temp + plot_annotation(title=title)
  
  return(list(plot=plot, mean_auc=mean_auc, mean_bacc=mean_bacc, mean_mcc=mean_mcc))
}

# ---
# example

eucalyptus_raw <- load_eucalyptus()
vis_dat(eucalyptus_raw)
eucalyptus_imputed <- impute_missMDA(dataset=eucalyptus_raw)
vis_dat(eucalyptus_imputed)
result <- train_and_test(eucalyptus_imputed, target='Utility', positive='1', title='eucalyptus + missMDA')
result$plot

# ---
# playground
