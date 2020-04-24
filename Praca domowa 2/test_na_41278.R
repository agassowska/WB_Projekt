# Potrzebne funkcje
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

impute_mice <- function(dataset) {
  # jeden ze zbiorów ma spację w nazwie
  names(dataset)<-str_replace_all(names(dataset), pattern = " ", replacement = "")
  missings <- is.na(dataset)
  return(mice::complete(mice(dataset, nnet.MaxNWts = 3000, diagnostics = FALSE, remove_collinear = FALSE, m = 1, maxit = 1, method = 'pmm', where = missings)))
}

# Test

load_result <- load(41278)

name <- load_result$name
dataset <- load_result$dataset

target <- load_result$target

pos=dataset[1, target]
dataset[, target] <- factor(unlist(lapply(dataset[, target], function(x) ifelse(x==pos, 1, 0))), levels=c(1, 0))
colnames(dataset) <- make.names(colnames(dataset),unique = T)

learner <- lrn('classif.ranger', predict_type='prob', num.trees = 100)
result <- train_and_test(dataset, imputer=impute_mice, learner=learner, target=target, positive='1', title=paste0(name, ' + mice'))