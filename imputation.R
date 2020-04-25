source('functions.R')
ids <- list.dirs(path='./datasets_raw', full.names=TRUE)
ids <- gsub("./datasets_raw(/openml_dataset_)?", "", ids)
ids <- na.omit(as.numeric(ids))
ids <- ids[!ids==41278]

# basic
time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
# or
# time_data <- read.csv('./time_data/basic/time_data.csv')
# if you want to add to existing (dont duplicate datasets then!)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_basic, 'basic')$time
  time_data[nrow(time_data)+1, ] <- c('basic', name, time)
}
write.csv(x=time_data, file='./time_data/basic/time_data.csv', row.names=FALSE)

# missRanger
time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_missRanger, 'missRanger')$time
  time_data[nrow(time_data)+1, ] <- c('missRanger', name, time)
}
write.csv(x=time_data, file='./time_data/missRanger/time_data.csv', row.names=FALSE)

# VIM_knn
time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_VIM_knn, 'VIM_knn')$time
  time_data[nrow(time_data)+1, ] <- c('VIM_knn', name, time)
}
write.csv(x=time_data, file='./time_data/VIM_knn/time_data.csv', row.names=FALSE)

# VIM_hotdeck
time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_VIM_knn, 'VIM_hotdeck')$time
  time_data[nrow(time_data)+1, ] <- c('VIM_hotdeck', name, time)
}
write.csv(x=time_data, file='./time_data/VIM_hotdeck/time_data.csv', row.names=FALSE)

# softImpute
time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_VIM_knn, 'softImpute')$time
  time_data[nrow(time_data)+1, ] <- c('softImpute', name, time)
}
write.csv(x=time_data, file='./time_data/softImpute/time_data.csv', row.names=FALSE)

# mice
time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_mice, 'mice')$time
  time_data[nrow(time_data)+1, ] <- c('mice', name, time)
}
write.csv(x=time_data, file='./time_data/mice/time_data.csv', row.names=FALSE)