setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('functions.R')
ids <- list.dirs(path='./datasets_raw', full.names=TRUE)
ids <- gsub("./datasets_raw(/openml_dataset_)?", "", ids)
ids <- na.omit(as.numeric(ids))
ids <- ids[!ids==41278]

time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_mice, 'mice')$time
  time_data[nrow(time_data)+1, ] <- c('mice', name, time)
}
write.csv(x=time_data, file='./time_data/mice/time_data.csv', row.names=FALSE)
