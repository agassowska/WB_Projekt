source('functions.R')
ids <- list.dirs(path='./datasets_raw', full.names=TRUE)
ids <- gsub("./datasets_raw(/openml_dataset_)?", "", ids)
ids <- na.omit(as.numeric(ids))
ids <- ids[!ids %in% c(1018, 41278)]

time_data <- data.frame(package=character(), dataset=character(), imputation_time=numeric(), stringsAsFactors=FALSE)
for(id in ids) {
  load_result <- load_raw(id)
  name <- load_result$name
  time <- split_and_impute(id, impute_amelia, 'Amelia')$time
  time_data[nrow(time_data)+1, ] <- c('Amelia', name, time)
}
write.csv(x=time_data, file='./time_data/Amelia/time_data.csv', row.names=FALSE)