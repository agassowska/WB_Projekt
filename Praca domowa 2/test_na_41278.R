load_result <- load(41278)

name <- load_result$name
dataset <- load_result$dataset
names(dataset)<-str_replace_all(names(dataset), pattern = " ", replacement = "")
target <- load_result$target

pos=dataset[1, target]
dataset[, target] <- factor(unlist(lapply(dataset[, target], function(x) ifelse(x==pos, 1, 0))), levels=c(1, 0))
colnames(dataset) <- make.names(colnames(dataset),unique = T)

learner <- lrn('classif.ranger', predict_type='prob', num.trees = 100)
result <- train_and_test(dataset, imputer=impute_mice, learner=learner, target=target, positive='1', title=paste0(name, ' + mice'))