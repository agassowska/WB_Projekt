library(dplyr)
library(ggplot2)
library(patchwork)

### ranger ###

test_data <- read.csv('./test_data/test_data.csv')
test_data <- test_data[test_data$model=='ranger', ]
test_data_ranked <- test_data %>%
  group_by(dataset) %>% 
  mutate(test_auc_ranked=rank(-test_auc, ties.method="first")) %>%
  mutate(test_bacc_ranked=rank(-test_bacc, ties.method="first")) %>%
  mutate(test_mcc_ranked=rank(-test_mcc, ties.method="first")) %>%
  group_by(package) %>%
  mutate(test_auc_ranked_mean=mean(test_auc_ranked)) %>%
  mutate(test_bacc_ranked_mean=mean(test_bacc_ranked)) %>%
  mutate(test_mcc_ranked_mean=mean(test_mcc_ranked))

auc_1 <- ggplot(test_data_ranked, aes(y=test_auc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_auc_ranked_mean, label=test_auc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

auc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_auc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

auc <- auc_1/auc_2 + plot_annotation(title='ranger model\nAUC measure')

bacc_1 <- ggplot(test_data_ranked, aes(y=test_bacc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_bacc_ranked_mean, label=test_bacc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

bacc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_bacc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

bacc <- bacc_1/bacc_2 + plot_annotation(title='ranger model\nBACC measure')

mcc_1 <- ggplot(test_data_ranked, aes(y=test_mcc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_mcc_ranked_mean, label=test_mcc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

mcc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_mcc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('MCC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

mcc <- mcc_1/mcc_2 + plot_annotation(title='ranger model\nMCC measure')

ggsave(path='./final_plots', filename='ranger_auc.png', plot=auc)
ggsave(path='./final_plots', filename='ranger_bacc.png', plot=bacc)
ggsave(path='./final_plots', filename='ranger_mcc.png', plot=mcc)

### xgboost ###

test_data <- read.csv('./test_data/test_data.csv')
test_data <- test_data[test_data$model=='xgboost', ]
test_data_ranked <- test_data %>%
  group_by(dataset) %>% 
  mutate(test_auc_ranked=rank(-test_auc, ties.method="first")) %>%
  mutate(test_bacc_ranked=rank(-test_bacc, ties.method="first")) %>%
  mutate(test_mcc_ranked=rank(-test_mcc, ties.method="first")) %>%
  group_by(package) %>%
  mutate(test_auc_ranked_mean=mean(test_auc_ranked)) %>%
  mutate(test_bacc_ranked_mean=mean(test_bacc_ranked)) %>%
  mutate(test_mcc_ranked_mean=mean(test_mcc_ranked))

auc_1 <- ggplot(test_data_ranked, aes(y=test_auc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_auc_ranked_mean, label=test_auc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

auc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_auc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

auc <- auc_1/auc_2 + plot_annotation(title='xgboost model\nAUC measure')

bacc_1 <- ggplot(test_data_ranked, aes(y=test_bacc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_bacc_ranked_mean, label=test_bacc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

bacc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_bacc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

bacc <- bacc_1/bacc_2 + plot_annotation(title='xgboost model\nBACC measure')

mcc_1 <- ggplot(test_data_ranked, aes(y=test_mcc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_mcc_ranked_mean, label=test_mcc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

mcc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_mcc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('MCC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

mcc <- mcc_1/mcc_2 + plot_annotation(title='xgboost model\nMCC measure')

ggsave(path='./final_plots', filename='xgboost_auc.png', plot=auc)
ggsave(path='./final_plots', filename='xgboost_bacc.png', plot=bacc)
ggsave(path='./final_plots', filename='xgboost_mcc.png', plot=mcc)

### kNN ###

test_data <- read.csv('./test_data/test_data.csv')
test_data <- test_data[test_data$model=='kNN', ]
test_data_ranked <- test_data %>%
  group_by(dataset) %>% 
  mutate(test_auc_ranked=rank(-test_auc, ties.method="first")) %>%
  mutate(test_bacc_ranked=rank(-test_bacc, ties.method="first")) %>%
  mutate(test_mcc_ranked=rank(-test_mcc, ties.method="first")) %>%
  group_by(package) %>%
  mutate(test_auc_ranked_mean=mean(test_auc_ranked)) %>%
  mutate(test_bacc_ranked_mean=mean(test_bacc_ranked)) %>%
  mutate(test_mcc_ranked_mean=mean(test_mcc_ranked))

auc_1 <- ggplot(test_data_ranked, aes(y=test_auc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_auc_ranked_mean, label=test_auc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

auc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_auc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

auc <- auc_1/auc_2 + plot_annotation(title='kNN model\nAUC measure')

bacc_1 <- ggplot(test_data_ranked, aes(y=test_bacc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_bacc_ranked_mean, label=test_bacc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

bacc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_bacc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

bacc <- bacc_1/bacc_2 + plot_annotation(title='kNN model\nBACC measure')

mcc_1 <- ggplot(test_data_ranked, aes(y=test_mcc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_mcc_ranked_mean, label=test_mcc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

mcc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_mcc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('MCC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

mcc <- mcc_1/mcc_2 + plot_annotation(title='kNN model\nMCC measure')

ggsave(path='./final_plots', filename='kNN_auc.png', plot=auc)
ggsave(path='./final_plots', filename='kNN_bacc.png', plot=bacc)
ggsave(path='./final_plots', filename='kNN_mcc.png', plot=mcc)

### NaiveBayes ###

test_data <- read.csv('./test_data/test_data.csv')
test_data <- test_data[test_data$model=='NaiveBayes', ]
test_data_ranked <- test_data %>%
  group_by(dataset) %>% 
  mutate(test_auc_ranked=rank(-test_auc, ties.method="first")) %>%
  mutate(test_bacc_ranked=rank(-test_bacc, ties.method="first")) %>%
  mutate(test_mcc_ranked=rank(-test_mcc, ties.method="first")) %>%
  group_by(package) %>%
  mutate(test_auc_ranked_mean=mean(test_auc_ranked)) %>%
  mutate(test_bacc_ranked_mean=mean(test_bacc_ranked)) %>%
  mutate(test_mcc_ranked_mean=mean(test_mcc_ranked))

auc_1 <- ggplot(test_data_ranked, aes(y=test_auc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_auc_ranked_mean, label=test_auc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

auc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_auc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

auc <- auc_1/auc_2 + plot_annotation(title='Naive Bayes model\nAUC measure')

bacc_1 <- ggplot(test_data_ranked, aes(y=test_bacc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_bacc_ranked_mean, label=test_bacc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

bacc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_bacc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

bacc <- bacc_1/bacc_2 + plot_annotation(title='Naive Bayes model\nBACC measure')

mcc_1 <- ggplot(test_data_ranked, aes(y=test_mcc_ranked, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  geom_label(aes(x=package, y=test_mcc_ranked_mean, label=test_mcc_ranked_mean), nudge_x=0.24, size=4, color='red', alpha=0.25) +
  ylab('rank') +
  labs(title='Rank by package (lower is better)', subtitle='Each point represents measure value on particular dataset.', caption='Red line and numbers represent mean rank.') +
  theme(plot.caption=element_text(color='red'))

mcc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_mcc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('MCC') +
  ylim(c(0, 1)) +
  labs(title='Value by package and dataset (higher is better)') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

mcc <- mcc_1/mcc_2 + plot_annotation(title='Naive Bayes model\nMCC measure')

ggsave(path='./final_plots', filename='NaiveBayes_auc.png', plot=auc)
ggsave(path='./final_plots', filename='NaiveBayes_bacc.png', plot=bacc)
ggsave(path='./final_plots', filename='NaiveBayes_mcc.png', plot=mcc)

### czasy ###

dirs <- list.files('./time_data')
time_data <- data.frame()
for(dir in dirs) {
  temp_data <- read.csv(paste0('./time_data/', dir, '/time_data.csv'))
  time_data <- rbind(time_data, temp_data)
}

time_1 <- ggplot(time_data, aes(x=package, y=imputation_time)) +
  geom_boxplot(fill='skyblue', alpha=.75) +
  ylab('log(imputation time) (in seconds)') +
  scale_y_log10() +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

time_2 <- ggplot(time_data, aes(x=dataset, color=package, y=imputation_time)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  #geom_line(aes(group=package), alpha=.25) +
  ylab('log(imputation time) (in seconds)') +
  scale_y_log10() +
  labs(title='by package and dataset') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

time <- time_1/time_2 + plot_annotation(title='Imputation time')

ggsave(path='./final_plots', filename='time.png', plot=time)

