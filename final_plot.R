library(ggplot2)
library(patchwork)

test_data <- read.csv('test_data.csv')
time_data <- read.csv('time_data.csv')

auc_1 <- ggplot(test_data, aes(x=package, y=test_auc)) +
  geom_boxplot(fill='skyblue') +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

auc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_auc)) +
  geom_point(size=3, position=position_dodge(width=.5)) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='by package and dataset')

auc <- auc_1/auc_2 + plot_annotation(title='AUC measure')

bacc_1 <- ggplot(test_data, aes(x=package, y=test_bacc)) +
  geom_boxplot(fill='skyblue') +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

bacc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_bacc)) +
  geom_point(size=3, position=position_dodge(width=.5)) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='by package and dataset')

bacc <- bacc_1/bacc_2 + plot_annotation(title='BACC measure')

mcc_1 <- ggplot(test_data, aes(x=package, y=test_mcc)) +
  geom_boxplot(fill='skyblue') +
  ylab('MCC') +
  ylim(c(-1, 1)) +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

mcc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_mcc)) +
  geom_point(size=3, position=position_dodge(width=.5)) +
  ylab('MCC') +
  ylim(c(-1, 1)) +
  labs(title='by package and dataset')

mcc <- mcc_1/mcc_2 + plot_annotation(title='MCC measure')

time_1 <- ggplot(time_data, aes(x=package, y=train_set_imputation_time+test_set_imputation_time)) +
  geom_boxplot(fill='skyblue') +
  ylab('time (in seconds)') +
  scale_y_log10() +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

time_2 <- ggplot(time_data, aes(x=dataset, color=package, y=train_set_imputation_time+test_set_imputation_time)) +
  geom_point(size=3, position=position_dodge(width=.5)) +
  ylab('time (in seconds)') +
  scale_y_log10() +
  labs(title='by package and dataset')

time <- time_1/time_2 + plot_annotation(title='Total imputation time (train_set + test_set)')

ggsave(path='./final_plots', filename='auc.png', plot=auc)
ggsave(path='./final_plots', filename='bacc.png', plot=bacc)
ggsave(path='./final_plots', filename='mcc.png', plot=mcc)
ggsave(path='./final_plots', filename='time.png', plot=time)
    