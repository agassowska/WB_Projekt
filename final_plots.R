library(ggplot2)
library(patchwork)

test_data <- read.csv('./test_data/test_data.csv')

auc_1 <- ggplot(test_data, aes(x=package, y=test_auc)) +
  geom_boxplot(fill='skyblue', alpha=.75) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

auc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_auc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  geom_line(aes(group=package), alpha=.25) +
  ylab('AUC') +
  ylim(c(0, 1)) +
  labs(title='by package and dataset') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

auc <- auc_1/auc_2 + plot_annotation(title='AUC measure')

auc

bacc_1 <- ggplot(test_data, aes(x=package, y=test_bacc)) +
  geom_boxplot(fill='skyblue', alpha=.75) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

bacc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_bacc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  geom_line(aes(group=package), alpha=.25) +
  ylab('BACC') +
  ylim(c(0, 1)) +
  labs(title='by package and dataset') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

bacc <- bacc_1/bacc_2 + plot_annotation(title='BACC measure')

bacc

mcc_1 <- ggplot(test_data, aes(x=package, y=test_mcc)) +
  geom_boxplot(fill='skyblue', alpha=.75) +
  ylab('MCC') +
  ylim(c(-1, 1)) +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

mcc_2 <- ggplot(test_data, aes(x=dataset, color=package, y=test_mcc)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  geom_line(aes(group=package), alpha=.25) +
  ylab('MCC') +
  ylim(c(-1, 1)) +
  labs(title='by package and dataset') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )


mcc <- mcc_1/mcc_2 + plot_annotation(title='MCC measure')

mcc

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
  geom_line(aes(group=package), alpha=.25) +
  ylab('log(imputation time) (in seconds)') +
  scale_y_log10() +
  labs(title='by package and dataset') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

time <- time_1/time_2 + plot_annotation(title='Imputation time')

time

ggsave(path='./final_plots', filename='auc.png', plot=auc)
ggsave(path='./final_plots', filename='bacc.png', plot=bacc)
ggsave(path='./final_plots', filename='mcc.png', plot=mcc)
ggsave(path='./final_plots', filename='time.png', plot=time)
    