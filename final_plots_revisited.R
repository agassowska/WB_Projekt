library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)

### measures_plot ###

test_data <- read.csv('./test_data/test_data.csv')

measures_plot_data <- melt(test_data)

measures_subplot <- ggplot(measures_plot_data, aes(x=dataset, color=package, y=value)) +
  geom_point(size=2, position=position_dodge(width=.75)) +
  ylim(c(0, 1)) +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

measures_plot <- measures_subplot +
  facet_grid(rows = vars(variable), cols = vars(model), switch='y') +
  labs(title='Measure value (on test subset) by package and dataset for each measure/model combination',
       subtitle='higher is better') +
  theme(plot.title=element_text(size=16),
        plot.subtitle=element_text(size=14),
        strip.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.text=element_text(size=10))

measures_plot

### rankings_plot ###

test_data <- read.csv('./test_data/test_data.csv')

rankings_plot_data <- test_data %>%
  group_by(model, dataset) %>% 
  mutate(test_auc_ranked=rank(-test_auc, ties.method="first")) %>%
  mutate(test_bacc_ranked=rank(-test_bacc, ties.method="first")) %>%
  mutate(test_mcc_ranked=rank(-test_mcc, ties.method="first")) %>%
  select(-c('test_auc', 'test_bacc', 'test_mcc'))

rankings_plot_data <- melt(rankings_plot_data)

rankings_subplot <- ggplot(rankings_plot_data, aes(y=value, x=package)) +
  geom_boxplot(fill='skyblue', alpha=.75, outlier.alpha=0) +
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.18, fill='red', alpha=0.5) +
  stat_summary(fun=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..), width=.75, color='red') +
  stat_summary(fun=mean, geom="label", aes(label=round(..y.., digits=1)), color='red', alpha=0.75) +
  xlab('') +
  ylab('')

rankings_plot <- rankings_subplot +
  facet_grid(rows = vars(variable), cols = vars(model), switch='y') +
  labs(title='Ranked measure value (on test subset) by package for each measure/model combination',
       subtitle='lower is better', caption='Each red point represents ranked position on particular dataset.\nEach red line and number represent mean rank.') +
  theme(plot.title=element_text(size=16),
        plot.subtitle=element_text(size=14),
        strip.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.text=element_text(size=10),
        plot.caption=element_text(size=10, color='red'))

rankings_plot
  
### times_plot ###

dirs <- list.files('./time_data')
time_data <- data.frame()
for(dir in dirs) {
  temp_data <- read.csv(paste0('./time_data/', dir, '/time_data.csv'))
  time_data <- rbind(time_data, temp_data)
}

time_subplot_1 <- ggplot(time_data, aes(x=package, y=imputation_time)) +
  geom_boxplot(fill='skyblue', alpha=.75) +
  ylab('log(imputation time) (in seconds)') +
  scale_y_log10() +
  labs(title='by package') +
  theme(
    legend.position='none'
  )

time_subplot_2 <- ggplot(time_data, aes(x=dataset, color=package, y=imputation_time)) +
  geom_point(size=2, position=position_dodge(width=.6)) +
  ylab('log(imputation time) (in seconds)') +
  scale_y_log10() +
  labs(title='by package and dataset') +
  theme(
    axis.text.x=element_text(angle=45, vjust=0.5)
  )

times_plot <- time_subplot_1 / time_subplot_2 + 
  plot_annotation(title='Imputation time') +
  theme(plot.title=element_text(size=16),
        plot.subtitle=element_text(size=14),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.text=element_text(size=10))
times_plot
