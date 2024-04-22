# fit an time series model to the IPD counts

# monthly IPD cases (2015 Jan to 2023 Dec)
y <- c(25, 32, 12, 12, 5, 9, 8, 5, 4, 16, 12, 22,
       16, 15, 10, 20, 24, 10, 11, 7, 13, 16, 18, 30, 
       16, 22, 18, 13, 18, 14, 8, 9, 14, 13, 23, 18, 
       24, 29, 18, 21, 8, 7, 12, 6, 6, 12, 21, 25, 
       30, 21, 17, 14, 6, 13, 10, 10, 6, 15, 20, 27, 
       18, 14, 5, 2, 2, 1, 0, 1, 0, 0, 2, 1, 
       1, 2, 0, 2, 0, 2, 2, 1, 1, 1, 8, 4, 
       3, 1, 6, 3, 0, 0, 3, 1, 0, 4, 1, 6, 
       5, 3, 6, 5, 19, 9, 9, 2, 9, 5, 6, 16)

y <- ts(y, start=c(2015, 1), frequency=12)

# fit the model to tscount

# create table for fitted values from the time series model
library(tscount)

modelp <- tsglm(y[1:61], model = list(past_obs=1, past_mean = 12), distr="nbinom")

fitp <- predict(modelp, 47, estim="normapprox")

data_fit = cbind(time(y)[62:108], fitp$interval[,1], rev(fitp$interval[,2]), fitp$median)
data_fit <- as.data.frame(data_fit)
colnames(data_fit) <- c('time', 'lower', 'upper', 'median')

#create a table for observed values
data_obs = cbind(time(y), y)
data_obs <- as.data.frame(data_obs)
colnames(data_obs) <- c('time', 'count')

# create figure (ggplot)
library(ggplot2)
new_theme <- theme_classic()
theme_set(new_theme)

time_series <- ggplot()+geom_line(data=data_obs, aes(time, count), col = "red", size=0.5)+ 
  geom_line(data=data_fit, aes(time, median), col = "blue", size=0.5) + 
  geom_ribbon(data = data_fit, aes(x=time, ymax=upper , ymin=lower), fill="blue", alpha=0.15)+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 7),axis.text = element_text(size = 7))+
  scale_x_continuous(breaks = seq(2015, 2023, by = 1))+
  ylab(paste("IPD Case Count", sep = ""))+
  xlab ("Year")

ggsave(time_series, file="time_series.pdf", width = 15, height = 8, units = "cm")

