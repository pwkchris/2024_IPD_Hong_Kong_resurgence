library(readxl)
library(tidyverse)
library(dplyr)

case_count_table <- read_excel('case_count_all_years.xlsx', 'Table')

title_list <- colnames(case_count_table)[2:6]

case_count_table = select(case_count_table,  -c('Year'))  #remove the col of year 

for (i in 1:9){
  case_count_table[i, 'Others'] <- case_count_table[i, 6] - sum(case_count_table[i, 1:5])
}

row.names(case_count_table)= 2015:2023
colnames(case_count_table) <- c("serotype_3", 'serotype_19A', 'serotype_14', 'serotype_23A', 'serotype_15A', 'Total','Others')

new_theme <- theme_classic()
theme_set(new_theme)

graph_all <- ggplot(case_count_table, aes(2015:2023)) +       # Create ggplot2 plot
geom_line(aes(y = serotype_3, group=1), color= 'yellow') +
geom_line(aes(y = serotype_19A, group=1), color= 'green') +
geom_line(aes(y = serotype_14,group=1), color= 'blue') + 
geom_line(aes(y = serotype_23A, group=1), color = 'red' ) +
geom_line(aes(y = serotype_15A, group=1), color = 'orange') +
geom_line(aes(y = Total, group=1), color = 'black') +  
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11),axis.text = element_text(size = 11),
      axis.line = element_line(color = "black", linewidth = .4, lineend = "square"), axis.ticks = element_line(color = "black", size = 0.4), 
      axis.ticks.length=unit(.1, "cm"))+scale_x_continuous(breaks = seq(2015, 2023, by = 1))+
xlab(paste("Year", sep = ""))+
  ylab ("Number of Cases")+ ggtitle('Number of Cases per Year') 

ggsave(graph_all, file=paste0("All_plot_1",".pdf"), width = 15, height = 12, units = "cm")

case_percentage <- read_excel('case_count_all_years.xlsx', 'Percentage')
graph_dist <- ggplot(data = case_percentage, aes(x = Year, y = Percentage, fill = Serotype)) + 
geom_bar(position = "fill", stat = "identity") +
theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 11),axis.text = element_text(size = 11),
        axis.line = element_line(color = "black", linewidth = .4, lineend = "square"), axis.ticks = element_line(color = "black", size = 0.4), 
        axis.ticks.length=unit(.1, "cm"))+scale_x_continuous(breaks = seq(2015, 2023, by = 1))+
xlab(paste("Year", sep = ""))+
ylab ("Percentage of Cases")+ ggtitle('Serotype Distribution by Year')   

ggsave(graph_dist, file=paste0("graph_dist_1",".pdf"), width = 15, height = 12, units = "cm")

