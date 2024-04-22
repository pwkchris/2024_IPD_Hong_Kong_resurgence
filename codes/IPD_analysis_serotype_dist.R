library(readxl)
library(tidyverse)
library(dplyr)
combined_table <-read_excel("combined_table_PowerBI_202401.xlsx")
combined_table <- data.frame(combined_table)
colnames(combined_table) <- c("colnum", "serotype", "<2", "2-4", "5-17", "18-49", 
                              "50-64", ">65", "total", "year", "month", "date")

by_year_serotype <- combined_table %>% group_by(serotype, year) %>% 
  summarise(total_case = sum(total))

combined_table_analysis = select(combined_table,  c('serotype', 'total', 'year'))
combined_table_analysis = filter(combined_table_analysis, !(year %in% c(2024)))

serotype_pcv13 = c('1', '3', '4', '5', '6A', '6B', '7F', '9V', '14', '18C', '19A', '19F', '23F')
serotype_pcv15_x_13 = c('22F', '33F')
serotype_pcv20_x_15 = c('8', '10A', '11A', '12F', '15B') 
serotype_pcv23_x_20 = c('2', '9N', '17F', '20')

combined_table_13 = filter(combined_table_analysis, serotype %in% serotype_pcv13)
combined_table_15_x13 = filter(combined_table_analysis, serotype %in% serotype_pcv15_x_13)
combined_table_20_x15 = filter(combined_table_analysis, serotype %in% serotype_pcv20_x_15)
combined_table_23_x20 = filter(combined_table_analysis, serotype %in% serotype_pcv23_x_20)
combined_table_non = filter(combined_table_analysis, !(serotype %in% serotype_pcv13), !(serotype %in% serotype_pcv15_x_13), 
                            !(serotype %in% serotype_pcv20_x_15), !(serotype %in% serotype_pcv23_x_20), !(serotype %in% c('Total')))

combined_table_13 = aggregate(combined_table_13$total,  by = list(combined_table_13$year), FUN = sum)
colnames(combined_table_13) = c('year', 'number')
combined_table_13$group = 'pcv13'

combined_table_15_x13 = aggregate(combined_table_15_x13$total, by = list(combined_table_15_x13$year), FUN = sum)
colnames(combined_table_15_x13) = c('year', 'number')
combined_table_15_x13$group = 'pcv15_exclude_pcv13'

combined_table_20_x15 = aggregate(combined_table_20_x15$total, by = list(combined_table_20_x15$year),FUN = sum)
colnames(combined_table_20_x15) = c('year', 'number')
combined_table_20_x15$group = 'pcv20_exclude_pcv15'

combined_table_23_x20 = aggregate(combined_table_23_x20$total, by = list(combined_table_23_x20$year), FUN = sum)
colnames(combined_table_23_x20) = c('year', 'number')
combined_table_23_x20$group = 'pcv23_exclude_pcv20'

combined_table_non = aggregate(combined_table_non$total,by = list(combined_table_non$year), FUN = sum)
colnames(combined_table_non) = c('year', 'number')
combined_table_non$group = 'non_vaccine'

combined_table_analysis_1 = rbind(combined_table_13, combined_table_15_x13, combined_table_20_x15, 
                                combined_table_23_x20, combined_table_non)
combined_table_analysis_1 <- xtabs(number~year+group, data = combined_table_analysis_1) 
combined_table_analysis_1 = as.data.frame.matrix(combined_table_analysis_1)

combined_table_serotype_dist = data.frame()
for (j in 1:length(combined_table_analysis_1)){
  combined_table_serotype_dist[1,j] <- sum(combined_table_analysis_1[rownames(combined_table_analysis_1) %in% c('2015', '2016', '2017', '2018', '2019'),j])
  combined_table_serotype_dist[2,j] <- sum(combined_table_analysis_1[rownames(combined_table_analysis_1) %in% c('2020', '2021', '2022'),j])
  combined_table_serotype_dist[3,j] <- sum(combined_table_analysis_1[rownames(combined_table_analysis_1) %in% c('2023'),j])
}
colnames(combined_table_serotype_dist) = colnames(combined_table_analysis_1)
rownames(combined_table_serotype_dist) = c('pre-pandemic', 'pandemic', 'post-pandemic')

results <- chisq.test(combined_table_serotype_dist)

#post-hoc test
combined_table_serotype_dist_pre_post = combined_table_serotype_dist[-2, ]
results_1 <- chisq.test(combined_table_serotype_dist_pre_post)

combined_table_serotype_dist_pan_post = combined_table_serotype_dist[-1, ]
results_2 <- chisq.test(combined_table_serotype_dist_pan_post)

combined_table_serotype_dist_pre_pan = combined_table_serotype_dist[-3, ]
results_3 <- chisq.test(combined_table_serotype_dist_pre_pan)

##############################
serotype_pcv15 = c('1', '3', '4', '5', '6A', '6B', '7F', '9V', '14', '18C', '19A', '19F', '23F','22F', '33F')
serotype_pcv20 = c('1', '3', '4', '5', '6A', '6B', '7F', '9V', '14', '18C', '19A', '19F', '23F','22F', '33F','8', '10A', '11A', '12F', '15B') 
serotype_pcv23 = c('1', '3', '4', '5', '6B', '7F', '9V', '14', '18C', '19A', '19F', '23F','22F', '33F','8', '10A', '11A', '12F', '15B','2', '9N', '17F', '20')

combined_table_15 = filter(combined_table_analysis, serotype %in% serotype_pcv15)
combined_table_20 = filter(combined_table_analysis, serotype %in% serotype_pcv20)
combined_table_23 = filter(combined_table_analysis, serotype %in% serotype_pcv23)

combined_table_15 = aggregate(combined_table_15$total, by = list(combined_table_15$year), FUN = sum)
colnames(combined_table_15) = c('year', 'number')
combined_table_15$group = 'pcv15'

combined_table_20 = aggregate(combined_table_20$total, by = list(combined_table_20$year),FUN = sum)
colnames(combined_table_20) = c('year', 'number')
combined_table_20$group = 'pcv20'

combined_table_23 = aggregate(combined_table_23$total, by = list(combined_table_23$year), FUN = sum)
colnames(combined_table_23) = c('year', 'number')
combined_table_23$group = 'pcv23'

combined_table_analysis_table = rbind(combined_table_13, combined_table_15, combined_table_20, 
                                combined_table_23, combined_table_non)
combined_table_analysis_table <- xtabs(number~year+group, data = combined_table_analysis_table) 
combined_table_analysis_table = as.data.frame.matrix(combined_table_analysis_table)

combined_table_all = filter(combined_table_analysis, serotype %in% c('Total'))
combined_table_all = aggregate(combined_table_all$total,  by = list(combined_table_all$year), FUN = sum)
colnames(combined_table_all) = c('year', 'number')
combined_table_all$group = 'all'
