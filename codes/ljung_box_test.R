# read case number data
y <- c(25, 32, 12, 12, 5, 9, 8, 5, 4, 16, 12, 22,
       16, 15, 10, 20, 24, 10, 11, 7, 13, 16, 18, 30, 
       16, 22, 18, 13, 18, 14, 8, 9, 14, 13, 23, 18, 
       24, 29, 18, 21, 8, 7, 12, 6, 6, 12, 21, 25, 
       30, 21, 17, 14, 6, 13, 10, 10, 6, 15, 20, 27, 
       18, 14, 5, 2, 2, 1, 0, 1, 0, 0, 2, 1, 
       1, 2, 0, 2, 0, 2, 2, 1, 1, 1, 8, 4, 
       3, 1, 6, 3, 0, 0, 3, 1, 0, 4, 1, 6, 
       5, 3, 6, 5, 19, 9, 9, 2, 9, 5, 6, 16)

# seperate the data into the period 2015-2019 and 2020-2023
data_pre_COVID = y[1:60]
data_post_COVID = y[61:108]

#conduct Ljung-Box test
Box.test(data_pre_COVID, lag = 12, type = "Ljung-Box")
Box.test(data_post_COVID, lag = 12, type = "Ljung-Box")
