setwd("/Users/nikhilkaul/Documents/Spring Semester 2019/CSC 422/Final Project")
mydata = read.csv(file = "top2018.csv")
my_data <- mydata[, c(4,5,6,7,8,9, 10, 11, 12, 13, 14, 15)]
correlationMatrix = cor(my_data)
round(correlationMatrix, 2)