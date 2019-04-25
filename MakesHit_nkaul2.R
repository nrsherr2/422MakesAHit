library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2)
set.seed(123)

setwd("/Users/nikhilkaul/Documents/Spring Semester 2019/CSC 422/Final Project")
mydata = read.csv(file = "spotify_training_data.csv")
my_data <- mydata[, c(2,3,4,5,6,7,8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]

p = ggplot(my_data,aes(x=danceability,  
                    y=energy, 
                    color=genre))

p + geom_jitter(alpha=0.3) +  
  scale_color_manual(breaks = c('country','edm', 'rock', 'rap', 'pop', 'metal'),
                     values=c('darkgreen','red', 'blue', 'black', 'green', 'orange'))

##sample <- sample.int(n = nrow(my_data), size = floor(.66*nrow(my_data)), replace = F)
sample <- sample(1:3, size = nrow(my_data), prob = c(.6, .2, .2), replace = T)
train <- my_data[sample == 1, ]
validation <- my_data[sample ==2, ]
test  <- my_data[sample == 3, ]

x <- validation[,4:17]
y <- validation[,18]
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=300)

rf = randomForest(genre ~ energy + danceability + speechiness + valence + liveness + instrumentalness
                    + acousticness + loudness + popularity + tempo + mode + explicit + key,  ntree = 100,data = train, mtry=4)
plot(rf, main = "Random Forest Algorithm")
legend("topright", 
       legend = c("OOB", "country", "edm", "metal", "pop", "rap", "rock"),
       fill = c("black", "red", "green", "blue", "cyan", "pink", "black"), cex = .70
)
print(rf)

train$predicted.response = predict(rf , train)
print(confusionMatrix(data = train$predicted.response, reference = train$genre))

test$predicted.response = predict(rf , test)
print(confusionMatrix(data = test$predicted.response, reference = test$genre))



