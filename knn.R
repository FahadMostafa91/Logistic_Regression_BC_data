library(readr)
wdbc <- read_csv("C:/Users/gmostafa/Downloads/wdbc")
View(wdbc)
summary(wdbc)

# https://www.youtube.com/watch?v=lDCWX6vCLFA
#https://www.youtube.com/watch?v=4ObVzTuFivY

wdbc<-wdbc[,-1]
data_norm<-function(x) {((x-min(x))/(max(x)-min(x)))}
wdbc_norm <- as.data.frame(lapply(wdbc[,-1],data_norm))
summary(wdbc_norm[,1:4])

wdbc_train <-wdbc_norm[1:450,]
wdbc_test <-wdbc_norm[451:569,]

library(class)
wdbc_pred <- knn(wdbc_train, wdbc_test, wdbc[1:450,1], k=21)