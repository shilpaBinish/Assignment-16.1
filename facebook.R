getwd()
setwd("C:\\Users\\User\\Downloads\\Dataset (3)\\Dataset\\Testing")
disease
d <- read.csv("Features_TestSet.csv", na.strings = c(""," ","NA"))
d<-d[!duplicated(d),]
library(Amelia)
missmap(d)
sapply(d,function(x) sum(is.na(x)))
x=na.omit(d)
d<-d[!names(d) %in% c("X")]
d
prop.table(table(d$PagePopularity/likes ))
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
is_outlier
dput(colnames(d))

#Remove Highly correlated 
d1=d %>% select(-findCorrelation(cor(d %>% select(-id,-PagePopularity/likes)),cutoff=.8))
d1
#Random Forest
library("rpart")
library("caret")
set.seed(1234)
df3=cbind(raw_timestamp_part_1=d$PagePopularity/likes ,d1)
index<-createDataPartition(df3$PagePopularity/likes ,times = 1,p = .9,list=FALSE)
training=df3[index,]
testing=df3[-index,]
#########LinearModelling
library("caret")
model_dt=train(PagePopularity/likes ~ .,data=training,method="lm")
model_dt_1 = predict(model_dt, data = training)
cm<-confusionMatrix(model_dt_1,testing$PagePopularity/likes,positive = "M")
cm
library("caret")
set.seed(123)
df3=cbind(PagePopularity/likes=d$PagePopularity/likes ,d1)
index<-createDataPartition(df3$PagePopularity/likes ,times = 1,p = .9,list=FALSE)
training=df3[index,]
testing=df3[-index,]
#Random Forest
library("rpart")
library("caret")
set.seed(1234)
df3=cbind(raw_timestamp_part_1=d$PagePopularity/likes ,d1)
index<-createDataPartition(df3$PagePopularity/likes ,times = 1,p = .9,list=FALSE)
training=df3[index,]
testing=df3[-index,]
library("caret")
model_dt=train(raw_timestamp_part_1 ~ .,data=training,method="cv")
model_dt_1 = predict(model_dt, data = training)
cm<-confusionMatrix(model_dt_1,testing$raw_timestamp_part_1,positive = "M")
cm
