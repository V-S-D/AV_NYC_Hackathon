# AV_NYC_Hackathon
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
av <- read.csv("~/Downloads/AV/nyc/avtrain.csv")
test <- read.csv("~/Downloads/AV/nyc/avtest.csv")
str(av)
av$ID=NULL
ID=test$ID
test$ID = NULL

cat("feature engineering")
temp=av$TIMESTAMP
year=as.numeric(substr(temp,1,4))
month=as.numeric(substr(temp,5,6))
day=as.numeric(substr(temp,7,8))
time=as.numeric((substr(temp,10,10)))

temp2=test$TIMESTAMP
year2=as.numeric(substr(temp2,1,4))
month2=as.numeric(substr(temp2,5,6))
day2=as.numeric(substr(temp2,7,8))
time2=as.numeric((substr(temp2,10,10)))

v1=sqrt((av$U10)^2 + (av$V10)^2)
v2=sqrt((av$U100)^2 + (av$V100)^2)
av$U10=av$U100=av$V10=av$V100=NULL
av$v1=v1
av$v2=v2
av$year=year
av$month=month
av$day=day
av$time=time
test$year=year2
test$month=month2
test$day=day2
test$time=time2
av$TIMESTAMP=NULL

str(av)
v1=sqrt((test$U10)^2 + (test$V10)^2)
v2=sqrt((test$U100)^2 + (test$V100)^2)
test$U10=test$U100=test$V10=test$V100=NULL
test$v1=v1
test$v2=v2
test$TIMESTAMP=NULL
str(test)

#tree=rpart(TARGETVAR~.,data=av)
#prp(tree)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:10)*0.0001)
tr=train(TARGETVAR~ZONEID+v1+v2+month,data=av,method="rpart",trControl=tr.control,tuneGrid=cp.grid)
tr
best.tree = tr$finalModel
#prp(best.tree)
pred = predict(best.tree, newdata=test)
submission=data.frame(ID=ID,TARGETVAR=pred)
write.csv(submission, "nyc.csv", row.names = F);
