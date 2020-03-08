#Predict iris flower species from flower measurement
#multi-class classification problem
data(iris)
head(iris)
#basic descriptive statistics
str(iris)
dim(iris)
summary(iris)
plot(iris)
boxplot(iris[,-5])
#loading caret and randomForest package
library(caret)
#library(randomForest)<we could build a RF model by using randomForest package>
library(ggplot2)
#creating validation set(80-20  split) and do some basic operation
dataset<-iris
na.omit(dataset)
levels(dataset$Species)
index<-createDataPartition(dataset$Species,p=0.8,list=FALSE)
trainset<-dataset[index,]
testset<-dataset[-index,]
#check for any missing values
any(is.na(dataset))
#building models
control<-trainControl(method = "cv", number = 10)
model.rf<-train(Species~.,data=trainset,method="rf",metric='Accuracy',trControl=control)
model.lda<-train(Species~.,data=trainset,method="lda",metric='Accuracy',trControl=control)
model.rpart<-train(Species~.,data=trainset,method="rpart",metric='Accuracy',trControl=control)
model.knn<-train(Species~.,data=trainset,method="knn",metric='Accuracy',trControl=control)
model.svm<-train(Species~.,data=trainset,method="svmRadial",metric="Accuracy",trControl=control)
#storing all fitted models in a form of list,summary statistics and comparison
results<-resamples(list(rf=model.rf,lda=model.lda,rpart=model.rpart,knn=model.knn))
summary(results)
dotplot(results)

#print the fitted model and we found LDA and KNN gave us the best accuracy
print(model.rf)
print(model.lda)
print(model.rpart)
print(model.knn)

#prediction and confusion matrix for LDA
pred<-predict(model.lda,testset)
confusionMatrix(pred,testset$Species)

#prediction and confusion matrix for KNN
pred<-predict(model.knn,testset)
confusionMatrix(pred,testset$Species)