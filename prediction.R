
`|.character` = function(e1,e2){grep(e2,e1,value=T)}
`|.default` =.Primitive("|")
`|` = function (e1, e2) UseMethod('|')


library(caret)
library(randomForest)
library(mlbench)





training=download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","trainingraw.csv")
testing=download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","testing.csv")

#command="head -n 2 training.csv  |tail -n 1|sed 's:,:\\n:g'|cat -n |grep -v '\"\"'|sed 's:\\t:;:'|cut -d ';' -f 1"
command="sed 's:\"\"::g' trainingraw.csv > training.csv"
system(command)
#whichar=whichar=as.integer(system(command,intern=T))
vec=rep("numeric",160)
whichar=1:7
vec[whichar]=rep("character",length(whichar))
vec[160]="factor"
trainingraw=read.csv("training.csv",na.strings=c("N",NA,"", "#DIV/0!"),stringsAsFactors = F)
nacolumns=unlist(lapply(1:160,function(i){
        sum(is.na(trainingraw[,i]))
}))
removecol=which(nacolumns > 2)
rtrainingraw=trainingraw[,-removecol]
ti=complete.cases(rtrainingraw)
atraining=rtrainingraw[ti,-(1:7)]
set.seed(8)
atraining$index=1:dim(atraining)[1]
library(dplyr)

getevensample=unlist(lapply(levels(as.factor(atraining$classe)),function(cl){sample_n(filter(atraining,classe==cl),40)$index}))
library(data.table)
training=filter(atraining,index %in% getevensample)
        #sample_n(atraining,200)
training=select(training,-index)
testing=read.csv("testing.csv")
#TrainData <- select(training,-classe)
#TrainClasses <- training$classe
library(caret)
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
#,classProbs=TRUE,summaryFunction = twoClassSummary)
#working knnFit <- train(classe ~ ., data = training, method = "knn",
#                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 4)
library(mlbench)
#noout=training[,1:(length(training)-1)]
#stopifnot(length(which(lapply((noout),class)=="character"))==0)
#which(lapply((noout),class)=="character")
#correlationMatrix <- cor(noout)
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
#print(highlyCorrelated)
#nctraining=training[,-highlyCorrelated]
nctraining=training
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(classe~., data=nctraining,
               method="lvq", preProcess="pca", trControl=control)
model2 <- train(classe~., data=nctraining, method="rf", preProcess="scale", trControl=control)

importanceLVQ <- varImp(model,scale=T)
importance <- varImp(model2,scale=F)
# summarize importance
print(importance)
# plot importance
plot(importance)
cm=(confusionMatrix(model2))
library(ggplot2)
library(reshape2)
nba.m=melt(cm$table*5)
p <- ggplot(nba.m, aes(Prediction, Reference)) + geom_tile(aes(fill = value),colour = "white")+  scale_fill_gradient2(low="#006400", mid="#f2f6c3",high="#cd0000",midpoint=0.5)


control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x=noout, y=as.factor(training$classe), sizes=c(1:17), rfeControl=control,
               metric="Accuracy")
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
#knnFit <- train(classe ~ ., data = training, method = "knn",
 #                               trControl = ctrl, preProcess = c("center","scale"), tuneLength = 4)
                

