# install.packages("caret"); install.packages("randomForest"); install.packages("rpart"); 
library(lattice); library(ggplot2); library(caret); library(randomForest); library(rpart); library(rpart.plot);
library(caret); library(randomForest); library(rpart); library(RColorBrewer); library(rattle)

set.seed(1234)
#Random forest model
library(caret)
library(mlbench)
set.seed(3)


# First step is to load and clean the data
#To load
#read.csv(file_dest_training, na.strings=c("NA",""), header=TRUE)
trainingset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testingset <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))

##Additionally, the training data set can be found on the following URL:
trUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

##The testing data set can be found on the following URL:
teUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

##Load data to memory.
training <- read.csv(url(trUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(teUrl), na.strings=c("NA","#DIV/0!",""))
training <- training[,colSums(is.na(training)) == 0]
testing <- testing[,colSums(is.na(testing)) == 0]

##Partioning the training set into two
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]
##plot to just see what kind of data we are dealing with
plot(myTRraining$classe, col="lightgreen", main = "Plot of levels of variable classe within the TrainTrainingSet data set", xlab="classe", ylab="Frequency" )




##prediction model 1: Using Decision Tree

model <- rpart(classe ~ ., data=myTRaining, method="class")

prediction1 <- predict(model, myTEsting, type = "class")
fancyRpartPlot(modFitA1)
# Plot the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)


##predicting
predictionsA1 <- predict(modFitA1, myTEsting, type = "class")

##confusion matrix
confusionMatrix(predictionsA1, myTEsting$classe)

## Confusion Matrix 
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1251  149   15   61   17
##          B   38  572   75   60   75
##          C   39  117  696  117  122
##          D   49   58   51  508   58
##          E   18   53   18   58  629
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7455          
##                  95% CI : (0.7331, 0.7577)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6774          
##                                           
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8968   0.6027   0.8140   0.6318   0.6981
## Specificity            0.9310   0.9373   0.9024   0.9473   0.9633
## Pos Pred Value         0.8379   0.6976   0.6379   0.7017   0.8106
## Neg Pred Value         0.9578   0.9077   0.9583   0.9292   0.9341
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2551   0.1166   0.1419   0.1036   0.1283
## Detection Prevalence   0.3044   0.1672   0.2225   0.1476   0.1582
## Balanced Accuracy      0.9139   0.7700   0.8582   0.7896   0.8307



Random Forests:Using ML algorithms for prediction
modFitB1 <- randomForest(classe ~. , data=myTRaining)
prediction2 <- predict(model2, myTEsting, type = "class")
predictionsB2 <- predict(modFitB1, TEsting, type = "class")
confusionMatrix(prediction2, myTEsting$classe)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2232    2    0    0    0
##          B    0 1516    4    0    0
##          C    0    0 1362    5    0
##          D    0    0    2 1280    0
##          E    0    0    0    1 1442
## 
## Overall Statistics
##                                         
##                Accuracy : 0.998         
##                  95% CI : (0.997, 0.999)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.998         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    0.999    0.996    0.995    1.000
## Specificity             1.000    0.999    0.999    1.000    1.000
## Pos Pred Value          0.999    0.997    0.996    0.998    0.999
## Neg Pred Value          1.000    1.000    0.999    0.999    1.000
## Prevalence              0.284    0.193    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.163    0.184
## Detection Prevalence    0.285    0.194    0.174    0.163    0.184
## Balanced Accuracy       1.000    0.999    0.997    0.998    1.000---------this shows that it is the better results

predictionsB2 <- predict(modFitB1, TEsting, type = "class")
finalpredict <- predict(model2, TEsting, type="class")
finalpredict

##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
##REFERENCE
###https://newsroom.clevelandclinic.org/2021/09/29/90-percent-of-heart-disease-is-preventable-through-healthier-diet-regular-exercise-and-not-smoking/
