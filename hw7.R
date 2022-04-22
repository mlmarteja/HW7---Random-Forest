##########Imports###############
#import randomFores
install.packages("randomForest")
require(randomForest)
#import MASS
install.packages("MASS") 
require(MASS)
#import ggplot
library(ggplot2)
#attach allows us to access the variables in the Boston data frame
attach(Boston)
#returns the dimentions of the Boston dataframe
dim(Boston)

#this function sets the starting number used to generate a
#squauence of random numbers to 101
set.seed(101)
#a variable called train will contain the a set of randomlly
#training sample with 300 observations
train=sample(1:nrow(Boston),300)


#Why was the training set created differently than how we created
#training and testing data sets when writing code for Bayesian
#Classification and Logistic Regression?
#Ans:
#They are different because Logistic Regression and Bayesian classification's
# dataset are created to privde the probability of each obersevation falling into
# category. Whereas, randomforest data set is a randomlly selected subset.

#Uses the Boston dataframe to create a random forest
#uses a set of index vector with the train dataset
#medv~. - medv is the dependent variable and the "~."
# referrres to "all other varaibles" as the indepedent varaibles
Boston.rf=randomForest::randomForest(medv ~ . , data = Boston ,
                                     subset = train)
#print Boston.rf on console
Boston.rf
#plot Boton.rf
#In this plot, we have the x axis with the number of trees created and the y axis with the error
#When looking at the graph, as the number of trees increase, then the number of error decreases.
plot(Boston.rf)
#This is making possible 13 Out of Bag Sample Errors found at each split
oob.err=double(13)
#This is making possible 13 Test set Error found at each split
test.err=double(13)

#loop to create 13 different predictions
for(mtry in 1:13){
  #rf contains the returned values of the expoeted variable randomForest
  # in the function of randomForest. The dataset used is Boston. The subset
  # is train. This will indicate which rows will be used.The number of
  # trees to grow is 400.
  rf=randomForest::randomForest(medv ~ . ,
                                data = Boston ,
                                subset = train,
                                mtry=mtry,
                                ntree=400)
  #out of bounds error holds 400 mean standard error from the random forest
  oob.err[mtry] = rf$mse[400]
  
  #stores a prediction of the remaining set of values in Boston that were not in the
  #training set and adding it into the random forest. This set is stored into pred.
  pred<-predict(rf,Boston[-train,]) 
  
  #error test data set
  test.err[mtry]=with(Boston[-train,],mean((medv - pred)^2))
  
  #concatnate the randomlly generated prediction
  cat(mtry," ")
}

#Why is there a for loop?
#Ans:
#The for loop is used to crate 13 predicted trees and different mtry values.

#prints test error
test.err

#print Out of Bound error
oob.err # what is OOB stand for and what does it have to do with Randome Forests? How does Bootstrap come into this?
#OOB = Out of Bag
#it is a way to validate the random forest model.
#The bootstrap is a part of this because a bootstrap sample
#selected will act as a training sample and the dataset that
#is not included is the Out of Bag sample that will not be
#used as training data

#Plotting both Test Error and Bag Error
#The red line is the Out of Bag Error estimates and the blue line is the 
#Error calucated on the test set. When looking a the the lines, we can see that
#the error minimizes after the fourth split. 
matplot(1:mtry, cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
#what is a split?
#Ans:a split in a part of a decision tree within the random forest
legend("topright",legend=c("Out of Bag (Validation Set) Error","Test (Training Set) Error"),pch=19, col=c("red","blue"))


#Is the above code doing a classification or using regression to predict a value?
#What is the difference between those two things?
#How is each calculated? (use words to explain, not code)
#Ans:
#The code above is using regression to predict a value.
#Classifcation of a random forest is created to model predictor variables
# and categorical responses
#Regression of a random feorest predicts the mean or average of the individual
#trees returned which is what was plotted above.
#The regression is calculated 

#This uses the out of bag data
#The impotance method permutes the OOB data from the Boston random forest to record it's prediction error
#The same will be done to a predictor variable
#the difference between those are averaged.
#that data is then kept in ImpData
ImpData <- as.data.frame(importance(Boston.rf))
#Stores the Varaible names into ImpData
ImpData$Var.Names <- row.names(ImpData)

#This graphs the different varialbes in the ImpData to its node purity
#Node purity is a metric that calculates the split of a tree.
#This is essencially a way to measure the importance of each variable
#When looking at the graph, the variables rm and lstat shows high purity,
#this means that if you were to pick a point, it would most likely belong to
#the variables with the highest purity.
ggplot(ImpData, aes(x=Var.Names, y=`IncNodePurity`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`IncNodePurity`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#Explain how to read and understand each visualization the code produces.

