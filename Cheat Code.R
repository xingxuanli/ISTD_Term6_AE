#Sequence generation
seq(2,10,by = 2)
seq(2,10,length = 2)
seq(6,-1,by = -2)
seq(3,30,length = 11) #num 

#rep
rep(2,4)#2 2 2 2 
rep(c(1,5),4)#1 5 1 5 1 5 1 5
rep(c(1,2),c(4,4))#1 1 1 1 2 2 2 2

#Matrix
x = matrix(c(3,-1,2,-1),nrow = 2,ncol = 2)
#     [,1] [,2]   
#[1,]   6    4
#[2,]  -2   -2
# '*' element-wise multiplication
# '%*%' matrix multiplication

#NA relevant commands
#Summary: to see how many NAs in a feature. 
summary(Q6_data$Smartphone)
#sum(is.na) also works
sum(is.na(Q6_data$Smartphone)) 
#Returns the features with NAs
which(is.na(limited))

#Table: How many / proportion 
table(Q6_data$Smartphone)
#useNA = 'No' - default
table(Q6_data$Internet.Use,Q6_data$Smartphone,dnn = c('Internet.Use', 'Smartphone'))
#The largest number of people that have exactly same value in A and B
max(table(limited$Info.On.Internet,limited$Age))

#Tapply
tapply('1','2',mean / sum / table / sd  )

#Subset 
subset(Q6_data, Q6_data$Internet.Use == 1|Q6_data$Smartphone == 1)

#GGplot
ggplot(data = parole, aes(x = Age,fill = as.factor(Male))) + geom_histogram(binwidth=5,closed=c("left"),
                                                                            color ='red',center=17.5,
                                                                            position = 'identity',
                                                                            alpha = '0.5') + facet_grid(Male~.)

#Confidence Interval for training set
confint(model1,level = 0.99)

#Confidence Interval for test set
#Method 1
pred <- predict(model1, newdata = )
pred$confint(level = 0.99)
#Method 2
predict.lm(m1,newdata = data.frame(horsepower=98),interval=c('confidence'),level = .99)

#Correlation -> In simple linear model, cor^2 = R^2 = 1-sse/sst
cor()
R^2 = cor^2
R^2 = 1-sse/sst
#To exclude qualitative variables in corrleation, create subset first. 
auto1 <- subset(auto,select = -c(name))
cor(auto1)

#plot
plot(data.x,data.y)
abline(model1)

#Multicollinearity can be used to explain why an univerate model overshadows a multivariate model.

#Plot univariate model's coefficient vs multivariate model's coefficient 
x <- c(model1$coef[2],model2$coef[2])
y <- modellall$coef[2:3]
plot(x,y,main ='',xlab = '',ylab = '')

#Polynomial linear regression
modelploy <- lm(medv~poly(lstat,2,raw+TRUE), data = boston)
summary(modelploy)

#Subset/split
#Linear regression - 1
trainingSet <- subset(dataSet, dataSet$Year <= 2006)
testSet <- subset(dataSet, dataSet$Year > 2006)
#Linear regression - 2
split <- sample.split(Paroles$Violator,SplitRatio = 0.7)
train <- subset(Parole,split = TRUE)
test <- subset(Parole,split ==FALSE)
#Logistic regression
trainid <- sample(1:nrow(College),0.8*nrow(College))
testid <- -trainid
train<- College[trainid,]
test<- College[testid,]

#Calculate R^2 for test set. 
pred <- predict(m_simplified,newdata = testingSet)
sse <- sum((climate$Temp - pred)^2)
sse <- mean(model1$residuals^2)
sst <- sum((climate$Temp - mean(climates$Temp))^2)
testR^2 <- 1-sse/sst

#T-test
t.test(dataSet$SLG[dataSet$Year == 1996],dataSet$SLG[dataSet$Year = 2006])#two-sided

#Prediction for logistic regression
pred <- predict(model1, newdata = test, type='response') #response will return probability instead of odds.
max(pred) #Return the maximum prob. 
table(as.integer(pred > 0.5), test$Violator)# Confusion matrix with 0.5 threshold. 

#ROC curve
library(ROCR)
predrocr <- prediction(pred, test$Violator)
auc <- performance(predrocr, measure = "auc")@y.values
auc

#Build-up a glm model with nothing
model1 <- glm(resp~-1,data=training,family=binomial)
#Build-up a glm model with only intercept
model1 <- glm(resp~1,data=training,family=binomial)

#LL(beta)<Log likelihood> = -0.5*residual-deviance

