###### Matrix
x = matrix(c(3,-1,2,-1),nrow = 2,ncol = 2)
#     [,1] [,2]   
#[1,]   6    4
#[2,]  -2   -2
# '*' element-wise multiplication
# '%*%' matrix multiplication

###### Subset 
subset(Q6_data, Q6_data$Internet.Use == 1|Q6_data$Smartphone == 1)

###### Train Test Split
library(caTools)
set.seed(2000)
spl <- sample.split(data$over50k, SplitRatio = 0.6)
train <- subset(data, spl==TRUE)
test <- subset(data, spl==FALSE)

###### CART
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
# Regression
cart1 <- rpart(over50k ~., data=train)
# Classification
cart2 <- rpart(over50k ~., data=train, method = 'class')
prp(cart1) 
prp(cart1,type=1) # labels all nodes (not just the leaves)
prp(cart1,type=4) # draws separate labels for leaf and right directions for all nodes and label nodes
prp(cart1,type=4,extra=4) # in addition, this also plots the probability per class of observation
fancyRpartPlot(cart1)
# Prediction
predictcart1 <- predict(cart1,newdata=test,type="class")
# Pruning
plotcp(cart1)
opt <- which.min(cart1$cptable[,"xerror"]) ## find the smallest xerror
cp <- cart1$cptable[opt, "CP"] # 0.03589744
cart2 <- prune(cart1,cp=0.035897)
# Prepruning
cart3 <- rpart(rev~.,method="class",
               maxdepth= 30, 
               minbucket = 20)

# MSE
mse1 <- mean((pred - test$medv)^2)

###### Random Forest
library(randomForest)
forest <- randomForest(rev~.,nodesize=5,ntree=250,type='class')
# The prediction is carried out through majority vote (across all trees)
predictforest <- predict(forest,newdata=test,type="class")

###### Text Sentiment analysis
# Load the tm package.
library(tm)
library(SnowballC)
# Build a new corpus variable called corpus.
corpus <- Corpus(VectorSource(emails$text))
# Using tm map, convert the text to lowercase.
corpus <- tm_map(corpus, content_transformer(tolower))
# Using tm map, remove all punctuation from the corpus.
corpus <- tm_map(corpus, removePunctuation)
# Using tm map, remove all English stopwords from the corpus.
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove some words
corpus <- tm_map(corpus,removeWords,c("drive","driving","driver","self-driving","car","cars"))
# Using tm map, stem the words in the corpus.
corpus <- tm_map(corpus, stemDocument)
# Build a document term matrix from the corpus, called dtm
dtm <- DocumentTermMatrix(corpus)

# With this function, we find term appearing with frequency lower than 50%
findFreqTerms(dtm,lowfreq=50) # 50%
# We can also check the frequency of specific words
dtm[,"accid"]
# In this specific case, we remove all terms with at least 99.5% empty entries
dtm <- removeSparseTerms(dtm,0.995)

# We transform the term-document matrix into a matrix and, then, into a dataframe
twittersparse <- as.data.frame(as.matrix(dtm))
# This helps ensure that columns have valid names. 
# For example, names starting with numbers are modified (e.g., 300k -> X300k).
colnames(twittersparse) <- make.names(colnames(twittersparse))
# Most frequently appeared word
which.max(colSums(twittersparse))

# Get word counts in decreasing order
word_freqs = sort(colSums(twittersparse), decreasing=TRUE)




###### Confusion matrix (accuracy)
table1 <- table(test$over50k, pred >=0.5)
sum(diag(table1))/sum(table1)

###### ROC curve
library(ROCR)
predrocr <- prediction(pred[,2], test$Violator)
perf <- performance(predrocr, x.measure="fpr",measure="tpr") # calculate False and True Positive Rate (FPR and TPR)
auc <- performance(predrocr, measure = "auc")@y.values
auc



