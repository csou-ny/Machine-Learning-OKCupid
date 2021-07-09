install.packages("dplyr")
library(dplyr)
Cupid <- read.csv("/Users/Sou/Desktop/OKCupid.csv", stringsAsFactors = TRUE)

####Also one section devoted to EDA
###
##########Cleanup and factor level reductions
Cupid2 <- subset(Cupid, income >0 & income <= 500000 ) #No income coded as "-1"
#Cupid2$status <- ifelse(Cupid2$status == "single", "Single", "Not Single")
#Cupid2$status <- as.factor(Cupid2$status)
Cupid2 %>% count(status) 
#returns 1 Not Single  983
#        2     Single 10000

#logistic regression
logistic <- glm(status~ income + age +height, data= Cupid2, family="binomial")
summary(logistic)
coef(logistic)
log.pred = predict(logistic, data=Cupid2, type="response")
dim(Cupid2)
#[1] 10983    22
pred.log <- rep("Single", 10983)
pred.log[log.pred >0.5] = "Not Single"
contrasts(Cupid2$status)
#######
#Single
#Not Single      0
#Single          1
table(pred.log, Cupid2$status)
####
# pred.log     Not Single Single
# Not Single        983  10000
#### Doesnt tell us much--- which is why we need to 
#logistic regression with training and testing:
#Because of unbalanced ratio--- consider scaling the data so that all obs with "Not Single"
#will all appear along with some sampling of the obs with "Single"
#First reduce to half number of obs
#set.seed(1)
#SingleSample <- subset(Cupid2, Cupid2$status=="Single") #10,000 entries
#SingleSample2 <- SingleSample[sample(nrow(SingleSample), 5000, replace=FALSE),]
#5,000 entries---half of original
#Now combine with the "Not Single" for a better ratio 
#NotSingleSample <- subset(Cupid2, Cupid2$status=="Not Single") #983 obs
#StatusSample <- rbind(SingleSample2, NotSingleSample ) #5983 obs total mixed of two factor levels
#Now create the train and test from this
#Status.train <- StatusSample[sample(nrow(StatusSample), nrow(StatusSample)/2, replace=FALSE),] #3135 obs
#Status.test <- StatusSample[!Status.train,]
train = (Cupid2$age <35)
#Status.train <- (StatusSample$age <35)
CupidUnder35 <- Cupid2[!train,] #[1] 3626   22 dimensions
StatusUnder35 = Cupid2$status[!train] 
contrasts(Cupid2$status)
#             Single
#Not Single      0
#Single          1
glm.fit.logistic <- glm(status ~ income + age +height, data= Cupid2,
                        family=binomial, subset=train)
glm.probs.logistic <- predict(glm.fit.logistic, CupidUnder35, type="response")
glm.pred= rep("Single", 3626) 
glm.pred[glm.probs.logistic >0.5] = "Not Single"

table(glm.pred, StatusUnder35) #confusion matrix----now it finally returns a proper confusion matrix
#StatusUnder35
#glm.pred     Not Single Single
#Not Single        262   3229
#Single              5    130
ratio <- (262+130)/(3229+5) # 0.1212121
#This ratio is pretty low- an indication that the model needs to be reconsidered or
#the selection bias needs to be taken care of

#### incorporate the time and number of languages spoken into the prediction
glm.fit.logistic2 <- glm(status ~ income + age +height + education, data= Cupid2,
                        family=binomial, subset=train)
glm.probs.logistic2 <- predict(glm.fit.logistic, CupidUnder35, type="response")
glm.pred2= rep("Single", 3626) 
glm.pred2[glm.probs.logistic2 >0.5] = "Not Single"

table(glm.pred2, StatusUnder35)

#Logistic2
train2 = (Cupid2$age <35)
#Status.train <- (StatusSample$age <35)
CupidUnder35 <- Cupid2[!train2,] #[1] 3626   22 dimensions
SexUnder35 = Cupid2$sex[!train2] 
glm.fit.logistic3 <- glm(sex ~ age + income+ height, data= Cupid2, family=binomial, subset=train2)
summary(glm.fit.logistic3)
glm.probs.logistic3 <- predict(glm.fit.logistic, CupidUnder35, type="response")
glm.pred3= rep("m", 3626) 
glm.pred3[glm.probs.logistic3 >0.5] = "f"
contrasts(Cupid2$sex)
table(glm.pred3, SexUnder35)

#LDA
library(MASS)
lda.fit <- lda(Cupid2$sex ~ age + income+ height,data= Cupid2)
summary(lda.fit)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit,CupidUnder35 )
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, SexUnder35)
SexUnder35
#lda.class    f    m
#f            687  151
#m            353 2435


#KNN
library(class)
train.X <- cbind(Cupid2$age, Cupid2$income, Cupid2$height)[train2,]
test.X <- cbind(Cupid2$age, Cupid2$income, Cupid2$height)[!train2,]
train.sex <- Cupid2$sex[train2]
set.seed(1)
knn.pred = knn(train.X, test.X, train.sex, k=1)
table(knn.pred, SexUnder35)

#Simple Classificaton Trees
library(tree)
High <- ifelse(Cupid2$income <= 150000, "No", "Yes")
Older <- ifelse(Cupid2$age <= 40, "No", "Yes")
White <- ifelse(Cupid2$ethnicity =="white", "No", "Yes")
Cupid3 <- data.frame(Cupid2, High, Older, White)
tree.Cupid <- tree(High~ education + job + offspring + sex +drugs+ body_type+ Older+ White+ diet + drinks+
                     orientation+ religion , 
                   data=Cupid3)
summary(tree.Cupid)
plot(tree.Cupid)
text(tree.Cupid, pretty=0)

set.seed(1)
train.tree <- sample(1:nrow(Cupid3), 10983/2)
test.tree <- Cupid3[-train.tree,]
High.test <- Cupid3$High[-train.tree]
tree.Cupid2<- tree(High~ education + job + offspring + sex +drugs+ body_type+ Older+ White+ diet + drinks+
                     orientation+ religion ,data=Cupid3, subset=train.tree)
tree.pred2 <- predict(tree.Cupid2, test.tree, type="class")
table(tree.pred2, High.test) #0.9823379 very high success rate
plot(tree.Cupid2)
text(tree.Cupid2, pretty=0)

###is pruning necessary?
cv.Cupid <- cv.tree(tree.Cupid2, FUN=prune.misclass)
prune.Cupid <- prune.tree(tree.Cupid2, best=6)
plot(prune.Cupid)
text(prune.Cupid, pretty=0)

#Bagging and Random Forest

library(randomForest)
set.seed(1)
bag.Cupid <- randomForest(High~ education + job + offspring + sex +drugs+ body_type+ Older+ White+ diet + drinks+
                            orientation+ religion, data=Cupid3, subset=train.tree, importance=TRUE)
bag.Cupid
#importance(bag.Cupid)
varImpPlot(bag.Cupid)

#Random Forest is a generalization of Bagging. Not used in this project since we don't use all the predictors 
#as some are not relevant. 

#Boosting
library(gbm)
set.seed(1)
High2 <- ifelse(Cupid3$High =="Yes", 0,1 )
Cupid4 <- data.frame(Cupid3, High2)
boost.Cupid <- gbm(High2 ~education + job + offspring + sex +drugs+ body_type+ Older+ White+ diet + drinks+
                     orientation+ religion, data= Cupid4[-train,], distribution = "bernoulli", n.trees = 5000, 
                   interaction.depth = 4)
summary(boost.Cupid)



