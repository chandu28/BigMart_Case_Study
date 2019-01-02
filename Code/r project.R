# working directory
setwd('C:\\Users\\Chandu\\Desktop\\IMAR DATA')
getwd()

#importing training and testing data
train_dataset <-read.csv('Train_UWu5bXk.csv',header= TRUE, sep=',')
test_dataset <- read.csv('Test_u94Q5KV.csv',header= TRUE, sep = ',')

#summary of data
summary(train_dataset)

#checking sknewness
#install.packages('psych')

#invoke package for skewness
library(psych)
describe(train_dataset)

# to find outliers either formula pr Hmsic
library(Hmisc)
describe(train_dataset)

#boxplot
#boxplot(train_dataset$Item_Weight)

#missing value train
sum(is.na(train_dataset$Item_Weight))
train_dataset$Item_Weight <- ifelse(is.na(train_dataset$Item_Weight),
                                    median(train_dataset$Item_Weight,na.rm = TRUE),
                                    train_dataset$Item_Weight)
sum(is.na(train_dataset$Item_Weight))

#missing value test
sum(is.na(test_dataset$Item_Weight))
test_dataset$Item_Weight <- ifelse(is.na(test_dataset$Item_Weight),
                                    median(test_dataset$Item_Weight,na.rm = TRUE),
                                    test_dataset$Item_Weight)

sum(is.na(test_dataset$Item_Weight))

#outlet size train

table(train_dataset$Outlet_Size)
class(train_dataset$Outlet_Size)
train_dataset$Outlet_Size <- as.character(train_dataset$Outlet_Size)
train_dataset$Outlet_Size[train_dataset$Outlet_Size=='']<- "Medium"
train_dataset$Outlet_Size <- as.factor(train_dataset$Outlet_Size)

#outlet size test

table(test_dataset$Outlet_Size)
class(test_dataset$Outlet_Size)
test_dataset$Outlet_Size <- as.character(test_dataset$Outlet_Size)
test_dataset$Outlet_Size[test_dataset$Outlet_Size=='']<- "Medium"
test_dataset$Outlet_Size <- as.factor(test_dataset$Outlet_Size)

#missing charcter empty train
#inconsistency data
table(train_dataset$Item_Fat_Content) #table calling 
train_dataset$Item_Fat_Content <- as.character(train_dataset$Item_Fat_Content)
train_dataset$Item_Fat_Content[train_dataset$Item_Fat_Content == 'LF'] <- 'Low Fat'
train_dataset$Item_Fat_Content[train_dataset$Item_Fat_Content == 'low fat'] <- 'Low Fat'
train_dataset$Item_Fat_Content[train_dataset$Item_Fat_Content == 'reg'] <- 'Regular'
train_dataset$Item_Fat_Content <- as.factor(train_dataset$Item_Fat_Content)

#missing charcter empty test
#inconsistency data
table(train_dataset$Item_Fat_Content) #table calling 
test_dataset$Item_Fat_Content <- as.character(test_dataset$Item_Fat_Content)
test_dataset$Item_Fat_Content[test_dataset$Item_Fat_Content == 'LF'] <- 'Low Fat'
test_dataset$Item_Fat_Content[test_dataset$Item_Fat_Content == 'low fat'] <- 'Low Fat'
test_dataset$Item_Fat_Content[test_dataset$Item_Fat_Content == 'reg'] <- 'Regular'
test_dataset$Item_Fat_Content <- as.factor(test_dataset$Item_Fat_Content)


#creating  a new variable for year of business(creating value for algorthim)
train_dataset$yob <- 2018- train_dataset$Outlet_Establishment_Year
 boxplot(train_dataset$yob)

 #new variable for test 
 test_dataset$yob <- 2018- test_dataset$Outlet_Establishment_Year

 #applying log 
 train_dataset$log_Weight <-log(train_dataset$Item_Weight)
 train_dataset$log_mrp <- log(train_dataset$Item_MRP)
 train_dataset$log_yob <- log(train_dataset$yob)
 #applying log test
 test_dataset$log_Weight <-log(test_dataset$Item_Weight)
 test_dataset$log_mrp <- log(test_dataset$Item_MRP)
 test_dataset$log_yob <- log(test_dataset$yob)

 #applying inverse train
 train_dataset$inverse_weight <- 1/train_dataset$Item_Weight
 train_dataset$inverse_mrp <- 1/train_dataset$Item_MRP
 train_dataset$inverse_yob <- 1/train_dataset$yob 
 
 #applying inverse test
 test_dataset$inverse_weight <- 1/test_dataset$Item_Weight
 test_dataset$inverse_mrp <- 1/test_dataset$Item_MRP
 test_dataset$inverse_yob <- 1/test_dataset$yob 
 
 #square of continous train
 train_dataset$sq_weight <- (train_dataset$Item_Weight)^2
 train_dataset$sq_mrp <-  (train_dataset$Item_MRP)^2
 train_dataset$sq_yob <- (train_dataset$yob)^2 
 
 #square of continous test
 test_dataset$sq_weight <- (test_dataset$Item_Weight)^2
 test_dataset$sq_mrp <-  (test_dataset$Item_MRP)^2
 test_dataset$sq_yob <- (test_dataset$yob)^2 
 
 #visibility
 median(train_dataset$Item_Visibility)
 summary(train_dataset$Item_Visibility)
 train_dataset$Item_Visibility[train_dataset$Item_Visibility=='0']<- 0.05393093
 #test visibility
 median(test_dataset$Item_Visibility)
 summary(test_dataset$Item_Visibility)
 test_dataset$Item_Visibility[test_dataset$Item_Visibility=='0']<- 0.05415425
 

 #correlation
 
 #square root of continous train
 train_dataset$sqrt_weight <- sqrt(train_dataset$Item_Weight)
 train_dataset$sqrt_mrp <-  sqrt(train_dataset$Item_MRP)
 train_dataset$sqrt_yob <- sqrt(train_dataset$yob)
 
 #square of continous test
 test_dataset$sqrt_weight <- sqrt(test_dataset$Item_Weight)
 test_dataset$sqrt_mrp <-  sqrt(test_dataset$Item_MRP)
 test_dataset$sqrt_yob <- sqrt(test_dataset$yob)
 
 cor(train_dataset$Item_Weight,train_dataset$Item_Visibility)
 cor(train_dataset$Outlet_Size,train_dataset$Outlet_Type) # only munerical
cor(train_dataset$Item_Outlet_Sales,train_dataset$Item_MRP)
 
#model creation linearmodel (dependent(continous), independent(continous)) ~ because = is already used in equating in backend r
 names(train_dataset)
 model <- lm(Item_Outlet_Sales ~ Item_Weight +
                                Item_Visibility +
                                   Item_MRP +
                                yob, data= train_dataset)
 
 # check for sum(residual)
 #norminal distribution (box plot for residuals)
summary(model)


# finding predicted values for the model
train_dataset$predictedoutput <- predict(model,train_dataset)
#residual
train_dataset$residual <- train_dataset$Item_Outlet_Sales - train_dataset$predictedoutput

model$residuals # for residuals
train_dataset$residual2 <- model$residuals
rsm_model1 <- sqrt(mean((train_dataset$residual)^2))
rsm_model1

#model2 both categorical and numerical
names(train_dataset)
model2 <-lm(Item_Outlet_Sales ~ Item_Weight+
                                Item_Fat_Content +
                                Item_Visibility +Item_Type+Item_MRP +
                                 Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                yob, data=train_dataset)
summary(model2)
#predicted
train_dataset$predictedoutput2 <- predict(model2,train_dataset)
#residual
train_dataset$residual_model2 <- train_dataset$Item_Outlet_Sales - train_dataset$predictedoutput2

#rmse
#rmse_model2<- sqrt(mean((train_dataset$Item_Outlet_Sales- train_dataset$predictedoutput2)^2))
rmse_model2 <- sqrt(mean((train_dataset$residual_model2)^2))
rmse_model2


#model3 for log of continous
names(train_dataset)
model3 <-lm(Item_Outlet_Sales ~ 
                                Item_Fat_Content +
                                Item_Type+
                                Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                 log_Weight + log_mrp +log_yob,
                                 data=train_dataset)
summary(model3)
train_dataset$predictedoutput3 <- predict(model3,train_dataset)
#residual
train_dataset$residual_model3 <- train_dataset$Item_Outlet_Sales - train_dataset$predictedoutput3
rmse_model3<- sqrt(mean((train_dataset$residual_model3)^2))
rmse_model3

#create model4 for inverse and model5 for sqayre try inverse square at home 

#model
model_4 <- lm((Item_Outlet_Sales ~ 
                 Item_Fat_Content +
                 Item_Type+
                 Outlet_Size +Outlet_Location_Type+Outlet_Type+
                 inverse_weight + inverse_mrp + inverse_yob),
                 data=train_dataset)
summary(model_4)
train_dataset$predictedoutput_model4 <- predict(model_4,train_dataset)
model_4$residuals
train_dataset$residual_model4 <- model_4$residuals
rms_model4 <- sqrt(mean((train_dataset$residual_model4)^2))
rms_model4



model_5 <- lm(Item_Outlet_Sales ~ 
                Item_Fat_Content +
                Item_Type+
                Outlet_Size +Outlet_Location_Type+Outlet_Type+
                sq_weight + sq_mrp +sq_yob,
                data=train_dataset)
summary(model_5)
train_dataset$predictedoutput_model5 <- predict(model_5,train_dataset)
model_5$residuals 
train_dataset$residual_model5 <- model_5$residuals
rsm_model5 <- sqrt(mean((train_dataset$residual_model5)^2))
rsm_model5

# sqaure with visibiliity
 train_dataset$sq_visibility <- (train_dataset$Item_Visibility)^2
 
 model_6 <- lm(Item_Outlet_Sales ~ 
                 Item_Fat_Content +
                 Item_Type+
                 Outlet_Size +Outlet_Location_Type+Outlet_Type+
                 sq_weight + sq_mrp +sq_yob + sq_visibility,
               data=train_dataset)
 summary(model_6)
 train_dataset$predictedoutput_model6 <- predict(model_6, train_dataset)
 model_6$residuals 
 train_dataset$residual_model6 <- model_6$residuals
 rsm_model6 <- sqrt(mean((train_dataset$residual_model6)^2))
 rsm_model6
 
 #decision tree
 library(party)
 png(file="log_sales_tree.png")
 set.seed(12)
 decision_tree_logvalues <- ctree(Item_Outlet_Sales ~ 
                          Item_Fat_Content +
                          Item_Type+Item_Visibility_transform
                          Outlet_Size +Outlet_Location_Type+Outlet_Type+
                          log_Weight + log_mrp +log_yob,
                        data=train_dataset)
 summary(decision_tree_logvalues)
  decision_tree_logvalues
  plot(decision_tree_logvalues)
  dev.off()
  str(train_dataset)
 train_dataset$predicted_log_sales_decision <- predict(decision_tree_logvalues,train_dataset,type='response')  
 train_dataset$decision_tree_log_residuals <- train_dataset$Item_Outlet_Sales- train_dataset$predicted_log_sales_decision
 rms_decison_tree <- sqrt(mean((train_dataset$decision_tree_log_residuals)^2))
 rms_decison_tree
 
 #decison tree normal with all variables
 library(party)
 png(file="sales_tree.png")
 set.seed(12)
 decision_tree <- ctree(Item_Outlet_Sales ~ 
                          Item_Fat_Content +
                          Item_Visibility+Item_Type+Item_MRP +
                          Outlet_Size +Outlet_Location_Type+Outlet_Type+
                          yob, data=train_dataset)
 summary(decision_tree)
 decision_tree
 plot(decision_tree)
 dev.off()
 str(train_dataset)
 train_dataset$predicted_sales_decision <- predict(decision_tree,train_dataset,type='response')  
 train_dataset$decision_tree_residuals <- train_dataset$Item_Outlet_Sales- train_dataset$predicted_sales_decision
 rms_decison_tree1 <- sqrt(mean((train_dataset$decision_tree_residuals)^2))
 rms_decison_tree1
 
 #sq tree
 library(party)
 png(file="log_sales_tree.png")
 set.seed(12)
 decision_tree_sq <- ctree(Item_Outlet_Sales ~ Item_Weight+
                        Item_Fat_Content +
                          Item_Visibility_transform +Item_Type+Item_MRP +
                          Outlet_Size +Outlet_Location_Type+Outlet_Type+
                          yob, data=train_dataset)
 summary(decision_tree_sq)
 decision_tree_sq
 plot(decision_tree_sq)
 dev.off()
 str(train_dataset)
 train_dataset$predicted_sales_decision_sq <- predict(decision_tree,train_dataset,type='response')  
 train_dataset$decision_tree_residuals_sq <- train_dataset$Item_Outlet_Sales- train_dataset$predicted_sales_decision
 rms_decison_tree_sq <- sqrt(mean((train_dataset$decision_tree_residuals)^2))
 rms_decison_tree_sq
 
 #inverse
 library(party)
 png(file="inverse_sales_tree.png")
 set.seed(12)
 decision_tree_inv<- ctree(Item_Outlet_Sales ~ 
                          Item_Fat_Content +
                          Item_Visibility +Item_Type+Item_MRP +
                          inverse_weight + inverse_mrp + inverse_yob
                          , data=train_dataset)
 summary(decision_tree_inv)
 decision_tree_inv
 plot(decision_tree_inv)
 dev.off()
 str(train_dataset)
 train_dataset$predicted_inv_sales_decision <- predict(decision_tree_inv,train_dataset,type='response')  
 train_dataset$decision_tree_inv_residuals <- train_dataset$Item_Outlet_Sales- train_dataset$predicted_inv_sales_decision
 rms_decison_tree_inv <- sqrt(mean((train_dataset$decision_tree_inv_residuals)^2))
 rms_decison_tree_inv
 
 #inverse
 library(party)
 png(file="sqrt_sales_tree.png")
 set.seed(12)
 decision_tree_sqrt<- ctree(Item_Outlet_Sales ~ 
                             Item_Fat_Content +
                             Item_Visibility +Item_Type+Item_MRP +
                             sqrt_weight + sqrt_mrp + sqrt_yob
                           , data=train_dataset)
 summary(decision_tree_sqrt)
 decision_tree_sqrt
 plot(decision_tree_sqrt)
 dev.off()
 str(train_dataset)
 train_dataset$predicted_sqrt_sales_decision <- predict(decision_tree_sqrt,train_dataset,type='response')  
 train_dataset$decision_tree_sqrt_residuals <- train_dataset$Item_Outlet_Sales- train_dataset$predicted_sqrt_sales_decision
 rms_decison_tree_sqrt <- sqrt(mean((train_dataset$decision_tree_sqrt_residuals)^2))
 rms_decison_tree_sqrt
 
 
 #random forest with continous and categorical
 library(randomForest)
 model_random_forest1 <- randomForest(Item_Outlet_Sales ~ Item_Weight+
                                       Item_Fat_Content +
                                       Item_Visibility+Item_Type+Item_MRP +
                                       Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                       yob,ntree=1500,data=train_dataset) 
 
train_dataset$random_op1<- predict(model_random_forest1,train_dataset,type='response') 
train_dataset$random_residuals1 <-train_dataset$Item_Outlet_Sales- train_dataset$random_op1
rms_random1 <- sqrt(mean((train_dataset$random_residuals1)^2))
rms_random1

#random forest with log
library(randomForest)
model_random_forest2 <- randomForest(Item_Outlet_Sales ~ Item_Weight+
                                       Item_Fat_Content +
                                       Item_Type+
                                       Item_Visibility_transform +
                                       Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                       log_Weight + log_mrp +log_yob,
                                       data=train_dataset) 

train_dataset$random_op2<- predict(model_random_forest2,train_dataset,type='response') 
train_dataset$random_residuals2 <-train_dataset$Item_Outlet_Sales- train_dataset$random_op2
rms_random2 <- sqrt(mean((train_dataset$random_residuals2)^2))
rms_random2

#square 
library(randomForest)
model_random_forest3 <- randomForest(Item_Outlet_Sales ~ Item_Weight +
                                       Item_Fat_Content +
                                       Item_Type+
                                       Item_Visibility_transform+
                                       Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                       sq_weight + sq_mrp +sq_yob,
                                     data=train_dataset) 

train_dataset$random_op3<- predict(model_random_forest3,train_dataset,type='response') 
train_dataset$random_residuals3 <-train_dataset$Item_Outlet_Sales- train_dataset$random_op3
rms_random3 <- sqrt(mean((train_dataset$random_residuals3)^2))
rms_random3

#inverse
library(randomForest)
model_random_forest4 <- randomForest(Item_Outlet_Sales ~ 
                                       Item_Fat_Content +
                                       Item_Type+
                                       Item_Visibility_transform+
                                       Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                       inverse_weight + inverse_mrp + inverse_yob,
                                       data=train_dataset) 

train_dataset$random_op4<- predict(model_random_forest4,train_dataset,type='response') 
train_dataset$random_residuals4 <-train_dataset$Item_Outlet_Sales- train_dataset$random_op4
rms_random4 <- sqrt(mean((train_dataset$random_residuals4)^2))
rms_random4

library(randomForest)
model_random_forest5 <- randomForest(Item_Outlet_Sales ~ 
                                       Item_Fat_Content +
                                       Item_Type+
                                       Item_Visibility_transform+
                                       Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                       sqrt_weight + sqrt_mrp + sqrt_yob,
                                       ntree=1000,data=train_dataset) 

train_dataset$random_op5<- predict(model_random_forest5,train_dataset,type='response') 
train_dataset$random_residuals5 <-train_dataset$Item_Outlet_Sales- train_dataset$random_op5
rms_random5 <- sqrt(mean((train_dataset$random_residuals5)^2))
rms_random5

#svm
library(e1071)
model_support_vector_radial <- svm(Item_Outlet_Sales ~ 
                                     Item_Fat_Content +
                                     Item_Type+
                                     Item_Visibility+
                                     Outlet_Size +Outlet_Location_Type+Outlet_Type+
                                     sq_weight + sq_mrp +sq_yob,
                                   data=train_dataset) 
model_support_vector_radial
train_dataset$predicted_svm <- predict(model_support_vector_radial,train_dataset)
train_dataset$svm_residuals <- train_dataset$Item_Outlet_Sales- train_dataset$predicted_svm
rsm_svm <- sqrt(mean((train_dataset$svm_residuals)^2))
rsm_svm
cm_svm <- table(train_dataset$Loan_Status, train_dataset$predicted_svm)
cm_svm



#test with random
test_dataset$Item_Outlet_Sales <- predict(model_random_forest1,test_dataset,type="response")
write.csv(test_dataset,"SampleSubmission.csv")

#test with decison tree
test_dataset$Item_Outlet_Sales <- predict(decision_tree_logvalues,test_dataset,type="response")
write.csv(test_dataset,"SampleSubmission.csv")

#test with svm
test_dataset$Item_Outlet_Sales 
