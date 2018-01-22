############################Handwritten Digit Recognition using SVM##################################
#############################SUBMITTED BY Kiran T DDA1710335#########################################
#####################################################################################################

#Problem approach::

# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building : Using SVM Classification for high accuracy of class prediction
#    4.1 Check Linear kernel
#    4.2 Check RBF Kernel
# 5. Hyperparameter tuning and cross validation for the Kernel which has high Accuracy

######################################################################################################

#Business Understanding
#Specific scenario in Pattern Recognition: Handwritten Digit Recognition
#To develop a model which can correctly idenify the digit between 0-9 written in an image

######################################################################################################

#2. Data Understanding
#http://yann.lecun.com/exdb/mnist/
#Training set of 60,000 samples and a Test set of 10,000 samples 
#What we have in the data set:
#These are the pixel data of images that are centered in a 28x28 image computed by calculating the center of mass of the pixels, 
#and translated the image so as to position this point at the center of the 28x28 field
#Each pixel value ranges between 0 to 255, where based on the different values represnt the color gradient in grey scale
#Hence when each of these rows are printed appropriately would represent a hand written digit in 2d plane
#The training data set has a good combination of varied varities of numbers from differnt writers hence making the data set good for training the model
#This is a multi classification problem with total 10 classes, various classes belong to each digit category 0-9

######################################################################################################

#Loading neccessary libraries
library(kernlab)
library(caret)
library(e1071)
library(xgboost)
library(Metrics)

######################################################################################################

#3. Data Preparation: 
#Loading Training Data
train_data <- read.csv("mnist_train.csv",header=FALSE)

#Loading Test Data
test_data <- read.csv("mnist_test.csv",header=FALSE)

#Checking the dimensions of Train and Test data sets
dim(train_data)
dim(test_data)

#Checking for unique data records
#length(unique(train_data)) 
#length(unique(test_data))

#Looking at the dataset and it's Structure
View(train_data)
View(test_data)
str(train_data)
str(test_data)

#Observing the data distribution
summary(train_data)

#checking for missing values
sum(is.na(train_data))
sum(is.na(test_data))
#Since both the sums are zero there are no missing values in the datasets

#Making our target class/feature to factor variable
train_data[,1] <- factor(train_data[,1])
test_data[,1] <- factor(test_data[,1])

str(train_data)
str(test_data)

##############################################################################################
#Exploratory Data Analysis

#Univariate Analysis
plot(train_data$V1)
#No. of Records for each digit are equally balanced in the dataset 

##############################################################################################
#Feature Scaling
#Scaling the entire Train and Test data sets before executing the model
#Dependent variable is not scaled
train_data[-1] = scale(train_data[-1],scale=F)
test_data[-1] = scale(test_data[-1],scale=F)

#Since the no. of columns are very high 785, due to hardware limitations and to acheive the optimum results
#Proceeding to apply Dimensionality Reduction Technique to reduce the no. of columns which equallay represents the entire data
#Choosing and applying PCA as the Dimensionality Reduction technique in this case
#PCA is applied to reduce the no. of independent variables 
pca = prcomp(train_data[-1],scale=FALSE)
head(pca)
#Applying PCA on Test data aswell
pca_test = prcomp(test_data[-1],scale=FALSE)
head(pca_test)


###############################################################################################
# 4. Model Building

#Reducing the no. of independent variable components using PCA to 1 column
nComp <- 1
dfComponents <- predict(pca,newdata = train_data[-1])[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),cluster=train_data[1])
head(dfEvaluate)
train_data2 <- dfEvaluate

#Performing the same on test data
dfComponents_test <- predict(pca,newdata = test_data[-1])[,1:nComp]
dfEvaluate_test <- cbind(as.data.frame(dfComponents_test),cluster=test_data[1])
head(dfEvaluate_test)
test_data2 <- dfEvaluate_test
#Keeping the identical column names both in Train and Test Data
colnames(test_data2) <- c("dfComponents","V1")

#Train Data Sampling
#Since the training data is too large to build a model
#Due to limted computational power proceeding with sample of train data
set.seed(40)
train_data_indices <- sample(1:nrow(train_data2),0.15*nrow(train_data2))
sample_train_data <- train_data2[train_data_indices,]

#Checking the distribution of sample_train_data
table(sample_train_data$V1)
#We see that all data for different classes are available and no class is left out in this sample
#0    1    2    3    4    5    6    7    8    9 
#830 1011  887  967  874  842  894  966  849  880

#Model Building with 1 PCA component

#Building a Simple SVM Linear Kernel vanilladot model
linear_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "vanilladot")
linear_eval <- predict(linear_model, test_data2)

#confusion matrix - Linear Kernel
confusionMatrix(linear_eval,test_data2$V1)

#Building a RBF Kernel
RBF_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "rbfdot")
RBF_eval<- predict(RBF_model, test_data2)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_eval,test_data2$V1)

##################################################################################################
#Since the accuracy was pretty low trying with higher no. of PCA components
#Model Building with 5 PCA component

nComp <- 5
dfComponents <- predict(pca,newdata = train_data[-1])[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),cluster=train_data[1])
head(dfEvaluate)
train_data2 <- dfEvaluate

#Performing the same on test data
dfComponents_test <- predict(pca,newdata = test_data[-1])[,1:nComp]
dfEvaluate_test <- cbind(as.data.frame(dfComponents_test),cluster=test_data[1])
head(dfEvaluate_test)
test_data2 <- dfEvaluate_test
colnames(test_data2) <- colnames(train_data2)

#Since the training data is too large to build a model, due to limted computational power proceeding with sample of train data
set.seed(40)
train_data_indices <- sample(1:nrow(train_data2),0.15*nrow(train_data2))
sample_train_data <- train_data2[train_data_indices,]

#Checking the distribution of sample_train_data
table(sample_train_data$V1)

#Implementing Simple Linear Kernel vanilladot
linear_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "vanilladot")
linear_eval <- predict(linear_model, test_data2)

#confusion matrix - Linear Kernel
confusionMatrix(linear_eval,test_data2$V1)

#Results with 5 PCA components
#Overall Statistics

#Accuracy : 0.7046          
#95% CI : (0.6955, 0.7135)
#No Information Rate : 0.1135          
##Kappa : 0.6716  

#Using RBF Kernel
RBF_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "rbfdot")
RBF_eval<- predict(RBF_model, test_data2)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_eval,test_data2$V1)

#Results with 5 PCA components
#Overall Statistics

#Accuracy : 0.7611          
#95% CI : (0.7526, 0.7694)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.7345  

##################################################################################################
#Since the accuracy is still low, checking with higher no. of components
#Model Building with 10 PCA component

nComp <- 10
dfComponents <- predict(pca,newdata = train_data[-1])[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),cluster=train_data[1])
head(dfEvaluate)
train_data2 <- dfEvaluate

#Performing the same on test data
dfComponents_test <- predict(pca,newdata = test_data[-1])[,1:nComp]
dfEvaluate_test <- cbind(as.data.frame(dfComponents_test),cluster=test_data[1])
head(dfEvaluate_test)
test_data2 <- dfEvaluate_test
colnames(test_data2) <- colnames(train_data2)

#Since the training data is too large to build a model, due to limted computational power proceeding with sample of train data
set.seed(40)
train_data_indices <- sample(1:nrow(train_data2),0.15*nrow(train_data2))
sample_train_data <- train_data2[train_data_indices,]

#Checking the distribution of sample_train_data
table(sample_train_data$V1)

#Implementing Simple Linear Kernel vanilladot
linear_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "vanilladot")
linear_eval <- predict(linear_model, test_data2)

#confusion matrix - Linear Kernel
confusionMatrix(linear_eval,test_data2$V1)

#Results with 10 PCA components
#Overall Statistics

#Accuracy : 0.8419         
#95% CI : (0.8346, 0.849)
#No Information Rate : 0.1135         
#P-Value [Acc > NIR] : < 2.2e-16      
#Kappa : 0.8243

#Using RBF Kernel
RBF_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "rbfdot")
RBF_eval<- predict(RBF_model, test_data2)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_eval,test_data2$V1)

#Results with 10 PCA components
#Overall Statistics

#Accuracy : 0.9123          
#95% CI : (0.9066, 0.9178)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.9025 

##################################################################################################
#Checking the accuracy improvement with higher no. of PCA
#Model Building with 15 PCA component

nComp <- 15
dfComponents <- predict(pca,newdata = train_data[-1])[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),cluster=train_data[1])
head(dfEvaluate)
train_data2 <- dfEvaluate

#Performing the same on test data
dfComponents_test <- predict(pca,newdata = test_data[-1])[,1:nComp]
dfEvaluate_test <- cbind(as.data.frame(dfComponents_test),cluster=test_data[1])
head(dfEvaluate_test)
test_data2 <- dfEvaluate_test
colnames(test_data2) <- colnames(train_data2)

#pca = preProcess(x = train_data[-1],method = 'pca', thresh = 0.95,scale=FALSE)
#pca_train_data <- predict(pca,train_data2)

#Since the training data is too large to build a model, due to limted computational power proceeding with sample of train data
set.seed(40)
train_data_indices <- sample(1:nrow(train_data2),0.15*nrow(train_data2))
sample_train_data <- train_data2[train_data_indices,]

#Checking the distribution of sample_train_data
table(sample_train_data$V1)

#Implementing Simple Linear Kernel vanilladot
linear_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "vanilladot")
linear_eval <- predict(linear_model, test_data2)

#confusion matrix - Linear Kernel
confusionMatrix(linear_eval,test_data2$V1)

#Results with 15 PCA components
#Overall Statistics

#Accuracy : 0.8806          
#95% CI : (0.8741, 0.8869)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.8673

#Using RBF Kernel
RBF_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "rbfdot")
RBF_eval<- predict(RBF_model, test_data2)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_eval,test_data2$V1)

#Results with 15 PCA components
#Overall Statistics

#Accuracy : 0.9427         
#95% CI : (0.938, 0.9472)
#No Information Rate : 0.1135         
#P-Value [Acc > NIR] : < 2.2e-16      
#Kappa : 0.9363

##################################################################################################
#Checking further if there is scope of accuracy improvement with higher no. of PCA
#Model Building with 20 PCA component

nComp <- 20
dfComponents <- predict(pca,newdata = train_data[-1])[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),cluster=train_data[1])
head(dfEvaluate)
train_data2 <- dfEvaluate

#Performing the same on test data
dfComponents_test <- predict(pca,newdata = test_data[-1])[,1:nComp]
dfEvaluate_test <- cbind(as.data.frame(dfComponents_test),cluster=test_data[1])
head(dfEvaluate_test)
test_data2 <- dfEvaluate_test
colnames(test_data2) <- colnames(train_data2)

#Since the training data is too large to build a model, due to limted computational power proceeding with sample of train data
set.seed(40)
train_data_indices <- sample(1:nrow(train_data2),0.15*nrow(train_data2))
sample_train_data <- train_data2[train_data_indices,]

#Checking the distribution of sample_train_data
table(sample_train_data$V1)

#Implementing Simple Linear Kernel vanilladot
linear_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "vanilladot")
linear_eval <- predict(linear_model, test_data2)

#confusion matrix - Linear Kernel
confusionMatrix(linear_eval,test_data2$V1)

#Using RBF Kernel
RBF_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "rbfdot")
RBF_eval<- predict(RBF_model, test_data2)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_eval,test_data2$V1)


#Model Accuracy: 0.9128
#Statistics by Class
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9755   0.9859   0.9012   0.8881   0.9379   0.8576   0.9447   0.9125   0.8419   0.8692
#Specificity            0.9939   0.9918   0.9872   0.9823   0.9897   0.9908   0.9940   0.9907   0.9929   0.9898

#Using RBF Kernel
RBF_model <- ksvm(sample_train_data$V1~ ., data = sample_train_data, scale = FALSE, kernel = "rbfdot")
RBF_eval<- predict(RBF_model, test_data)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_eval,test_data$V1)

#Accuracy : 0.9581
#Statistics by Class:
  
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9888   0.9921   0.9554   0.9673   0.9623   0.9439   0.9718   0.9358   0.9343   0.9247
#Specificity            0.9963   0.9973   0.9948   0.9932   0.9939   0.9956   0.9959   0.9955   0.9966   0.9943


#Further increasing the number of PCA components doesn't increase the accuracy to high level,
#Hence finalizing the no. of PCA components with 20 

####################################################################################
####################################################################################
#Definitely the accuracy and predicting power is more in case of using RBF Kernel
#Hence choosing RBF model which predicts the classes with better accuracy than linear model
#####################################################################################
#####################################################################################

#5. Performing Hyperparameter tuning and Cross Validation on RBF model to choose the best C and Sigma value

# Performing Cross Validation with K=5 fold 
trainControl <- trainControl(method="cv", number=5)

# Assigning Evaluation metric to be Accuracy.
metric <- "Accuracy"

#Choosing the set of hyperparameters to be trained while 5 fold modeling
set.seed(10)
grid <- expand.grid(.sigma=seq(0.05, 0.1, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm <- train(V1~., data=sample_train_data, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
plot(fit.svm)

#Result of Cross Validation
#Support Vector Machines with Radial Basis Function Kernel 

#9000 samples
#20 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 7199, 7200, 7202, 7199, 7200 
#Resampling results across tuning parameters:
  
#  sigma  C  Accuracy   Kappa    
#0.05   1  0.9568889  0.9520784
#0.05   2  0.9604437  0.9560290
#0.05   3  0.9594438  0.9549173
#0.05   4  0.9598886  0.9554117
#0.05   5  0.9598889  0.9554120
#0.06   1  0.9592218  0.9546713
#0.06   2  0.9614442  0.9571414
#0.06   3  0.9606659  0.9562759
#0.06   4  0.9601105  0.9556589
#0.06   5  0.9599991  0.9555351
#0.07   1  0.9599993  0.9555352
#0.07   2  0.9624441  0.9582529
#0.07   3  0.9618880  0.9576349
#0.07   4  0.9612214  0.9568939
#0.07   5  0.9607770  0.9564001
#0.08   1  0.9612219  0.9568945
#0.08   2  0.9624441  0.9582526
#0.08   3  0.9622213  0.9580055
#0.08   4  0.9612214  0.9568940
#0.08   5  0.9607768  0.9563992
#0.09   1  0.9619993  0.9577583
#0.09   2  0.9628888  0.9587474
#0.09   3  0.9628881  0.9587468
#0.09   4  0.9615545  0.9572639
#0.09   5  0.9609990  0.9566460
#0.10   1  0.9612219  0.9568944
#0.10   2  0.9627772  0.9586237
#0.10   3  0.9624439  0.9582532
#0.10   4  0.9619992  0.9577582
#0.10   5  0.9615548  0.9572641

#Accuracy was used to select the optimal model using  the largest value
#The final values used for the model were sigma = 0.09 and C = 2
                                        ###########################

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1    2    3    4    5    6    7    8    9
#0  968    0    7    0    1    4    8    1    0    3
#1    0 1129    2    1    0    0    3    6    0    6
#2    2    2  994    5    3    3    0   20    2    0
#3    0    1    5  976    0   17    0    0   16    7
#4    0    0    5    0  942    2    2    5    6   18
#5    5    1    1   15    0  848    3    0    8    8
#6    2    0    1    0    3    7  937    1    5    1
#7    1    0   12    6    2    2    1  973    4   10
#8    2    1    4    5    1    7    4    4  926   12
#9    0    1    1    2   30    2    0   18    7  944

#Overall Statistics

#Accuracy : 0.9637          
#95% CI : (0.9598, 0.9673)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.9597          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:

#Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9878   0.9947   0.9632   0.9663   0.9593   0.9507   0.9781   0.9465   0.9507   0.9356
#Specificity            0.9973   0.9980   0.9959   0.9949   0.9958   0.9955   0.9978   0.9958   0.9956   0.9932
#Pos Pred Value         0.9758   0.9843   0.9641   0.9550   0.9612   0.9539   0.9791   0.9624   0.9586   0.9393
#Neg Pred Value         0.9987   0.9993   0.9958   0.9962   0.9956   0.9952   0.9977   0.9939   0.9947   0.9928
#Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
#Detection Rate         0.0968   0.1129   0.0994   0.0976   0.0942   0.0848   0.0937   0.0973   0.0926   0.0944
#Detection Prevalence   0.0992   0.1147   0.1031   0.1022   0.0980   0.0889   0.0957   0.1011   0.0966   0.1005
#Balanced Accuracy      0.9925   0.9963   0.9795   0.9806   0.9775   0.9731   0.9879   0.9711   0.9731   0.9644

######################################################################
#Based on the Tuned Hyperparameters obtained C=2 ,sigma =0.09
# Checking overfitting of Non-Linear - SVM on the Test data set
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm, test_data2[-21])
confusionMatrix(evaluate_non_linear, test_data2$V1)

#Confusion Matrix and Statistics

#Reference
#Prediction    0    1    2    3    4    5    6    7    8    9
#0  968    0    7    0    1    4    8    1    0    3
#1    0 1129    2    1    0    0    3    6    0    6
#2    2    2  994    5    3    3    0   20    2    0
#3    0    1    5  976    0   17    0    0   16    7
#4    0    0    5    0  942    2    2    5    6   18
#5    5    1    1   15    0  848    3    0    8    8
#6    2    0    1    0    3    7  937    1    5    1
#7    1    0   12    6    2    2    1  973    4   10
#8    2    1    4    5    1    7    4    4  926   12
#9    0    1    1    2   30    2    0   18    7  944

#Overall Statistics

#Accuracy : 0.9637          
#95% CI : (0.9598, 0.9673)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.9597          
#Mcnemar's Test P-Value : NA              

#Statistics by Class:
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9878   0.9947   0.9632   0.9663   0.9593   0.9507   0.9781   0.9465   0.9507   0.9356
#Specificity            0.9973   0.9980   0.9959   0.9949   0.9958   0.9955   0.9978   0.9958   0.9956   0.9932
#Pos Pred Value         0.9758   0.9843   0.9641   0.9550   0.9612   0.9539   0.9791   0.9624   0.9586   0.9393
#Neg Pred Value         0.9987   0.9993   0.9958   0.9962   0.9956   0.9952   0.9977   0.9939   0.9947   0.9928
#Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
#Detection Rate         0.0968   0.1129   0.0994   0.0976   0.0942   0.0848   0.0937   0.0973   0.0926   0.0944
#Detection Prevalence   0.0992   0.1147   0.1031   0.1022   0.0980   0.0889   0.0957   0.1011   0.0966   0.1005
#Balanced Accuracy      0.9925   0.9963   0.9795   0.9806   0.9775   0.9731   0.9879   0.9711   0.9731   0.9644

######################################################################
#Conclusion
#Classification Technique : SVM -RBF
#Final Accuracy of the model : 96.37%
#Hyperparameters C= 2, sigma = 0.09
######################################################################



