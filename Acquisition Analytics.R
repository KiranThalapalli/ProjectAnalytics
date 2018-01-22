############################ Submitted by KIRAN T DDA1710335 ##############################
#####################------------Bank Marketing Analysis-------------######################
#-----------------------------------------------------------------------------------------#
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
##Loading required libraries
library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(dplyr)

###########################################################
# 1. Business Understanding:- Prospect Profiling, to predict the probability of response 
# from each prospect and target the ones most likely to respond to the next telemarketing campaign

# Also to achieve 80% of total responders at the minimum possible cost. The total number of responders 
# is the total number of prospects who responded, from the available data of about 41188 data points

###########################################################
#CHECKPOINT 1
###########################################################
## 2. Data understanding

# Loading bank marketing data in the working directory. 
bank_data<- read.csv("bank_marketing.csv")
View(bank_data)

# Checking structure of dataset 
str(bank_data)

# Summary of dataset
summary(bank_data)

# Checking response rate of prospect customer
response <- 4640/(36548+4640)
response

# Checking missing values
sum(is.na(bank_data))

############################################################

# 3. EDA and Data Preparation

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))

# Box plot 
boxplot(bank_data$age)

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)
View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset
str(bank_data)

#-----Next Variable is "job"
# Checking the levels of the job
levels(bank_data$job)


# Plotting bar graph for job variable.
# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}
plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 
str(bank_data)

# Checking Marital status
summary(bank_data$marital)

# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"

# Plotting marital status
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot
plot_response(bank_data$education,"Education_levels")

#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)
plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 
summary(bank_data$housing)
plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"
summary(bank_data$loan)
plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 
summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable
# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 
summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)
bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset
# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))

# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)
quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type
bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary
summary(bank_data$pdays)
levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")

# Number of prospects under each category
table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor
bank_data$previous <- as.factor(bank_data$previous)
levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')
summary(bank_data$poutcome)
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

###########################################################################
########################## 4. Modelling ###################################  
##----------------------Logistic Regression------------------------------##
#-------------------------------------------------------------------------#    

# Removing binning variables 
bank_data <- bank_data[, -21]

# Adding unique ProspectID to each row
bank_data$prospect_id <- seq.int(nrow(bank_data))

#creating dummy variables before model building
bank_data$response <- as.integer(bank_data$response)
k1 <- bank_data
bank_data <- dummy.data.frame(bank_data)
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#Removing duration variable from the bank_data data frame as
# Duration variable will not available since the call wouldn't have been made with the prospect
bank_data <- bank_data[,-45]

#---------------------------------------------------------    

# splitting into train and test data

set.seed(75)
split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)
train1 <- bank_data[split_indices, ]
train <- train1[,-61]
test1 <- bank_data[!split_indices, ]
test <- test1[,-61]
nrow(train)/nrow(bank_data)
nrow(test)/nrow(bank_data)

#---------------------------------------------------------    

# Logistic Regression
logistic_1 <- glm(response ~ ., family = "binomial", data = train)
summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 
#logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = response ~ age + jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + monthoct + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + previousLess_than_3_times + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    euribor3m, family = "binomial", data = train)

# checking vif for logistic_2 
vif(logistic_2)
summary(logistic_2)

# removing "`previousMore than_3_times`" since vif is high and the values do not interpret any meaning 
logistic_3 <- glm(formula = response ~ age + jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + monthoct + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    euribor3m, family = "binomial", data = train)

summary(logistic_3)
vif(logistic_3)

# Removing " cons.conf.idx" variable.
logistic_4 <- glm(formula = response ~ age + jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + monthoct + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx +
                    euribor3m, family = "binomial", data = train)


summary(logistic_4)
vif(logistic_4)

# Removing "euribor3m"
logistic_5 <- glm(formula = response ~ age + jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov + monthoct + day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx, family = "binomial", data = train)

summary(logistic_5)
vif(logistic_5)

# Removing "monthoct +"
logistic_6 <- glm(formula = response ~ age + jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov +  day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx, family = "binomial", data = train)

summary(logistic_6)
vif(logistic_6)


# Removing "age"
logistic_7 <- glm(formula = response ~ jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov +  day_of_weekfri + 
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx, family = "binomial", data = train)

summary(logistic_7)
vif(logistic_7)

# Removing " day_of_weekfri"
logistic_8 <- glm(formula = response ~ jobretired + jobstudent + educationPrimary_Education + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov +  
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx, family = "binomial", data = train)

summary(logistic_8)
vif(logistic_8)

# Removing "educationPrimary_Education"
logistic_9 <- glm(formula = response ~ jobretired + jobstudent + 
                    educationTertiary_Education + contactcellular + monthaug + 
                    monthjun + monthmar + monthmay + monthnov +  
                    day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days +
                    poutcomefailure + emp.var.rate + cons.price.idx, family = "binomial", data = train)

summary(logistic_9)
vif(logistic_9)

# Removing "jobstudent"
logistic_10 <- glm(formula = response ~ jobretired + 
                     educationTertiary_Education + contactcellular + monthaug + 
                     monthjun + monthmar + monthmay + monthnov +  
                     day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days +
                     poutcomefailure + emp.var.rate + cons.price.idx, family = "binomial", data = train)


summary(logistic_10)
vif(logistic_10)

logistic_final <- logistic_10
####################################################################################################
#CHECKPOINT 2.1 These are the significant variables as mentioned in logistic_final or logistic_10
####################################################################################################
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test[, -60], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression
# Let's use the probability cutoff of 50%.
predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.25)]
cutoff

##############################################################################
#CHECKPOINT 2.3: To find the optimal cutoff
##############################################################################

# Let's choose a cutoff value of 8.5% for final model
predicted_response <- factor(ifelse(predictions_logit >= 0.085, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

#Following was acheieved using logistic_10
#Accuracy 
#0.7688572 
#> sens
#Sensitivity 
#0.7025862 
#> spec
#Specificity 
#0.7772711 

############################################################   
# -------------------Model Evaluation-----------------------

# Appending the probabilities and response variables to the test data

test$predicted_probs <- predictions_logit
test$predicted_response <- predicted_response

# Creating new dataframe "test_predictions"
test_predictions <- test[, c("response", "predicted_probs", "predicted_response")]
summary(test_predictions$response)
summary(test_predictions$predicted_response)
response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])
response_rate

#########################################################################################
#CHECKPOINT 2.2 :Ordering based on the decreasing probability of response on test data
########################################################################################
# sorting the probabilities in decreasing order 
test_predictions <- test_predictions[order(test_predictions$predicted_probs, decreasing = T), ]

summary(test_predictions$response[1:6800])
summary(test_predictions$predicted_response[1:6800])

# plotting the lift chart

#Function to plot the Lift chart
lift <- function(labels , predicted_prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

############################################################################################
#CHECKPOINT 2.3: Model Evalution Metrics
############################################################################################

# Create a Table of cumulative gain and lift 
test_predictions$response <- as.factor(ifelse(test_predictions$response=="yes",1,0))
LG = lift(test_predictions$response, test_predictions$predicted_probs, groups = 10)
View(LG)

#Lift and Gain Charts
# Gain Chart 
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")

#############################################################################################





#############################################################################################
#CHECKPOINT 3
#############################################################################################

# Applying the model on the entire data set of 41188 observations
# To calculate the prediction probabilities using the generated Logistic Regression Model

bank_data_temp <- bank_data[,-c(60,61)]
#Predicting using logistic_final model
probability_predictions_logit_temp <- predict(logistic_final, newdata = bank_data_temp, type = "response")
summary(probability_predictions_logit_temp)

predicted_response_temp <- factor(ifelse(probability_predictions_logit_temp >= 0.085, 1,0))
summary(predicted_response_temp)
#No(0)   Yes(1) 
#29718 11470 
# Using this prediction model 11,470 prospects would respond to the campaign
# Now our goal is to identify the top 80% of these responders at minimum marketing cost

actual_response_temp <- factor(ifelse(bank_data$response == "yes",1,0))
summary(actual_response_temp)
# Response when randomly called
#no(0)   yes(1) 
#36548  4640 

#Calculating the cost per call
cost_of_call <- 0.033*k1$duration+0.8
#cost_of_call
sum(cost_of_call)
# 3,78,664.7 was the total cost incurred to procure 4640 customers when called randomly 

# Creating a new data frame with required fields
bank_data_df <- data.frame(cbind(bank_data$prospect_id,actual_response_temp,predicted_response_temp,probability_predictions_logit_temp,k1$duration,cost_of_call))
colnames(bank_data_df) <- c("prospect_id","actual_response","predicted_response","predicted_probability","duration","call_cost")

#Converting the responses to factor
bank_data_df$actual_response <- factor(ifelse(bank_data_df$actual_response == 2,1,0))
summary(bank_data_df$actual_response)
bank_data_df$predicted_response <- factor(ifelse(bank_data_df$predicted_response == 2,1,0))
summary(bank_data_df$predicted_response)

# sorting the probabilities in decreasing order 
bank_data_df <- bank_data_df[order(bank_data_df$predicted_probability, decreasing = T), ]

#####################################################################################################
#CHECKPOINT 5:
####################################################################################################

# Create a Table of cumulative gain and lift 
LG = lift(bank_data_df$predicted_response, bank_data_df$predicted_probability, groups = 10)

# Gain Chart 
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")
View(LG)

# NOTE: Top 80% prospects can be achieved from the first top 3 deciles however all the responders can also 
# be contacted within the top Y = 3 deciles

##########################################################################################################
#CHECKPOINT 4:
##########################################################################################################
#Scenario Subsetting the top 80% prospects into a data frame
#Since our model suggested 11,470 prospects would respond to the campaign
#The business objective is to target top 80% i.e X= 9176 prospects

target_df <- bank_data_df[1:9176,]
#View(target_df)
sum(target_df$call_cost)
#Rs. 92,192.02 would be the total cost if reached out to top 80% of the responders as suggested by the model
#92192.02/378664.7 = 0.24
#This would mean we could save 76% of marketing cost by reaching out to top 80% prospects when compared to reaching
# out to all the prospects randomply
summary(target_df$duration)
mean(target_df$duration)
# Average call duration in this case is 280.2146 sec
# Average total cost by using average duration = 9176 *(0.033 * 280.214 + 0.8) = Rs. 92191.84

###########################################################################################################
#Scenario when reached out to top 100% responding prospects as suggested by model
target_df2 <- bank_data_df[1:11470,]
#View(target_df2)
sum(target_df2$call_cost)
#Rs. 113116.4 would be the total cost if reached out all the top responders as suggested by the model
summary(target_df2$duration)
mean(target_df2$duration)
#Average duration 274.604
# Now if we directly contact all the 100% responding prospects approximate call cost would have been Rs. 1,13,116.4
# Which is far less than our inital cost (Rs. 3,78,664.7)
# By implementing this model we could save 378664.7 - 113116.4 = Rs. 265548.3
#265548.3/378664.7 = 0.70
#Hence by this model we could achieve 70% reduction in the marketing cost to target the top prospects
###########################################################################################################