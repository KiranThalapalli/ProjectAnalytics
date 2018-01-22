#SUBMITTED BY VIKRAM SHREEDHAR, KIRAN T
############################ Retail - Gaint Sales Forecasting #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation 
# 4. Exploratory Analysis
# 5. Model Building 
#  5.1 Classical Decomposition
#  5.2 Auto Arima
# 6 Model Evaluation & Forecasting

############################################################################################

# 1. Business Understanding: 

#The objective is to forecast sales and demand for next 6 months for  top most
#profitable and consistent segment from current 21 segments

############################################################################################

# 2. Data Understanding: 

# The data currently has the transaction level data, where each row represents a particular 
# order made on the online store. There are 24 attributes related to each such transaction.     
# The "Market" attribute has 7-factor levels representing the geographical market sector that  
# the customer belongs to. The "Segment" attribute tells which of the 3 segments that customer 
# belongs to.  The orders have been placed between January 2011 to December 2014 & shipping
# of the orders have happened from January 2011 to January 2015
# Data needs to be subsetted to 21 segments and profitablity to be verified for consistency on
# basis of coefficient of variation.   

# Number of Instances: 51290 rows
# Number of Attributes: 24 

############################################################################################

# 3. Data Preparation: 


# Loading Neccessary libraries

library(dplyr)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(data.table)
library(forecast)
library(graphics)
library(tseries)
library(cowplot)

# Loading Data
retail <- read.csv("Global Superstore.csv",stringsAsFactors = F)

# Understanding Dimensions
dim(retail)

# Structure of the dataset
str(retail)

# printing first few rows
head(retail)

# Exploring the data
summary(retail)
table(retail$Segment)
table(retail$Category)
table(retail$Segment,retail$Category)
table(retail$Market,retail$Segment)

# checking of duplicated rows in the retail dataset

table(duplicated.data.frame(retail))  # false for all 51290 observations and hence no duplicated rows

# finding #NA Values in dataset

table(rowSums(is.na(retail)))  # 41296 missing values in 41296 rows

table(colSums(is.na(retail)))  # 41296 missing values in 1 column

# Finding column name in which NA values are present

Colmns.NA<-""
for(i in c(1:ncol(retail)))
{
  len<-length(grep("TRUE",is.na(retail[,i])))
  if(len>0){
    Colmns.NA<-paste(colnames(retail[i]),":",len,Colmns.NA)
  }
}

Colmns.NA

# Postal.Code has 41296 NA Values.  Countries other than United States have NA values for postal code
# will not change the NA values as this would not affect the objective of forecasting Sales & Quantity
# attributes.

# Changing order and ship date from character format to date format and postal code from integer to factor &
# changing Segment, City,State,Country, Market, Region, Category, sub.Category,shipmode & order priority from character to factors

retail$Order.Date<-as.Date(retail$Order.Date,"%d-%m-%Y")
retail$Ship.Date<-as.Date(retail$Ship.Date,"%d-%m-%Y")
retail$Postal.Code<-as.factor(retail$Postal.Code)
retail$Segment<-as.factor(retail$Segment)
retail$City<-as.factor(retail$City)
retail$State<-as.factor(retail$State)
retail$Country<-as.factor(retail$Country)
retail$Market<-as.factor(retail$Market)
retail$Region<-as.factor(retail$Region)
retail$Category<-as.factor(retail$Category)
retail$Category<-as.factor(retail$Sub.Category)
retail$Ship.Mode<-as.factor(retail$Ship.Mode)
retail$Order.Priority<-as.factor(retail$Order.Priority)


str(retail)

# subsetting the data and finding the top 2 most consistent segments on basis of profitablity

# Aggregate of profit, total sales, avg sales, number of times of sales,Total quantity on Segment and Market

Analy1<-aggregate(retail$Sales,by=list(retail$Market,retail$Segment),FUN=sum)
names(Analy1)<-list("Market","Segment","Total_Sales_Amt")
Analy2<-aggregate(retail$Profit,by=list(retail$Market,retail$Segment),FUN=sum)
names(Analy2)<-list("Market","Segment","Total_profit")
Analy3<-aggregate(retail$Sales,by=list(retail$Market,retail$Segment),FUN=mean)
names(Analy3)<-list("Market","Segment","Avg_Sales")
Analy4<-aggregate(retail$Sales,by=list(retail$Market,retail$Segment),FUN=length)
names(Analy4)<-list("Market","Segment","Num_times_Sales")
Analy5<-aggregate(retail$Quantity,by=list(retail$Market,retail$Segment),FUN=sum)
names(Analy5)<-list("Market","Segment","Total_Qty")


Summary_Sales<-data.frame(Analy1,Analy2,Analy3,Analy4,Analy5)
View(Summary_Sales)
Summary_Sales<-Summary_Sales[,-c(4,5,7,8,10,11,13,14)]
View(Summary_Sales)


# We need to analyse the monthly profitablity and consistency in terms of coefficient of variation
# and append the data to Summary_Sales to decided on top 2 market segments for doing time series anlaysis
# Hence we will now use aggregate functions to see month wise Sales, profitablity and consistency

Analy6<- aggregate(retail$Sales,by=list(retail$Market,retail$Segment,format(as.Date(retail$Order.Date),"%Y%m")),FUN=sum)
names(Analy6)<-list("Market","Segment","Month","Total_Mnthly_Sales")
Analy7<- aggregate(retail$Quantity,by=list(retail$Market,retail$Segment,format(as.Date(retail$Order.Date),"%Y%m")),FUN=sum)
names(Analy7)<-list("Market","Segment","Month","Total_Mnthly_Qty")
Analy8<- aggregate(retail$Sales,by=list(retail$Market,retail$Segment,format(as.Date(retail$Order.Date),"%Y%m")),FUN=mean)
names(Analy8)<-list("Market","Segment","Month","Avg_Mnthly_Sales")
Analy9<- aggregate(retail$Sales,by=list(retail$Market,retail$Segment,format(as.Date(retail$Order.Date),"%Y%m")),FUN=length)
names(Analy9)<-list("Market","Segment","Month","Numtimes_Mnthly_Sales")
Analy10<- aggregate(retail$Profit,by=list(retail$Market,retail$Segment,format(as.Date(retail$Order.Date),"%Y%m")),FUN=sum)
names(Analy10)<-list("Market","Segment","Month","Total_Mnthly_profit")

Monthly_Smmrysales<-data.frame(Analy6,Analy7,Analy8,Analy9,Analy10)
View(Monthly_Smmrysales)
Monthly_Smmrysales<-Monthly_Smmrysales[,-c(5,6,7,9,10,11,13,14,15,17,18,19)]

# Adding Monthly Profit_percent as derived metric & sorting data on decreasing order in terms of monthly sales

Monthly_Smmrysales$Mnthly_Profit_percent<-(Monthly_Smmrysales$Total_Mnthly_profit/Monthly_Smmrysales$Total_Mnthly_Sales*100)
Monthly_Smmrysales<-Monthly_Smmrysales[order(Monthly_Smmrysales$Total_Mnthly_Sales,decreasing = TRUE),]  

# Finding Average monthly profit and Standard deviation across Market & Segments

Analy11<-aggregate(Monthly_Smmrysales$Mnthly_Profit_percent,by=list(Monthly_Smmrysales$Market,Monthly_Smmrysales$Segment),FUN=mean)
names(Analy11)<-list("Market","Segment","Avg_Mnthlyprofit_percent")
Analy12<-aggregate(Monthly_Smmrysales$Mnthly_Profit_percent,by=list(Monthly_Smmrysales$Market,Monthly_Smmrysales$Segment),FUN=sd)
names(Analy12)<-list("Market","Segment","Mnthlyprofitpercent_Stddev")

# Appending Avg Monthly profit percent and standard deviation of monthly profit to Summayr sales dataframe to finally decide
# on most consistent market and segment based on Coefficient of Variation

Summary_Sales<-data.frame(Summary_Sales,Analy11,Analy12)
View(Summary_Sales)
Summary_Sales<-Summary_Sales[,-c(8,9,11,12)]

# Adding derived metric of Coefficient of Variaiton (CV) which is CV = Standard Deviation/Mean 

Summary_Sales$Coeff_Var_Mthlyyprofitpercent<-(Summary_Sales$Mnthlyprofitpercent_Stddev/Summary_Sales$Avg_Mnthlyprofit_percent)
Summary_Sales<-Summary_Sales[order(Summary_Sales$Total_profit,decreasing = TRUE),]
Summary_Sales$Total_Profit_Percent<-(Summary_Sales$Total_profit/Summary_Sales$Total_Sales_Amt*100)


# Graphical analysis of Total Profit, Profit Percentage and CV of Monthly Profit to arrive at top 2 profitable market segments

plot1<-ggplot(Summary_Sales,aes(x=Summary_Sales$Market,y=Summary_Sales$Total_profit,fill=Summary_Sales$Segment))+geom_bar(stat = "identity",position = "dodge")+xlab("Market")+ylab("Profit")+ggtitle("Total_Profit")
plot2<-ggplot(Summary_Sales,aes(x=Summary_Sales$Market,y=Summary_Sales$Total_Profit_Percent,fill=Summary_Sales$Segment))+geom_bar(stat = "identity",position = "dodge")+xlab("Market")+ylab("Total_Profit_Percent")+ggtitle("Total_Profit_Percent")
plot3<-ggplot(Summary_Sales,aes(x=Summary_Sales$Market,y=Summary_Sales$Coeff_Var_Mthlyyprofitpercent,fill=Summary_Sales$Segment))+geom_bar(stat = "identity",position = "dodge")+xlab("Market")+ylab("CV of Mthly_Profit_percent")+ggtitle("Coefficient Variation of Monthly Profit percent")
combined_plot<-grid.arrange(plot1,plot2,plot3)

# Based on higher profitablity and lower coefficient of variation the two most profitable segments choosen are

#  1. APAC Consumer
#  2. EU Consumer

###################################################################################################################################

 # 4.EXPLORATORY DATA ANALYSIS of ENTIRE RETAIL DATASET
#UNIVARIATE ANALYSIS OF CATEGORICAL VARIABLES
plot_grid(
  ggplot(retail,aes(x=retail$Order.Date))+geom_histogram(binwidth = 100,fill="#6600CC")+theme_minimal()+labs(title ="Order Trend", x = "Order Date", y = "No. of Orders"), 
  ggplot(retail,aes(x=retail$Ship.Mode))+geom_bar(fill="#6600CC")+theme_minimal()+labs(title ="Shipment Mode Analysis", x = "Ship Mode", y = "Count"), 
  ggplot(retail,aes(x=retail$Segment))+geom_bar(fill="#6600CC")+theme_minimal()+labs(title ="Customer Segment Analysis", x = "Segment", y = "Count"),
  ggplot(retail,aes(x=retail$Quantity))+geom_histogram(fill="#6600CC")+theme_minimal()+labs(title ="Quantity Distribution", x = "Quantity Bins", y = "Count"),
  ggplot(retail,aes(x=retail$Profit))+geom_histogram(binwidth = 30,fill="#6600CC")+theme_minimal()+labs(title ="Profit Distribution", x = "Profit Bins", y = "Count")+coord_cartesian(xlim = c(-500, 500)),
  ggplot(retail,aes(x=retail$Market))+geom_bar(fill="#6600CC")+theme_minimal()+labs(title ="Market Analysis", x = "Market", y = "Count")+coord_flip(),
  ggplot(retail,aes(x=retail$Region))+geom_bar(fill="#6600CC")+theme_minimal()+labs(title ="Region Analysis", x = "Region", y = "Count")+coord_flip(),
  ggplot(retail,aes(x=retail$Category))+geom_bar(fill="#6600CC")+theme_minimal()+labs(title ="Category Analysis", x = "Category", y = "Count")+coord_flip(),
  ggplot(retail,aes(x=retail$Order.Priority))+geom_bar(fill="#6600CC")+theme_minimal()+labs(title ="Order Priority Analysis", x = "Order Priority", y = "Count"),
  ggplot(retail,aes(x=retail$Sales))+geom_histogram(binwidth = 30,fill="#6600CC")+theme_minimal()+labs(title ="Sales Distribution", x = "Sales Bins", y = "Count")+coord_cartesian(xlim = c(0, 2000)),rows=2)

#SEGMENTED UNIVARIATE ANALYSIS
#Market wise Sales and Profit Distribution
ggplot(retail,aes(x=retail$Sales))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Market wise Sales Distribution", x = "Sales Bins", y = "Count")+coord_cartesian(xlim = c(0, 500))+facet_wrap(~retail$Market)
ggplot(retail,aes(x=retail$Profit))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Market wise Profits Distribution", x = "Profit Bins", y = "Count")+coord_cartesian(xlim = c(-500, 500))+facet_wrap(~retail$Market)
#Segmentwise Sales and Profit Distribution
plot_grid(
  ggplot(retail,aes(x=retail$Sales))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Segment wise Sales Distribution", x = "Sales Bins", y = "Count")+coord_cartesian(xlim = c(0, 500))+facet_wrap(~retail$Segment),
  ggplot(retail,aes(x=retail$Profit))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Segment wise Profits Distribution", x = "Profit Bins", y = "Count")+coord_cartesian(xlim = c(-500, 500))+facet_wrap(~retail$Segment),rows=2)
#Regionwise Profit Distribution
ggplot(retail,aes(x=retail$Profit))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Region wise Profits Distribution", x = "Profit Bins", y = "Count")+coord_cartesian(xlim = c(-500, 500))+facet_wrap(~retail$Region)
#Product Categorywise Profit Distribution
ggplot(retail,aes(x=retail$Profit))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Product Category wise Profits Distribution", x = "Profit Bins", y = "Count")+coord_cartesian(xlim = c(-500, 500))+facet_wrap(~retail$Category)
#Order Prioritywise Profits Distribution
ggplot(retail,aes(x=retail$Profit))+geom_histogram(binwidth = 30,fill="#FF3333")+theme_minimal()+labs(title ="Order Priority wise Profits Distribution", x = "Profit Bins", y = "Count")+coord_cartesian(xlim = c(-500, 500))+facet_wrap(~retail$Order.Priority)

#Bivariate Analysis
plot_grid(
  #Product Category wise Sales and Profit
  ggplot(retail,aes(x=Category,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#FF9933")+coord_flip()+xlab("Category")+ylab("Sales")+ggtitle("Category wise Sales")+theme_minimal(),
  ggplot(retail,aes(x=Category,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#FF9933")+coord_flip()+xlab("Category")+ylab("Profit")+ggtitle("Category wise Profit")+theme_minimal(),
  
  #Market wise Sales and Profit
  ggplot(retail,aes(x=Market,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#FF9933")+coord_flip()+xlab("Market")+ylab("Sales")+ggtitle("Market wise Sales")+theme_minimal(),
  ggplot(retail,aes(x=Market,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#FF9933")+coord_flip()+xlab("Market")+ylab("Profit")+ggtitle("Market wise Profit")+theme_minimal(),cols=2)

plot_grid(
  #Segment wise Sales and Profit
  ggplot(retail,aes(x=Segment,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#FF9933")+xlab("Segment")+ylab("Sales")+ggtitle("Segment wise Sales")+theme_minimal(),
  ggplot(retail,aes(x=Segment,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#FF9933")+xlab("Segment")+ylab("Profit")+ggtitle("Segment wise Profit")+theme_minimal(),
  
  #Region wise Sales and Profit
  ggplot(retail,aes(x=Region,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#FF9933")+xlab("Region")+ylab("Sales")+ggtitle("Region wise Sales")+theme_minimal()+coord_flip(),
  ggplot(retail,aes(x=Region,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#FF9933")+xlab("Region")+ylab("Profit")+ggtitle("Region wise Profit")+theme_minimal()+coord_flip(),cols=2)

#Exploratory data analysis on APAC & EU Consumer

APAC_Consumer <-subset(retail,(retail$Market=="APAC")&(retail$Segment=="Consumer"))
EU_Consumer<-subset(retail,(retail$Market=="EU")&(retail$Segment=="Consumer"))
#BIVARIATE ANALYSIS ON APAC CONSUMER
plot_grid(
  #Product Category wise Sales and Profit
  ggplot(APAC_Consumer,aes(x=Category,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#CC6600")+coord_flip()+xlab("Category")+ylab("Sales")+ggtitle("Category wise Sales")+theme_minimal(),
  ggplot(APAC_Consumer,aes(x=Category,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#CC6600")+coord_flip()+xlab("Category")+ylab("Profit")+ggtitle("Category wise Profit")+theme_minimal(),
  
  #Region wise Sales and Profit
  ggplot(APAC_Consumer,aes(x=Region,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#CC6600")+xlab("Region")+ylab("Sales")+ggtitle("Region wise Sales")+theme_minimal()+coord_flip(),
  ggplot(APAC_Consumer,aes(x=Region,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#CC6600")+xlab("Region")+ylab("Profit")+ggtitle("Region wise Profit")+theme_minimal()+coord_flip(),cols=2)

#BIVARIATE ANALYSIS ON EU CONSUMER
plot_grid(
  #Product Category wise Sales and Profit
  ggplot(EU_Consumer,aes(x=Category,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#CC6600")+coord_flip()+xlab("Category")+ylab("Sales")+ggtitle("Category wise Sales")+theme_minimal(),
  ggplot(EU_Consumer,aes(x=Category,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#CC6600")+coord_flip()+xlab("Category")+ylab("Profit")+ggtitle("Category wise Profit")+theme_minimal(),
  
  #Region wise Sales and Profit
  ggplot(EU_Consumer,aes(x=Region,y=Sales))+geom_bar(stat = "identity",position = "dodge",fill="#CC6600")+xlab("Region")+ylab("Sales")+ggtitle("Region wise Sales")+theme_minimal()+coord_flip(),
  ggplot(EU_Consumer,aes(x=Region,y=Profit))+geom_bar(aes(fill=variable),stat = "sum",position = "dodge",fill="#CC6600")+xlab("Region")+ylab("Profit")+ggtitle("Region wise Profit")+theme_minimal()+coord_flip(),cols=2)


######################################################################################################################################

# 5. Model Building 
#  5.1 Classical Decomposition
#  5.2 Auto Arima
# 6 Model Evaluation & Forecasting 


####################################################################################################################################

#                                                       APAC CONSUMER

####################################################################################################################################

# Fitting APAC Consumer Sales Time series and forecasting for months 49 to 54

APAC_Sales<-subset(Monthly_Smmrysales,(Monthly_Smmrysales$Market=="APAC")&(Monthly_Smmrysales$Segment=="Consumer"))
APAC_Sales<-APAC_Sales[order(APAC_Sales$Month),]
APAC_Sales_TS<-APAC_Sales[,c(3,4)]
APAC_Qty_TS<-APAC_Sales[,c(3,5)]
APAC_Sales_TS$Month<-c(1:48)
APAC_Qty_TS$Month<-c(1:48)

nrow(APAC_Sales_TS)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser_sales_APAC<-ts(APAC_Sales_TS$Total_Mnthly_Sales)
indata_APAC_sales<-APAC_Sales_TS[1:42,]
timeser_sales_APAC<-ts(indata_APAC_sales$Total_Mnthly_Sales)
plot(timeser_sales_APAC)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries_1 <- stats::filter(timeser_sales_APAC, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_1[w+2] - smoothedseries_1[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_1[i] <- smoothedseries_1[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser_sales_APAC)
diff <- smoothedseries_1[n-w] - smoothedseries_1[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_1[i] <- smoothedseries_1[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata_APAC_sales$Month
lines(smoothedseries_1, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf1 <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_1)))
colnames(smootheddf1) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf1)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_sales_APAC-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

# Series: local_pred 
# ARIMA(0,0,0) with zero mean 

# sigma^2 estimated as 8.8e+07:  log likelihood=-443.75
# AIC=889.49   AICc=889.59   BIC=891.23

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi
# Dickey-Fuller = -6.8673, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi)


# KPSS Test for Level Stationarity

# data:  resi
# KPSS Level = 0.021731, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of APAC_Sales is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_APAC_sales <- APAC_Sales_TS[43:48,]
timevals_out <- outdata_APAC_sales$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

# forecast values
#        1        2        3        4        5        6 
# 48558.48 46804.80 43743.10 41101.79 40649.85 43381.89 

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata_APAC_sales[,2])[5]
MAPE_class_dec

# [1] 31.07429

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser_sales_APAC, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser_sales_APAC)
autoarima

# Series: timeser_sales_APAC 
# ARIMA(0,1,1) 

# Coefficients:
#   ma1
# -0.7559
# s.e.   0.1381

# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66


tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser_sales_APAC - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima
# Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi_auto_arima)

# KPSS Test for Level Stationarity

# data:  resi_auto_arima
# KPSS Level = 0.042734, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of APAC_Sales is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

# $pred
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 44898.7 44898.7 44898.7 44898.7 44898.7 44898.7

# $se
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 13204.60 13592.40 13969.43 14336.56 14694.51 15043.95

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata_APAC_sales[,2])[5]
MAPE_auto_arima

# 27.68952

#Lastly, let's plot the predictions along with original values to visually view the fit


auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser_sales_APAC, col = "black")
lines(auto_arima_pred, col = "red")

# Though autoarima MAPE is lower than decomposition method, we clearly see that values for month 43 to 48 is same and hence choosing
# classical decomposition method.

# Now predicting Sales for months 49 to 54 using classical decomposition method.

Pred_APAC_Sales <- predict(lmfit, data.frame(Month=c(49:54)))
Pred_APAC_Sales

#        1        2        3        4        5        6 
# 48894.49 55229.36 59321.81 57988.42 49177.89 33068.78 


# ---------------------------------- Forecasting APAC Consumer - Sales Quantity --------------------------------------------

 
nrow(APAC_Qty_TS)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser_qty_APAC<-ts(APAC_Qty_TS$Total_Mnthly_Qty)
indata_APAC_qty<-APAC_Qty_TS[1:42,]
timeser_qty_APAC<-ts(indata_APAC_qty$Total_Mnthly_Qty)
plot(timeser_qty_APAC)

#Smoothing the series - Moving Average Smoothing

x <-1
smoothedseries_2 <- stats::filter(timeser_qty_APAC, 
                                  filter=rep(1/(2*x+1),(2*x+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff1 <- smoothedseries_2[x+2] - smoothedseries_2[x+1]
for (i in seq(x,1,-1)) {
  smoothedseries_2[i] <- smoothedseries_2[i+1] - diff1
}

#Smoothing right end of the time series

n1 <- length(timeser_qty_APAC)
diff <- smoothedseries_2[n1-x] - smoothedseries_2[n1-x-2]
for (i in seq(n1-x+1, n1)) {
  smoothedseries_2[i] <- smoothedseries_2[i-1] + diff1
}

#Plot the smoothed time series

timevals_APACqty_in <- indata_APAC_qty$Month
lines(smoothedseries_2, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf2 <- as.data.frame(cbind(timevals_APACqty_in, as.vector(smoothedseries_2)))
colnames(smootheddf2) <- c('Month', 'Qty')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APACqty <- lm(Qty ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf2)
global_pred1 <- predict(lmfit_APACqty, Month=timevals_APACqty_in)
summary(global_pred1)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 211.5   305.7   420.0   403.2   490.2   604.4 

lines(timevals_APACqty_in, global_pred1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred1 <- timeser_qty_APAC-global_pred1
plot(local_pred1, col='red', type = "l")
acf(local_pred1)
acf(local_pred1, type="partial")
armafit_APACqty <- auto.arima(local_pred1)
tsdiag(armafit_APACqty)
armafit_APACqty

# Series: local_pred1 
# ARIMA(0,0,0) with zero mean 

# sigma^2 estimated as 10930:  log likelihood=-254.88
# AIC=511.76   AICc=511.86   BIC=513.5

# To check if the residual series is white noise

resi1 <- local_pred1-fitted(armafit_APACqty)

adf.test(resi1,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi1
# Dickey-Fuller = -7.6696, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi1)


# KPSS Test for Level Stationarity

# data:  resi1
# KPSS Level = 0.035507, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of APAC_Qty is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_APAC_qty <- APAC_Qty_TS[43:48,]
timevals_APACqty_out <- outdata_APAC_qty$Month

global_pred_out_APACqty <- predict(lmfit_APACqty,data.frame(Month =timevals_APACqty_out))

fcast1 <- global_pred_out_APACqty

# forecast values
#        1        2        3        4        5        6 
# 662.2493 745.9566 822.9346 882.4608 912.8208 903.1450  

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(fcast1,outdata_APAC_qty[,2])
MAPE_class_dec1

#                 ME     RMSE      MAE       MPE     MAPE
# Test set -88.59453 144.5853 112.7887 -18.39836 21.35525

# MAPE -  21.35525

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(global_pred1),ts(global_pred_out_APACqty))
plot(total_timeser_qty_APAC, col = "black")
lines(class_dec_pred1, col = "red")

#That was classical decomposition, now let's do an ARIMA fit

autoarima1 <- auto.arima(timeser_qty_APAC)
autoarima1
# Series: timeser_qty_APAC 
# ARIMA(0,1,0) 

# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85

tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima1 <- timeser_qty_APAC - fitted(autoarima1)

adf.test(resi_auto_arima1,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima1
# Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi_auto_arima1)

# KPSS Test for Level Stationarity

# data:  resi_auto_arima1
# KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of APAC_Qty is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima1 <- predict(autoarima1, n.ahead = 6)

# $pred
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 721 721 721 721 721 721

# $se
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 159.2675 225.2382 275.8593 318.5349 356.1329 390.1240

MAPE_auto_arima1 <- accuracy(fcast_auto_arima1$pred,outdata_APAC_qty[,2])
MAPE_auto_arima1

#         ME     RMSE      MAE       MPE     MAPE
#Test set 12 174.3722 147.6667 -7.362467 26.24458

# MAPE - 26.24458

#Lastly, let's plot the predictions along with original values to visually view the fit


auto_arima_pred1 <- c(fitted(autoarima1),ts(fcast_auto_arima1$pred))
plot(total_timeser_qty_APAC, col = "black")
lines(auto_arima_pred1, col = "red")

# Autoarima MAPE is lower in decomposition method and also we clearly see that values for month 43 to 48 is same in Auto Arima Model 
# and hence choosing classical decomposition method.

# Now predicting Sales for months 49 to 54 using classical decomposition method for APAC_Qty

Pred_APAC_Qty <- predict(lmfit_APACqty, data.frame(Month=c(49:54)))
Pred_APAC_Qty

#        1        2        3        4        5        6 
# 846.8576 746.0416 615.1924 482.4275 386.5087 369.0919

####################################################################################################################################

#                                                       EU CONSUMER

####################################################################################################################################

# Fitting EU Consumer Sales Time series and forecasting for months 49 to 54

EU_Sales<-subset(Monthly_Smmrysales,(Monthly_Smmrysales$Market=="EU")&(Monthly_Smmrysales$Segment=="Consumer"))
EU_Sales<-EU_Sales[order(EU_Sales$Month),]
EU_Sales_TS<-EU_Sales[,c(3,4)]
EU_Qty_TS<-EU_Sales[,c(3,5)]
EU_Sales_TS$Month<-c(1:48)
EU_Qty_TS$Month<-c(1:48)

nrow(EU_Sales_TS)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser_sales_EU<-ts(EU_Sales_TS$Total_Mnthly_Sales)
indata_EU_sales<-EU_Sales_TS[1:42,]
timeser_sales_EU<-ts(indata_EU_sales$Total_Mnthly_Sales)
plot(timeser_sales_EU)

#Smoothing the series - Moving Average Smoothing

y <-1
smoothedseries_3 <- stats::filter(timeser_sales_EU, 
                                  filter=rep(1/(2*y+1),(2*y+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff2 <- smoothedseries_3[y+2] - smoothedseries_3[y+1]
for (i in seq(y,1,-1)) {
  smoothedseries_3[i] <- smoothedseries_3[i+1] - diff2
}

#Smoothing right end of the time series

n2 <- length(timeser_sales_EU)
diff2 <- smoothedseries_3[n2-y] - smoothedseries_3[n2-y-1]
for (i in seq(n2-y+1, n2)) {
  smoothedseries_3[i] <- smoothedseries_3[i-1] + diff2
}

#Plot the smoothed time series

timevals_in_EUsales <- indata_EU_sales$Month
lines(smoothedseries_3, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf3 <- as.data.frame(cbind(timevals_in_EUsales, as.vector(smoothedseries_3)))
colnames(smootheddf3) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EUsales <- lm(Sales ~ sin(0.55*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf3)
global_pred2 <- predict(lmfit_EUsales, Month=timevals_in_EUsales)
summary(global_pred2)
lines(timevals_in_EUsales, global_pred2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred2 <- timeser_sales_EU-global_pred2
plot(local_pred2, col='red', type = "l")
acf(local_pred2)
acf(local_pred2, type="partial")
armafit_EUsales <- auto.arima(local_pred2)

tsdiag(armafit_EUsales)
armafit_EUsales

# Series: local_pred2 
# ARIMA(0,0,0) with zero mean 

# sigma^2 estimated as 105330470:  log likelihood=-447.52
# AIC=897.04   AICc=897.14   BIC=898.78

#We'll check if the residual series is white noise

resi2 <- local_pred2-fitted(armafit_EUsales)

adf.test(resi2,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi2
# Dickey-Fuller = -4.1886, Lag order = 3, p-value = 0.01175
# alternative hypothesis: stationary

kpss.test(resi2)


# KPSS Test for Level Stationarity

# data:  resi2
# KPSS Level = 0.042512, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of EU_Sales is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_EU_sales <- EU_Sales_TS[43:48,]
timevals_out_EUsales <- outdata_EU_sales$Month

global_pred_out_EUsales <- predict(lmfit_EUsales,data.frame(Month =timevals_out_EUsales))

fcast2 <- global_pred_out_EUsales

# forecast values
#        1        2        3        4        5        6 
# 41956.34 43625.94 43857.79 42280.25 38807.19 33765.27
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec2 <- accuracy(fcast2,outdata_EU_sales[,2])
MAPE_class_dec2

#                 ME     RMSE      MAE    MPE     MAPE
# Test set  11950.15 19617.93 17257.02 15.087 30.94227

# Mape 30.94227

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred2 <- c(ts(global_pred2),ts(global_pred_out_EUsales))
plot(total_timeser_sales_EU, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima2 <- auto.arima(timeser_sales_EU)
autoarima2
# Series: timeser_sales_EU 
# ARIMA(2,1,0) 

# Coefficients:
#           ar1      ar2
#       -0.5796  -0.4906
# s.e.   0.1346   0.1310

# sigma^2 estimated as 168564623:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81

tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima2 <- timeser_sales_EU - fitted(autoarima2)

adf.test(resi_auto_arima2,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima2
# Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary

kpss.test(resi_auto_arima2)

# KPSS Test for Level Stationarity

# data:  resi_auto_arima2
# KPSS Level = 0.05314, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of EU_Sales is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima2 <- predict(autoarima2, n.ahead = 6)

# $pred
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 39297.86 37221.06 42062.87 40275.32 38936.08 40589.28

# $se
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 12983.24 14083.80 14500.28 16710.60 17921.71 18648.29

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,outdata_EU_sales[,2])
MAPE_auto_arima2

#                ME     RMSE     MAE    MPE    MAPE
# Test set 12935.21 19499.14 16687.6 17.678 28.9226

# Mape is 28.9226

#Lastly, let's plot the predictions along with original values to visually view the fit


auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(total_timeser_sales_EU, col = "black")
lines(auto_arima_pred2, col = "red")

# Though autoarima MAPE is lower than decomposition method, we will decide based on prediction values for month 49 to 54
# based on decomposition numbers and autoarima numbers

# Now predicting Sales for months 49 to 54 using classical decomposition method.

Pred_EU_Sales <- predict(lmfit_EUsales, data.frame(Month=c(49:54)))
Pred_EU_Sales

#        1        2        3        4        5        6 
# 27961.65 22632.71 19236.00 19094.87 22965.78 30651.10  

# Now predicting EU Sales for months 49 to 54 using Auto Arima method

predict(autoarima2,n.ahead = 12)

# $pred
# Time Series:
#  Start = 43 
#    End = 54 
# Frequency = 1 
#           43       44       45       46       47       48       49       50       51       52       53       54
# [1] 39297.86 37221.06 42062.87 40275.32 38936.08 40589.28 40288.07 39651.62 40168.29 40181.05 39920.18 40065.12

# $se
# Time Series:
#   Start = 43 
#   End = 54 
# Frequency = 1 
# [1] 12983.24 14083.80 14500.28 16710.60 17921.71 18648.29 19855.66 20893.27 21702.03 22623.58 23511.22 24299.39

#  With statistical accuracy of MAPE we choose AutoArima model for EUSales.

# ---------------------------------- Forecasting EU Consumer - Sales Quantity --------------------------------------------


nrow(EU_Qty_TS)

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

total_timeser_qty_EU<-ts(EU_Qty_TS$Total_Mnthly_Qty)
indata_EU_qty<-EU_Qty_TS[1:42,]
timeser_qty_EU<-ts(indata_EU_qty$Total_Mnthly_Qty)
plot(timeser_qty_EU)

#Smoothing the series - Moving Average Smoothing

z <-1
smoothedseries_4<- stats::filter(timeser_qty_EU, 
                                  filter=rep(1/(2*z+1),(2*z+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff3 <- smoothedseries_4[z+2] - smoothedseries_4[z+1]
for (i in seq(z,1,-1)) {
  smoothedseries_4[i] <- smoothedseries_4[i+1] - diff3
}

#Smoothing right end of the time series

n3 <- length(timeser_qty_EU)
diff3 <- smoothedseries_4[n3-z] - smoothedseries_4[n3-z-2]
for (i in seq(n3-z+1, n3)) {
  smoothedseries_4[i] <- smoothedseries_4[i-1] + diff3
}

#Plot the smoothed time series

timevals_EUqty_in <- indata_EU_qty$Month
lines(smoothedseries_4, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf4 <- as.data.frame(cbind(timevals_EUqty_in, as.vector(smoothedseries_4)))
colnames(smootheddf4) <- c('Month', 'Qty')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EUqty <- lm(Qty ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
                    + Month, data=smootheddf4)
global_pred3 <- predict(lmfit_EUqty, Month=timevals_EUqty_in)
summary(global_pred3)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 142.5   304.1   359.1   365.3   451.0   550.3

lines(timevals_EUqty_in, global_pred3, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred3 <- timeser_qty_EU-global_pred3
plot(local_pred3, col='red', type = "l")
acf(local_pred3)
acf(local_pred3, type="partial")

armafit_EUqty <- auto.arima(local_pred3)

tsdiag(armafit_EUqty)
armafit_EUqty

# Series: local_pred3 
# ARIMA(2,0,0) with zero mean 

# Coefficients:
#           ar1      ar2
#       -0.6321  -0.6149
# s.e.   0.1177   0.1135

# sigma^2 estimated as 7346:  log likelihood=-246.07
# AIC=498.14   AICc=498.77   BIC=503.35

# To check if the residual series is white noise

resi3 <- local_pred3-fitted(armafit_EUqty)

adf.test(resi3,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi3
# Dickey-Fuller = -6.5322, Lag order = 3, p-value = 0.01
# alternative hypothesis: stationary


kpss.test(resi3)


# KPSS Test for Level Stationarity

# data:  resi3
# KPSS Level = 0.029728, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of EU_Qty is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_EU_qty <- EU_Qty_TS[43:48,]
timevals_EUqty_out <- outdata_EU_qty$Month

global_pred_out_EUqty <- predict(lmfit_EUqty,data.frame(Month =timevals_EUqty_out))

fcast3 <- global_pred_out_EUqty

# forecast values
#        1        2        3        4        5        6 
# 654.9468 751.6809 817.9781 836.9309 802.9654 725.3473  

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec3 <- accuracy(fcast3,outdata_EU_qty[,2])
MAPE_class_dec3

#                 ME     RMSE     MAE       MPE     MAPE
# Test set -56.64159 215.9931 189.977 -18.68602 33.33526

# MAPE -  33.33526

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred3 <- c(ts(global_pred3),ts(global_pred_out_EUqty))
plot(total_timeser_qty_EU, col = "black")
lines(class_dec_pred3, col = "red")

#That was classical decomposition, now let's do an ARIMA fit

autoarima3 <- auto.arima(timeser_qty_EU)
autoarima3
# Series: timeser_qty_EU 
# ARIMA(2,1,0) 

# Coefficients:
#   ar1      ar2
# 0.7359  -0.5879
# s.e.   0.1224   0.1185

# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94

tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima3 <- timeser_qty_EU - fitted(autoarima3)

adf.test(resi_auto_arima3,alternative = "stationary")

# Augmented Dickey-Fuller Test

# data:  resi_auto_arima3
# Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
# alternative hypothesis: stationary

kpss.test(resi_auto_arima3)

# KPSS Test for Level Stationarity

# data:  resi_auto_arima3
# KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 we safely conclude residual series of EU_Qty is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima3 <- predict(autoarima3, n.ahead = 6)

# $pred
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 452.7129 448.8772 491.8447 462.4816 458.8288 478.7789

# $se
# Time Series:
#  Start = 43 
#  End = 48 
# Frequency = 1 
# [1] 145.5500 150.5419 153.8418 183.2835 190.9621 196.7892

MAPE_auto_arima3 <- accuracy(fcast_auto_arima3$pred,outdata_EU_qty[,2])
MAPE_auto_arima3

#               ME     RMSE      MAE      MPE     MAPE
# Test set 242.746 316.7626 253.8108 27.53891 30.13319

# MAPE - 30.13319

#Lastly, let's plot the predictions along with original values to visually view the fit


auto_arima_pred3 <- c(fitted(autoarima3),ts(fcast_auto_arima3$pred))
plot(total_timeser_qty_EU, col = "black")
lines(auto_arima_pred3, col = "red")

# Though autoarima MAPE is lower than decomposition method, we clearly see that values for month 43 to 48 is same and hence choosing
# classical decompositiong method.

# Now predicting Qty for months 49 to 54 using classical decomposition method.

Pred_EU_Qty <- predict(lmfit_EUqty, data.frame(Month=c(49:54)))
Pred_EU_Qty

#        1        2        3        4        5        6 
# 628.0992 545.7354 515.3094 566.4012 711.5438 939.8777

# Now predicting Qty for months 43 to 54 using AutoArima model for EU qty

predict(autoarima3,n.ahead = 12)

# $pred
# Time Series:
#   Start = 43 
#   End = 54 
# Frequency = 1 
# [1] 452.7129 448.8772 491.8447 462.4816 458.8288 478.7789 466.2458 463.7401 472.9520 467.6464 466.1350 470.3663

# $se
# Time Series:
#   Start = 43 
#   End = 54 
# Frequency = 1 
# [1] 145.5500 150.5419 153.8418 183.2835 190.9621 196.7892 212.0755 220.2454 227.0917 237.6027 245.4548 252.4798

#  Classical decompostion method maimes the cyclic function and predicts the Qty hence we find very high in beginning and 
#  end period (Month 49 & Month 54).  However auto arima method is more tempered in predictions.  Now which predictions to use 
#  is an interesting problem to handle.  With statistical accuracy of MAPE if we choose AutoArima method than entity
#  would miss the sales oppurtunity but will protect its bottomline as investment in stocks will be limited. Trade off between
#  loss in investment in stock and loss in missing higher sales opputunity has to be evalauted before choosing the model.

#######################################################################################################################################



