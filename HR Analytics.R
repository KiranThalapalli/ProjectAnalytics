############################ HR Analytics Case Study Solution #######################################
#SUBMITTED BY Vikram Shreedhar DDA1710117, Kiran T DDA1710335,Prakash,Jaideep
#####################################################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
#####################################################################################################
### Business Understanding:

# Based on the past and current employee information,
# the company has maintained a database containing performance/personal information,
# by an employee,working hours, absentism, worklife balance, years of associaiton,work
# environment satisfaction levels, job involvement & satisfaction index.

## AIM:

# The aim is to automate the process of predicting 
# if an employee would leave company  or not and to find the factors affecting the attrition.
# This can be approached with Binary Classification Logistic Rgression  
# Whether an employee will leave or not will depend on data from the following four buckets:

# 1. Employee Information
# 2. Manager Survey data
# 3. Employee Survey data
# 4. Employee working hours data

##################################################################################################

### Data Understanding

#Loading Required Packages
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(DescTools)
library(GGally)
library(ROCR)

# Loading files
EmplySrvy_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
MgrSrvy_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
Emplygnrl_data<-read.csv("general_data.csv",stringsAsFactors = F)
Emply_intime<-read.csv("in_time.csv",stringsAsFactors = F)
Emply_outtime<-read.csv("out_time.csv",stringsAsFactors = F)

str(EmplySrvy_data) # 4410 observations of 4 variables
str(MgrSrvy_data)   # 4410 observations of 3 variables 
str(Emplygnrl_data) # 4410 observations of 24 variables including target variable 
str(Emply_intime)   # 4410 observations of intime of 262 working days 
str(Emply_outtime)  # 4410 observations of outtime of 262 working days 

# collating the data together in one file

length(unique(EmplySrvy_data$EmployeeID))  # 4410, confirming EmployeeID is key
length(unique(Emplygnrl_data$EmployeeID))  # 4410, confirming EmployeeID is key
length(unique(MgrSrvy_data$EmployeeID))    # 4410, confirming EmployeeID is key

#Confirming the EmpolyeeID's matches before merging 
setdiff(Emplygnrl_data$EmployeeID,EmplySrvy_data$EmployeeID)  # Identical EmployeeID across datasets
setdiff(Emplygnrl_data$EmployeeID,MgrSrvy_data$EmployeeID)    # Identical EmployeeID across datasets
setdiff(Emply_intime$X,Emply_outtime$X)                       # Identical EmployeeID across datasets
setdiff(Emplygnrl_data$EmployeeID,Emply_intime$X)             # Identical EmployeeID across datasets
setdiff(Emplygnrl_data$EmployeeID,Emply_outtime$X)            # Identical EmployeeID across datasets 

Employee<-merge(Emplygnrl_data,EmplySrvy_data,by="EmployeeID",all=F)
Employee<-merge(Employee,MgrSrvy_data,by="EmployeeID",all=F)

#on checking the both Intime & outtime datasets the count of NA's is same thus concluding that
#none of the employees joined during the year, or some employees has missed punching of times

table(is.na(Emply_intime))
table(is.na(Emply_outtime))

#Employee In time has 261 working days by removing 104 weekends from 365 days in an year. Also
#there are 12 public holidays on which time stamp for employees is not recorded and removing
#the same from both Intime and out time date. 
#Removing Columns whose percebtage of value is 100% or 1  

inTime_missing_values <- Emply_intime %>% summarise_all(funs(sum(is.na(.))/n()))
Emply_intime <- Emply_intime[,-(which(inTime_missing_values==1))]

outTime_missing_values <- Emply_outtime %>% summarise_all(funs(sum(is.na(.))/n()))
Emply_outtime <- Emply_outtime[,-(which(outTime_missing_values==1))]

#converting the character format to date formats

Emply_intime[,2:250]<-as.data.frame(lapply(Emply_intime[,2:250],strptime,format="%Y-%m-%d %H:%M:%S"))
Emply_outtime[,2:250]<-as.data.frame(lapply(Emply_outtime[,2:250],strptime,format="%Y-%m-%d %H:%M:%S"))

str(Emply_intime)
str(Emply_outtime)

#Calculating the hours of work for each employee

Diff_time<-as.data.frame(Map(difftime,Emply_outtime[,-1],Emply_intime[,-1]))

#Calculating the number of absent days in an year and % of absentism

Diff_time$Days_of_Absent<-apply(Diff_time,1,function(x) sum(is.na(x)))
Diff_time$percent_of_Absentism<-round(Diff_time$Days_of_Absent/249,2)

#Replacing all NA values as 0 as if an employee is absent taking the NA value as 0 will not affect
# mean or median of employee hours of work

Diff_time[is.na(Diff_time)]<-0

#Converting the POSXict format to numeric format so that median Actual work hours is counted.

Diff_time<-as.data.frame(sapply(Diff_time,as.numeric))
str(Diff_time)
Diff_time$MedianWrkhrs<-apply(Diff_time,1,median,na.rm=F)
Diff_time<-cbind(Emply_intime$X,Diff_time)
colnames(Diff_time)[1]<-c("EmployeeID")


#binding the the Medianwrkhrs,days_of_absent & percent_of_absentism column to masterfile of Employee

Employee<-cbind(Diff_time$MedianWrkhrs,Employee)
Employee<-cbind(Diff_time$Days_of_Absent,Employee)
Employee<-cbind(Diff_time$percent_of_Absentism,Employee)
colnames(Employee)[1]<-c("percent_of_Absentism")
colnames(Employee)[2]<-c("Days_of_Absent")
colnames(Employee)[3]<-c("Median_Actual_Wrkhrs")
Employee<-setcolorder(Employee,c(4:32,3,2,1))

View(Employee) ## Masterfile

##################################################################################################
### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(Employee) #4410 obs. of 32 variables;

#converting numeric to integer

Employee$percent_of_Absentism<-as.integer(Employee$percent_of_Absentism*100) 
Employee$Days_of_Absent<-as.integer(Employee$Days_of_Absent)

Employee$Median_Actual_Wrkhrs<-round(Employee$Median_Actual_Wrkhrs,2) # rounding to 2 decimals

#removing certain variables which are same across all employees.  Standard hours of work
#count of employees, Over 18 years Flag.  Also removing days of absent as percentage of absentism
#represents the same.

Employee$EmployeeCount<-NULL
Employee$Over18<-NULL
Employee$StandardHours<-NULL
Employee$Days_of_Absent<-NULL

str(Employee)

###################################################################################################

#Age,distance from home, Monthly income,Num of companies worked, percent sal hike, totalwkg years,
#training time last year, year at company, years since last promotion, years with curr manager,
#median actual wrk hrs, percent of absentism are continous variables

#Performance rating, work life balance, jobsatisfaction, Environ satisfaction, stock option level, 
#attrition, business travel, department, education, education field, gender, job level, job roles &
#maritial status are categorical variables

#################################################################################
#Exploratory Data Analysis
#Creating a general theme for Box plots and Bar plots
box_theme_x<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                    axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  legend.position="none")

##################################################################################
#Univariate Analysis of Continuous Variable

plot_grid(
#Age Distribution
  plot_grid(ggplot(Employee,aes(x=Employee$Age))+geom_histogram(bins = 10,aes(color="white"))+theme_minimal()+
  labs(title="Age Distribution",x="Age",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#DistanceFromHome
plot_grid(ggplot(Employee,aes(x=Employee$DistanceFromHome))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Distance Distribution",x="Distance from Home",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#MonthlyIncome
plot_grid(ggplot(Employee,aes(x=Employee$MonthlyIncome))+geom_histogram(bins = 15,aes(color="white"))+theme_minimal()+
  labs(title="Monthly Income Distribution",x="Monthly Income",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#NumCompaniesWorked
plot_grid(ggplot(Employee,aes(x=Employee$NumCompaniesWorked))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="No. of Companies Worked",x="No. of Companies Worked",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#PercentSalaryHike
plot_grid(ggplot(Employee,aes(x=Employee$PercentSalaryHike))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Percentage Hike Distribution",x="% Salary Hike",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#TotalWorkingYears
plot_grid(ggplot(Employee,aes(x=Employee$TotalWorkingYears))+geom_histogram(bins = 10,aes(color="white"))+theme_minimal()+
  labs(title="Total Working Years Distribution",x="Total Working Years",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),ncol=3,nrow=2)



plot_grid(
#TrainingTimesLastYear
plot_grid(ggplot(Employee,aes(x=Employee$TrainingTimesLastYear))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Trainings Taken Last Year Distribution",x="Trainings Taken Last Year",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#YearsAtCompany
plot_grid(ggplot(Employee,aes(x=Employee$YearsAtCompany))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Years at Company Distribution",x="Years at Company",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#Years since last promotion
plot_grid(ggplot(Employee,aes(x=Employee$YearsSinceLastPromotion))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Years since last promotion",x="Years since last promotion",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#YearsWithCurrentManager
plot_grid(ggplot(Employee,aes(x=Employee$YearsWithCurrManager))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Years with current Manager",x="Years with current Manager",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#ActualWorkHours
plot_grid(ggplot(Employee,aes(x=Employee$Median_Actual_Wrkhrs))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="Actual Work Hours Distribution",x="Median Actual Work Hours",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=Median_Actual_Wrkhrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),

#PercentageAbsent
plot_grid(ggplot(Employee,aes(x=Employee$percent_of_Absentism))+geom_histogram(bins = 5,aes(color="white"))+theme_minimal()+
  labs(title="% Absent Distribution",x="% Absent",y="Count")+theme(legend.position = "none"),
  ggplot(Employee, aes(x="",y=percent_of_Absentism))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, align = "v",ncol = 1),ncol=3,nrow=2)

####################################################################################
#Univariate Analysis of Categorical Variables

plot_grid(
#Attrition
ggplot(Employee,aes(x=Employee$Attrition))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Attrition Distribution",x="Attrition",y="Count")+theme(legend.position = "none"),

#Business Travel
ggplot(Employee,aes(x=Employee$BusinessTravel))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Business Travel Distribution",x="Frequency of Business Travel",y="Count")+theme(legend.position = "none"),

#Department
ggplot(Employee,aes(x=Employee$Department))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Department Distribution",x="Department",y="Count")+theme(legend.position = "none"),

#Education
ggplot(Employee,aes(x=Employee$Education))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Education Distribution",x="Education",y="Count")+theme(legend.position = "none"),

#EducationField
ggplot(Employee,aes(x=Employee$EducationField))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Education Field Distribution",x="Education Field",y="Count")+theme(legend.position = "none"),

#Gender
ggplot(Employee,aes(x=Employee$Gender))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Gender Distribution",x="Gender",y="Count")+theme(legend.position = "none"),

#JobLevel
ggplot(Employee,aes(x=Employee$JobLevel))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Job Level Distribution",x="Job Level",y="Count")+theme(legend.position = "none"),

#JobRole
ggplot(Employee,aes(x=Employee$JobRole))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Job Role Distribution",x="Job Role",y="Count")+theme(legend.position = "none"),

#MaritalStatus
ggplot(Employee,aes(x=Employee$MaritalStatus))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Marital Status Distribution",x="Marital Status",y="Count")+theme(legend.position = "none"),

#StockOptionLevel
ggplot(Employee,aes(x=Employee$StockOptionLevel))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Stock Option Level Distribution",x="Stock Option Level",y="Count")+theme(legend.position = "none"),

#EnvironmentSatisfaction
ggplot(Employee,aes(x=Employee$EnvironmentSatisfaction))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Environment Satisfaction Survey Distribution",x="Environment Satisfaction",y="Count")+theme(legend.position = "none"),

#JobSatisfaction
ggplot(Employee,aes(x=Employee$JobSatisfaction))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Job Satisfaction Survey Distribution",x="Job Satisfaction",y="Count")+theme(legend.position = "none"),

#WorkLifeBalance
ggplot(Employee,aes(x=Employee$WorkLifeBalance))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Work Life Balance Survey Distribution",x="Work Life Balance",y="Count")+theme(legend.position = "none"),

#JobInvolvement
ggplot(Employee,aes(x=Employee$JobInvolvement))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Job Involvement Rating Distribution",x="Job Involvement",y="Count")+theme(legend.position = "none"),

#PerformanceRating
ggplot(Employee,aes(x=Employee$PerformanceRating))+geom_bar(aes(color="white"))+theme_minimal()+
  labs(title="Performance Rating Distribution",x="Performance Rating",y="Count")+theme(legend.position = "none"),nrow=3,ncol=5)

####################################################################################
#Segmented Univariate analysis

plot_grid(ggplot(Employee,aes(x=BusinessTravel,fill=Attrition))+geom_bar()+bar_theme+theme(legend.position = "right"),
          ggplot(Employee,aes(x=Age,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=Department,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=Education,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=EducationField,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=Gender,fill=Attrition))+geom_bar()+bar_theme,align = "h")

plot_grid(ggplot(Employee,aes(x=JobLevel,fill=Attrition))+geom_bar()+bar_theme+theme(legend.position = "right"),
          ggplot(Employee,aes(x=JobRole,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=StockOptionLevel,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=PercentSalaryHike,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=MaritalStatus,fill=Attrition))+geom_bar()+bar_theme,align = "h")


plot_grid(ggplot(Employee,aes(x=TotalWorkingYears,fill=Attrition))+geom_bar()+bar_theme+theme(legend.position = "right"),
          ggplot(Employee,aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=YearsAtCompany,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=YearsWithCurrManager,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=EnvironmentSatisfaction,fill=Attrition))+geom_bar()+bar_theme,align = "h")


plot_grid(ggplot(Employee,aes(x=JobSatisfaction,fill=Attrition))+geom_bar()+bar_theme+theme(legend.position = "right"),
          ggplot(Employee,aes(x=WorkLifeBalance,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=JobInvolvement,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=PerformanceRating,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=Median_Actual_Wrkhrs,fill=Attrition))+geom_bar()+bar_theme,
          ggplot(Employee,aes(x=percent_of_Absentism,fill=Attrition))+geom_bar()+bar_theme,align = "h")

ggplot(Employee,aes(x=DistanceFromHome,fill=Attrition))+geom_bar()

##############################################################################
#Bi-Variate Analysis
ggplot(Employee,aes(x=EducationField,fill=Attrition))+geom_bar()+bar_theme+facet_wrap(~Department)

ggplot(Employee,aes(x=Gender,fill=Attrition))+geom_bar()+bar_theme+facet_wrap(~JobRole)

ggplot(Employee,aes(x=Gender,fill=Attrition))+geom_bar()+bar_theme+facet_wrap(~MaritalStatus)

ggplot(Employee,aes(x=JobSatisfaction,fill=Attrition))+geom_bar()+bar_theme+facet_wrap(~Department)

ggplot(Employee,aes(x=EnvironmentSatisfaction,fill=Attrition))+geom_bar()+bar_theme+facet_wrap(~Department)

ggplot(Employee,aes(x=JobSatisfaction,fill=Attrition))+geom_bar()+bar_theme+facet_wrap(~Gender)

###############################################################################
# Boxplots of numeric variables relative to Attrition
plot_grid(ggplot(Employee, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Employee, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Employee, aes(x=Attrition,y=percent_of_Absentism, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(Employee, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Employee, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Employee, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

#################################################################################
# Correlation between numeric variables
# Bivariate Analysis of Continuous Variables

ggpairs(Employee[, c("Age", "MonthlyIncome", "NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears",
                     "YearsAtCompany","percent_of_Absentism")])

#Total working years to age is highly correlated (0.681) and Total working years to years at company is
#also highly correlated (0.628)

##################################################################################################

### Data Preparation for Model BUilding

##Changing variables in correct format

Employee$Gender<- ifelse(Employee$Gender=="Female",1,0)

# Outlier treatment and imputing missing value
#Boxplot showed no outlier, except in Monthly salary, checking the same with percentiles

quantile(c(Employee$MonthlyIncome),seq(0,1,.01),na.rm = T)

# Missing value
sapply(Employee, function(x) sum(is.na(x))) # shows all 111 NAs 

table(factor(Employee$Attrition),is.na(Employee$EnvironmentSatisfaction))
table(factor(Employee$Attrition),is.na(Employee$NumCompaniesWorked))
table(factor(Employee$Attrition),is.na(Employee$JobSatisfaction))
table(factor(Employee$Attrition),is.na(Employee$WorkLifeBalance))
table(factor(Employee$Attrition),is.na(Employee$TotalWorkingYears))

#out of 111 NA's 16 NA's are of employees who have left and balance employees who are still in the system
# it is best to remove these observatiosn from analysis

Employee<-Employee[!is.na(Employee$TotalWorkingYears),]
Employee<-Employee[!is.na(Employee$NumCompaniesWorked),]
Employee<-Employee[!is.na(Employee$EnvironmentSatisfaction),]
Employee<-Employee[!is.na(Employee$JobSatisfaction),]
Employee<-Employee[!is.na(Employee$WorkLifeBalance),]

table(is.na(Employee))

# Feature standardisation

# Normalising continuous features 
Employee$DistanceFromHome<- scale(Employee$DistanceFromHome)             # Mean = 9.2   and SD = 8.1
Employee$MonthlyIncome<- scale(Employee$MonthlyIncome)                   # Mean = 65060 and SD = 47045
Employee$NumCompaniesWorked<-scale(Employee$NumCompaniesWorked)          # Mean = 2.69  and SD = 2.5
Employee$PercentSalaryHike<-scale(Employee$PercentSalaryHike)            # Mean = 15.2  and SD = 3.66 
Employee$TotalWorkingYears<-scale(Employee$TotalWorkingYears)            # Mean = 11.3  and SD = 7.79
Employee$TrainingTimesLastYear<-scale(Employee$TrainingTimesLastYear)    # Mean = 2.8   and SD = 1.29   
Employee$YearsAtCompany<-scale(Employee$YearsAtCompany)                  # Mean = 7.03  and SD = 6.15
Employee$YearsSinceLastPromotion<-scale(Employee$YearsSinceLastPromotion)# Mean = 2.19  and SD = 3.23
Employee$YearsWithCurrManager<-scale(Employee$YearsWithCurrManager)      # Mean = 4.13  and SD = 3.57
Employee$Median_Actual_Wrkhrs<-scale(Employee$Median_Actual_Wrkhrs)      # Mean = 7.68  and SD = 1.34
Employee$percent_of_Absentism<-scale(Employee$percent_of_Absentism)      # Mean = 5.09  and SD = 2.22

# converting target variable Attrition No/Yes character to factorwith levels 0/1 
Employee$Attrition<- ifelse(Employee$Attrition=="Yes",1,0)


# Checking Attrition rate of Employee

Attrition <- sum(as.numeric(Employee$Attrition))/nrow(Employee)
Attrition # 16.16% Attrition Rate. 

table(format(Employee$Age))

#Converting Age into Age Buckets

Employee$Age_Group<-ifelse(Employee$Age <= 25, "18 - 25", ifelse(Employee$Age <= 30, "25 - 30",
                    ifelse(Employee$Age <= 35, "30 - 35",ifelse(Employee$Age <= 40, "35 - 40",
                    ifelse(Employee$Age <= 45, "40 - 50",ifelse(Employee$Age <= 55, "50 - 55",
                    "Above 55"))))))

Employee$Age<-NULL
Employee<-setcolorder(Employee,c(1,28,2:27))

# creating a dataframe of categorical features
Employee_chr<- Employee[,-c(1,3,6,9,13,14,15,17,18,19,20,21,27,28)]

# converting categorical attributes to factor
Employee_fact<- data.frame(sapply(Employee_chr, function(x) factor(x)))
str(Employee_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Employee_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =Employee_fact))[,-1]))
# Final dataset
Employee_final<- cbind(Employee[,c(3,6,9,13,14,15,17,18,19,20,21,27,28)],dummies)
View(Employee_final) # 4300 obs of 62 Variables

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(Employee_final$Attrition, SplitRatio = 0.7)

train = Employee_final[indices,]

test = Employee_final[!(indices),]

########################################################################
########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2087.3....62 coeff..nullDev 2661.4...resDev 1963.3

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Removing EducationField.xLife.Sciences
model_3<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development +  EducationField.xMarketing + 
               EducationField.xOther + EducationField.xTechnical.Degree + 
               JobLevel.x5  + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)
summary(model_3)
vif(model_3)

# Excluding variables whoes p values are greater than 0.05 one by one
#EducationField.xMarketing
model_4<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development +  
               JobLevel.x5  + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)


summary(model_4)
vif(model_4)

# Removing variables which has high vif and low significance,
# Removing Businesstravel_rarely, 

model_5<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently  + 
               Department.xResearch...Development +  
               JobLevel.x5  + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_5)
vif(model_5)

# Excluding variables JobLevl.x5
model_6<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                        TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                        Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
                        Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                        BusinessTravel.xTravel_Frequently  + 
                        Department.xResearch...Development +  
                        JobRole.xManufacturing.Director + 
                        JobRole.xResearch.Director  + 
                        JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                        EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                        EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                        JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                        WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                        data = train)

summary(model_6)
vif(model_6)

#Removing variable Department.xResearch...Development which has p Value just greater than 0.05
model_7<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently  +  
               JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)


summary(model_7)
vif(model_7)


# RemovingJobInvolvement.X3 as p value is less insignificant in comparision to other variables

model_8<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently  +  
               JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", 
               data = train)


summary(model_8)
vif(model_8)


# Removing worklifebalance.x2 and worklifebalance.x3 as VIF is not reducing inspite of removing other
# variables.  

model_9<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently  +  
               JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x4, family = "binomial", 
               data = train)


summary(model_9)
vif(model_9)



# Removing WorkLifeBalance.X4 as p values are insignificant

model_10<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
               TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
               Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
               Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
               BusinessTravel.xTravel_Frequently  +  
               JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director  + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4, family = "binomial", 
             data = train)


summary(model_10)
vif(model_10)

# Remvoing StockoptionLevel.x1 as it is the less signifcant compared to other variables

model_11<-glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director  + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
                data = train)


summary(model_11)
vif(model_11)

# Remvoing Total Working years as VIF is not reducting inspite of removal of other variables

model_12<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs + Age_Group.x25...30 + Age_Group.x30...35 + 
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director  + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
                data = train)


summary(model_12)
vif(model_12)

# Remvoing Age_Group.X25...30 due to lesser significance

model_13<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs + Age_Group.x30...35 + 
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director  + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
              data = train)


summary(model_13)
vif(model_13)

# Remvoing Age_Group.X30...35 due to lesser significance

model_14<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs +  
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director  + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
              data = train)


summary(model_14)
vif(model_14)

# Remvoing JobRole.XSales.Executive due to lesser significance

model_15<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs +  
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director  + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
              data = train)


summary(model_15)
vif(model_15)

# Remvoing JobRole.XResearch.Director due to lesser significance

model_16<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs +  
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
                data = train)


summary(model_16)
vif(model_16)

# Remvoing Jobsatisfaction.x2 due to lesser significance

model_17<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs +  
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                JobSatisfaction.x4, family = "binomial", 
                data = train)


summary(model_17)
vif(model_17)

# Remvoing Jobsatisfaction.x3 due to lesser significance

model_18<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs +  
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                JobSatisfaction.x4, family = "binomial", 
              data = train)


summary(model_18)
vif(model_18)

# Remvoing TrainingTimesLast Yeardue to lesser significance

model_19<-glm(formula = Attrition ~ NumCompaniesWorked + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                Median_Actual_Wrkhrs +  
                Age_Group.x35...40 + Age_Group.x40...50 + Age_Group.x50...55 + 
                BusinessTravel.xTravel_Frequently  +  
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                JobSatisfaction.x4, family = "binomial", 
              data = train)


summary(model_19)
vif(model_19)

########################################################################
# With 14 significant variables in the model_19 it is final model

final_model<- model_19

#######################################################################
### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)


# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

# Let's use the probability cutoff of 40%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf


#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.001 to 0.84000 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.001,.84,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.4,0.6,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]
cutoff

# Values of  0.1704949 0.1789697 0.1874444 and average of these numbers is cutoff which is 0.17897

# Let's choose a cutoff value of 0.17897 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.17897, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition_1 <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition_1 <- ifelse(test_actual_attrition=="Yes",1,0)


#on testing  data
pred_object_test<- prediction(test_cutoff_attrition_1, test_actual_attrition_1)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

# 0.4926

####################################################################
# Calculating Lift & Gain and plotting the chart

# plotting the lift chart
lift <- function(labels , predicted_prob,groups=10) {
  
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

Attrition_decile = lift(test_actual_attrition_1, test_pred, groups = 10)
View(Attrition_decile)

#Exporting the Attrition_decile table to CSV to plot the Lift Gain Chart in Tableau
liftgain <- write.csv(Attrition_decile,file="Lift_Gain.csv")

###################################################################################################