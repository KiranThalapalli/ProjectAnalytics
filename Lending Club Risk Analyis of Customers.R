#LENDING CLUB ANALYSIS SUBMITTED BY KIRAN T, VIKRAM,PRAKASH
#Loading the required libraries
library(stringr)
library(lettercase)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(corrplot)
library(DescTools)
library(lubridate)
library(dplyr)
#Loading the Loan data

loan<-read.csv("loan.csv",na.strings=c("","NA"),as.is=TRUE,stringsAsFactors = F)

#Checking the initial summary glancing through the data

summary(loan)

#many columns has NA values and will not have any characterstic of loan applicants
#and hece have decided to remove columns which have NA Values

#to remove columns which only has entirely NA Values

loan1<-loan[colSums(!is.na(loan))>0]
summary(loan1)

###################################################################
#DATA CLEANING

#Checking duplicated entries.  Since sum is zero, concluding no duplicated items

sum(duplicated(loan1))

#to retain columns which is useful for data analysis and remove columns which have redundant
#removing columns which only has one value as zero or zero along with NA
na_count <-sapply(loan1, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

#on viewing the na_count we can safely conclude that following columns can be removed as it has
#either 0 , 1 or NA values

# tax_liens, delinq_amnt, chargeoff_within_12_mths, acc_now_delinq, application_type, policy_code,
# collections_12_mths_ex_med, mths_since_last_delinq, mths_since_last_record, desc
#Derriving a length of description metric before deleting desc column
loan1$num_of_desc_words <- sapply(gregexpr("\\W+", loan1$desc), length) + 1 
loan2<-loan1[, !(colnames(loan1) %in% c("tax_liens","delinq_amnt","chargeoff_within_12_mths","acc_now_delinq","application_type","policy_code","collections_12_mths_ex_med","mths_since_last_delinq","mths_since_last_record"))]

#Also following fiels can be removed with following logic as it does not add much value to data analysis
#member_id -  also randomly generated field by Lending Club for identification purposes only.
#sub_grade - contains redundant information that is already in the grade column.
#zip_code - mostly redundant with the addr_state column since only the first 3 digits of the 5 digit zip code are visible.

loan_new<-loan2[,!(colnames(loan2) %in% c("member_id","sub_grade","zip_code","pymnt_plan","initial_list_status","url","desc"))]

#correcting Date formats of all the dates

loan_new$issue_d<-dmy(paste0("01-",loan_new$issue_d))
typeof(loan_new$issue_d)

loan_new$earliest_cr_line<-dmy(paste0("01-",loan_new$earliest_cr_line))
typeof(loan_new$earliest_cr_line)

loan_new$last_pymnt_d<-dmy(paste0("01-",loan_new$last_pymnt_d))
typeof(loan_new$last_pymnt_d)

loan_new$next_pymnt_d<-dmy(paste0("01-",loan_new$next_pymnt_d))
typeof(loan_new$next_pymnt_d)

loan_new$last_credit_pull_d<-dmy(paste0("01-",loan_new$last_credit_pull_d))
typeof(loan_new$last_credit_pull_d)

#Assigning a new emp_title,title for all those blanks as Not Mentioned
loan_new[which(is.na(loan_new$emp_title)),grep("emp_title",colnames(loan_new))] <- "Not Mentioned"
loan_new[which(is.na(loan_new$title)),grep("title",colnames(loan_new))] <- "Not Mentioned"

#Formatting the text in home_ownership,title,emp_title and purpose columns to a Proper Case
loan_new$home_ownership <- str_ucfirst(str_decapitalize(loan_new$home_ownership))
loan_new$title <- str_ucfirst(str_decapitalize(loan_new$title))
loan_new$emp_title <- str_ucfirst(str_decapitalize(loan_new$emp_title))
loan_new$purpose <- str_cap_words(loan_new$purpose)

#Rounding off amount values to zero decimal places due to the high values of the actaual value itself or keeping the decimals to consistant decimal values
loan_new[,c("loan_amnt","funded_amnt","funded_amnt_inv","annual_inc","out_prncp","out_prncp_inv","total_pymnt","installment")] <- round(loan_new[,c("loan_amnt","funded_amnt","funded_amnt_inv","annual_inc","out_prncp","out_prncp_inv","total_pymnt","installment")],0)
loan_new[,c("total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_amnt")] <- round(loan_new[,c("total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_amnt")],0)
loan_new[,c("dti")] <- round(loan_new[,c("dti")],2)

#Making sure if all the required missing values are corrected before doing further data analysis by looking at the NA count
#We have considerably notice the less number of missing values, which may require additional info to impute them
#Huge number of NA in next_pymnt_d is because of the fully paid customers, since we would be analyzing the charged off customers we would have the dates for them hence retaining
na_count <-sapply(loan_new, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
View(na_count)

###################################################################################################
# DERIVED METRICS

##Monthly Income and Monthly Debt - Annual Income divide by 12 we get monthly income and
## monthly debt is monthly income * DTI / 100

loan_new$month_inc<-(loan_new$annual_inc/12)
loan_new$month_debt<-((loan_new$month_inc*loan_new$dti)/100)
loan_new$month_inc<-round(loan_new$month_inc,0)
loan_new$month_debt<-round(loan_new$month_debt,0)

##Loan Amount Defaulted is funded amout minus total received principal

loan_new$amt_defaulted<-(loan_new$funded_amnt-loan_new$total_rec_prncp)
loan_new$amt_defaulted<-round(loan_new$amt_defaulted,0)


##%of Loan Default is amt_defaulted/Funded amount

loan_new$perc_loan_default<-((loan_new$amt_defaulted/loan_new$funded_amnt)*100)
loan_new$perc_loan_default<-round(loan_new$perc_loan_default,0)

##To find revolving credit line available to borrower, which is revolving balance divided
##by revolving utilization rate

##conversion of revolving credit utilization to integer

loan_new$convrtd_revolutil_perctg<-(gsub("\\%","",loan_new$revol_util))
loan_new$convrtd_revolutil_perctg<-as.integer(loan_new$convrtd_revolutil_perctg)

##Derived metric of Revolving credit available

loan_new$total_revolcredit<-(loan_new$revol_bal/(100-loan_new$convrtd_revolutil_perctg)*100)
loan_new$total_revolcredit<-round(loan_new$total_revolcredit,0)

##Derived Metric of Credit history of the borrower is the loan issue date minus earliest credit line established date

loan_new$credit_histry_years<-round((loan_new$issue_d-loan_new$earliest_cr_line)/365,0)


#conversion of interest rate to integer and creating new column of interest rate slots

loan_new$convrtd_int_rate<-(gsub("\\%","",loan_new$int_rate))
loan_new$convrtd_int_rate<-as.integer(loan_new$convrtd_int_rate)
loan_new$intrate_slot = ifelse(loan_new$convrtd_int_rate < 10, "5% - 10%", ifelse(loan_new$convrtd_int_rate < 15,"10% - 15%",ifelse(loan_new$convrtd_int_rate < 20,"15% - 20%","20% & Above")))

##monthly_income intervals

loan_new$mthly_inc_slots<-ifelse(loan_new$month_inc <= 3000, "0 - 3000", ifelse(loan_new$month_inc <= 8000,"3001 - 8000",ifelse(loan_new$month_inc <= 15000,"8001 - 15000",ifelse(loan_new$month_inc <= 50000,"15001 - 50000",ifelse(loan_new$month_inc <= 100000,"50001 - 100000","Above 100000")))))

##monthly_debt invervals

loan_new$mth_debt_slots<-ifelse(loan_new$month_debt <= 500, "0 - 500", ifelse(loan_new$month_debt <= 1000,"501 - 1000",ifelse(loan_new$month_debt<= 2000,"1001 - 2000",ifelse(loan_new$month_debt <= 3000,"2001 - 3000",ifelse(loan_new$month_debt <= 5000,"3001 - 5000","Above 5000")))))

##Funded_Amt intervals

loan_new$funded_amt_slots<-ifelse(loan_new$funded_amnt <= 3000, "0 - 3000", ifelse(loan_new$funded_amnt <= 8000,"3001 - 8000",ifelse(loan_new$funded_amnt<= 15000,"8001 - 15000",ifelse(loan_new$funded_amnt <= 20000,"15001 - 20000",ifelse(loan_new$funded_amnt <= 30000,"20001 - 30000","Above 30000")))))

##revolving credit utilization rate - intervals

loan_new$revol_util_slots = ifelse(loan_new$convrtd_revolutil_perctg < 5, "0% - 5%", ifelse(loan_new$convrtd_revolutil_perctg < 20,"5% - 20%",ifelse(loan_new$convrtd_revolutil_perctg < 35,"20% - 35%",ifelse(loan_new$convrtd_revolutil_perctg < 50,"35% - 50%",ifelse(loan_new$convrtd_revolutil_perctg < 75,"50% - 75%",ifelse(loan_new$convrtd_revolutil_perctg < 85,"75% - 85%","85% & Above"))))))

##since the years befor 1970 is converted as 20th century adjuting for credit history negative numbers
## all negative numbers is taken as 45 years since it is before 1970
loan_new$credit_histry_years<-ifelse(loan_new$credit_histry_years < 0, 45,loan_new$credit_histry_years)

##credit History years - intervals
loan_new$credit_histry_slots = ifelse(loan_new$credit_histry_years < 5, "0 - 5", ifelse(loan_new$credit_histry_years < 10,"5 - 10",ifelse(loan_new$credit_histry_years < 20,"10 - 20",ifelse(loan_new$credit_histry_years < 30,"20 - 30","Above 30 Yrs"))))

###################################################################
#UNIVARIATE ANALYSIS OF CONTINUOUS VARIABLES
#UNDERSTANDING HOW EACH VARIABLE IS SPREAD ACCROSSED FOR MAJOR UNIQUE VARIABLES FOR OUR OBJECTIVE

plot1 <- ggplot(loan_new,aes(funded_amnt))+geom_histogram(bins=30,aes(color="black"))+theme_minimal()
Desc(loan_new$funded_amnt,main="Funded Amount Distribution")

plot2 <- ggplot(loan_new,aes(installment))+geom_histogram(bins=50,aes(color="black"))+theme_minimal()
Desc(loan_new$installment,main="Installment Distribution")

#We could see the outliers in the annual income values
plot3 <- Desc(loan_new$annual_inc,main="Annual Income Distribution")
plot3

plot4 <- ggplot(loan_new,aes(dti))+geom_histogram(bins=50,aes(color="black"))+theme_minimal()
Desc(loan_new$dti,main="Debt to Income Ration Distribution")

plot5 <- ggplot(loan_new,aes(open_acc))+geom_histogram(bins=50,aes(color="black"))+theme_minimal()
Desc(loan_new$open_acc,main="Open Account Distribution")

plot6 <- ggplot(loan_new,aes(revol_bal))+geom_histogram(bins=50,aes(color="black"))+theme_minimal()
Desc(loan_new$revol_bal,main="Revolving Balance Distribution")

plot7 <- ggplot(loan_new,aes(total_acc))+geom_histogram(bins=50,aes(color="black"))+theme_minimal()
Desc(loan_new$total_acc,main = "Total Account Distribution")

#Most of the principal amount that should be received for the funded amount and funded by investor are zero
Desc(loan_new$out_prncp)
Desc(loan_new$out_prncp_inv)

#Total Payment received till date for the amount funded
plot8 <- ggplot(loan_new,aes(total_pymnt))+geom_histogram(bins=50,aes(color="black"))+theme_minimal()
Desc(loan_new$total_pymnt)

#Principal amount received till date
plot9 <- ggplot(loan_new,aes(total_rec_prncp))+geom_histogram(bins=30,aes(color="black"))+theme_minimal()
Desc(loan_new$total_rec_prncp)

#Interest amount received till date
plot10 <- ggplot(loan_new,aes(total_rec_int))+geom_histogram(bins=30,aes(color="black"))+theme_minimal()
Desc(loan_new$total_rec_int)

#Late Recovery, Collection and Collection fees
plot11 <- Desc(loan_new$total_rec_late_fee,"Total Late Fees Received")
plot11
plot12 <- Desc(loan_new$recoveries,main="Recoveries Distibution")
plot12
plot13 <- Desc(loan_new$collection_recovery_fee,"Collection Fecovery Fee")
plot13

######################################################################################
#Analyzing all the above 10 plots in a grid
grid.arrange(plot1,plot2,plot4,plot5,plot6,plot7,plot8,plot9,plot10,ncol=3)
######################################################################################

#UNIVARIATE ANALYSIS OF CATEGORICAL VARiABLES
plot14 <- ggplot(loan_new,aes(x=term))+geom_bar(width=.25,fill="#993300")+theme_minimal()
plot14
plot15 <- ggplot(loan_new,aes(x=grade,fill=grade))+geom_bar(width=.50)+theme_minimal()
plot15
plot16 <- ggplot(loan_new,aes(x=home_ownership))+geom_bar()+theme_minimal()
plot16

#SEGMENTED UNIVARIATE ANALYSIS
Plot17<-ggplot(loan_new,aes(funded_amt_slots,fill=loan_status))+geom_bar(stat="count")
Plot18<-ggplot(loan_new,aes(intrate_slot,fill=loan_status))+geom_bar(stat="count")

Plot19<-ggplot(loan_new,aes(x=loan_status))+geom_bar(position = "stack",fill="blue")
Desc(loan_new$loan_status)
#BIVARIATE ANALYSIS
Plot20 <- ggplot(loan_new,aes(intrate_slot,fill=loan_status))+geom_bar(stat="count")
plot20
Plot21 <- ggplot(loan_new,aes(purpose,funded_amnt,fill=loan_status))+geom_bar(position="fill",stat="identity")+theme(axis.text.x=element_text(size=8,angle = 90))
plot21

########################################################################################
#ANALYSIS FOR DEFAULT
#Correlation Matrix
#TO FIND THE CORELATION BETWEEN THE VARIABLES BEFORE PROCEEDING WITH THE ANALYSIS TO GET AN OVERVIEW HOW THE VARIABLES ARE RELATED FOR CHARGED OFF CSUTOMERS

#Filtering the charged off customers
default <- subset(loan_new,loan_new$loan_status=="Charged Off")
#Considering only numeric data to plot correlation matrix
default <- default[,c(2:4,7,12,19,20,22:25,27,30:36,38,41:50)]
#"loan_amnt","funded_amnt","funded_amnt_inv","installment","annual_inc","dti","delinq_,36,
res <- cor(default,use="complete.obs")
corrplot(res, type = "upper", order = "hclust",tl.col = "black", tl.srt = 90)

#heat map if it helps in clustering
col<- colorRampPalette(c("green", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

########################################################################################
###Subsetting data of charged of loan to do Univariate,segmented univariate and
###bivariate analysis to find the drivers resulting in loan default

loan_chargedoff<- subset(loan_new,loan_new$loan_status == "Charged Off")

#################################################################
#Univariate Analysis on Charged off loans
#################################################################
#Analyzing individual variables
Desc(loan_chargedoff$funded_amnt,main="Funded amount distribution of Charged off Loans")

Desc(loan_chargedoff$verification_status,main="Defaulted based on Verification Status")

Desc(loan_chargedoff$amt_defaulted,main = "Loan Amt defaulted distribution")

Desc(loan_chargedoff$total_pymnt,main = "Total Payment recieved")

Desc(loan_chargedoff$dti,main = "Debt to Income ratio")

Desc(loan_chargedoff$month_inc,main = "Monthly Income distribution")

Desc(loan_chargedoff$grade,main = "Credit Grades")

Desc(loan_chargedoff$addr_state,main = "State wise loan defaults")

Desc(loan_chargedoff$credit_histry_slots,main = "Years of Credit History wise Loan defaults")

Desc(loan_chargedoff$delinq_2yrs,main = "Incidences of Deliquency on Loan over past 2 years")

Desc(loan_chargedoff$perc_loan_default,main = "% of loan Default")

Desc(loan_chargedoff$home_ownership,main="Percentage of Loan Default based on Home Ownership")

#################################################################
#Segmented Univariate Analysis on Charged off loans
#################################################################
Plot22<-ggplot(loan_chargedoff,aes(funded_amt_slots))+geom_bar(stat="count",fill="blue")
plot22
Plot23<-ggplot(loan_chargedoff,aes(mthly_inc_slots))+geom_bar(stat="count",fill="orange")
plot23
plot24<-ggplot(loan_chargedoff,aes(purpose,amt_defaulted))+geom_bar(stat="identity",fill="blue")+theme(axis.text.x=element_text(size=8,angle = 90))
plot24
Plot25<-ggplot(loan_chargedoff,aes(mth_debt_slots))+geom_bar(stat="count",fill="red")
plot25
Plot26<-ggplot(loan_chargedoff,aes(intrate_slot))+geom_bar(stat="count",fill="red")
plot26
Plot27<-ggplot(loan_chargedoff,aes(revol_util_slots))+geom_bar(stat="count",fill="red")+theme(axis.text.x=element_text(size=8,angle = 30))
plot27
#################################################################
#Bivariate Analysis on Charged off loans
#################################################################

Plot28<-ggplot(loan_chargedoff,aes(purpose,amt_defaulted))+geom_bar(stat="identity",fill="blue")+theme(axis.text.x=element_text(size=8,angle = 90))
Plot29<-ggplot(loan_chargedoff,aes(purpose,funded_amnt))+geom_bar(stat="identity",fill="blue")+theme(axis.text.x=element_text(size=8,angle = 90))
grid.arrange(Plot28,Plot29)

Plot30<-ggplot(loan_chargedoff,aes(grade,amt_defaulted))+geom_bar(stat="identity",fill="light blue")
plot30
Plot31<-ggplot(loan_chargedoff,aes(home_ownership,amt_defaulted))+geom_bar(stat="identity",fill="red")
Plot31

Plot32<-ggplot(loan_chargedoff,aes(emp_length,amt_defaulted))+geom_bar(stat="identity",fill="red")
Plot33<-ggplot(loan_chargedoff,aes(emp_length))+geom_bar(fill="red")
grid.arrange(Plot32,Plot33)

Plot34<-ggplot(loan_chargedoff,aes(grade,delinq_2yrs))+geom_bar(stat="identity",fill="red")
plot34
Plot35<-ggplot(loan_chargedoff,aes(addr_state))+geom_bar(stat="count",fill="blue")+facet_wrap(~grade)+theme(axis.text.x=element_text(size=8,angle = 90))
plot35

#Based on the length of description we tried to predict that person is more likely to default, this is just an observation may not be true since more number of fully paid also has the same pattern
#ggplot(loan_new,aes(loan_new$num_of_desc_words))+geom_histogram()+facet_wrap(~loan_new$loan_status)

