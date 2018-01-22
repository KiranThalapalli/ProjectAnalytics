#SPARK FUND CASE STUDY SUBMITTED BY KIRAN T,VIKRAM R S,PRAVEEN SINGH

#Checkpoint 1: Data Cleaning 1
#Load companies and round2 data into data frames
companies <- read.delim("companies.txt",sep="\t",stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = F)

#Get the unique count of companies from companies and rounds2 dataset
no_rounds2 <- length(unique(rounds2$company_permalink))
no_companies <- length(unique(companies$permalink))

#Merging the rounds2 data frame with companies data frame
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink =tolower(rounds2$company_permalink)
master_frame <- merge(rounds2,companies,by.x="company_permalink",by.y="permalink",all=FALSE)

#**********************************************************************************************************

#Checkpoint 2: Data Cleaning 2
#To count the no. of NA's in raised_amount_usd
length(which(is.na(master_frame$raised_amount_usd)))

#Replacing NA values with suitable numeric value 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

#**********************************************************************************************************

#Checkpoint 3: Funding Type Analysis
#Finding the average investment among various funding type to choose the best suited for Spark Fund
group_by(master_frame,funding_round_type) %>% summarise(mean=mean(raised_amount_usd))

#**********************************************************************************************************

#Checkpoint 4: Country Analysis
library(dplyr)
#Filter the venture type of funding
fil_df <- filter(master_frame,funding_round_type=="venture")
new_df <- data.frame(fil_df$country_code,fil_df$raised_amount_usd)

#Aggregating the funding amount raised by countries
grp_df <- aggregate(new_df$fil_df.raised_amount_usd,by=list(new_df$fil_df.country_code),FUN = sum)
grp_df <- grp_df[grp_df$Group.1 != "",]

#Fetching only the Top 9 countries based on amount invested
top9 <- head(arrange(grp_df,desc(x)),n=9)

#**********************************************************************************************************

#Checkpoint 5: Sector Analysis 1
library(stringr)
library(tidyr)
#Get the primary sector of each company from various sectors it's involved in
split_sector <- str_split(master_frame$category_list,pattern = "\\|",n=2)
primary_sector<-data.frame(sapply(split_sector,function(x) x[[1]]),stringsAsFactors = F)
colnames(primary_sector) <- "primary_sector"

#Loading the mapping data onto maping_frame data frame
mapping_frame <- read.csv("mapping.csv",stringsAsFactors = F,check.names = F)

#Converting the mapping_frame from wide to long format for easier analysis
mapping <- gather(mapping_frame,main_sector,value,2:10)
mapping <- mapping[!(mapping$value==0),]
mapping <- mapping[,-3]

#Mapping the primary sectors to one among the 8 main sectors
merge1 <- merge(primary_sector,mapping,by.x="primary_sector",by.y="category_list",all.x=T)

#Merging the main & primary sector with main_frame
master_frame <- cbind(master_frame,merge1)

#*******************************************************************************************************

#Checkpoint 6: Sector Analysis 2
#To avoid R considering the numeric values in exponential notation
options(scipen = 2)

#Creating a data frame for country USA (Top 1st Invested English Speaking country) for Venture type of funding
D1 <- filter(master_frame,country_code == "USA" & funding_round_type=="venture" & raised_amount_usd >=5000000 & raised_amount_usd<=15000000)

#Calculating the count/sum of amount_raised_usd
D1s <- aggregate(list(investment_sum=D1$raised_amount_usd),by=list(main_sector=D1$main_sector),FUN=sum)
D1c <- count(D1,main_sector)

#Merging count of and sum of amount_raised_usd columns to D1
names(D1c) <- c("main_sector","investment_count")
D1 <- merge(D1,D1c,by="main_sector")
D1 <- merge(D1,D1s,by="main_sector")

#Creating a data frame for country GBR (Top 2nd Invested English Speaking country) for Venture type of funding
D2 <- filter(master_frame,country_code == "GBR" & funding_round_type=="venture" & raised_amount_usd >=5000000 & raised_amount_usd<=15000000)

#Calculating the count/sum of amount_raised_usd
D2s <- aggregate(list(investment_sum=D2$raised_amount_usd),by=list(main_sector=D2$main_sector),FUN=sum)
D2c <- count(D2,main_sector)

#Merging count of and sum of amount_raised_usd columns to D1
names(D2c) <- c("main_sector","investment_count")
D2 <- merge(D2,D2c,by="main_sector")
D2 <- merge(D2,D2s,by="main_sector")

#Creating a data frame for country IND (Top 3rd Invested English Speaking country) for Venture type of funding
D3 <- filter(master_frame,country_code == "IND" & funding_round_type=="venture" & raised_amount_usd >=5000000 & raised_amount_usd<=15000000)

#Calculating the count/sum of amount_raised_usd
D3s <- aggregate(list(investment_sum=D3$raised_amount_usd),by=list(main_sector=D3$main_sector),FUN=sum)
D3c <- count(D3,main_sector)

#Adding two more columns to D1
names(D3c) <- c("main_sector","investment_count")
D3 <- merge(D3,D3c,by="main_sector")
D3 <- merge(D3,D3s,by="main_sector")

#************************************************************************************************************

#Export the master_frame into excel for analyzing in Tableau
write.csv(master_frame,"master_file.csv",row.names = F)
