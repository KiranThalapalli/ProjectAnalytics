# UBER CASE STUDY SUBMITTED BY KIRAN T DDA1710335

####################################################################################
#DATA GATHERING

#Setting up the working directory
#setwd("C:/Users/Kiran T/Desktop/IIIT-B/EDA/Uber Case Study")

#Importing the data set into R where uber is name of the data frame
uber <- read.csv("Uber Request Data.csv")

#Observing the structure of data
str(uber)
summary(uber)

#####################################################################################
#DATA CLEANING

#Having the column names consistantly
names(uber) <- c("Request_ID","Pickup_Point","Driver_ID","Trip_Status","Request_Time_Stamp","Drop_Time_Stamp")

#Replacing NA's in Driver ID column where cabs are not available to 0
#Adding zero here should not be a concern because applying descriptive statistics on this column is meaningless
uber$Driver_ID[which(is.na(uber$Driver_ID))] <- 0

#Parsing the date format's appropriately

library(lubridate)
uber$Request_Time_Stamp <- parse_date_time (uber$Request_Time_Stamp,c("d/m/Y H:M","d-m-Y H:M:S"))
uber$Drop_Time_Stamp <- parse_date_time(uber$Drop_Time_Stamp,c("d/m/Y H:M","d-m-Y H:M:S"))

#Replacing NA values in Drop Time Stamp to Request Time Stamp for all the NO CARS AVAILABLE AND CANCELLED trips
#ASSUMED BOTH TIME VALUES ARE SAME BECAUSE THIS REQUEST CAN BE ASSUMED TO GET COMPLETED AT THE TIME OF REQUEST ITSELF
uber$Drop_Time_Stamp[which(is.na(uber$Drop_Time_Stamp))] <- uber$Request_Time_Stamp[which(is.na(uber$Drop_Time_Stamp))] 

######################################################################################
#EXTRACTING DERIVED METRICS
#Segregating a user trip request into Accomplished/Not Accomplished
uber$User_Request <- NA
uber$User_Request[which(uber$Trip_Status == "Trip Completed")] <- "Accomplished"
uber$User_Request[which(uber$Trip_Status == "No Cars Available" | uber$Trip_Status == "Cancelled") ] <- "Not Accomplished"

#Extracting Weekday from the Request date
uber$Weekday <- wday(uber$Request_Time_Stamp,label=T)

#Extracting Request Hour and Drop Hour
uber$Request_Hour <- hour(uber$Request_Time_Stamp)
uber$Drop_Hour <- hour(uber$Drop_Time_Stamp)

#Travel Time
uber$Travel_Time_Mins <- round(as.numeric(uber$Drop_Time_Stamp-uber$Request_Time_Stamp)/60)

######################################################################################
#WRITE THE NEW DATA FRAME INTO A CSV FILE FOR ANALYZING IN TABLEAU
write.csv(uber,"Uber_Trip_Requests.csv",row.names = F)

######################################################################################
#UNIVARIATE ANALYSIS
#RANK FREQUENCY PLOTS OF UNORDERED CATEGORICAL VARIABLES: TRIP_STATUS
uber_Trip_Status <- uber[c(4)]
uber_Trip_Status <- aggregate(data = transform(uber_Trip_Status, Freq = seq_len(nrow(uber_Trip_Status))), Freq ~ ., length)
uber_Trip_Status$Rank <- NA
uber_Trip_Status$Rank[order(uber_Trip_Status$Freq,decreasing = T)] <- 1:nrow(uber_Trip_Status)
library(ggplot2)
#SCATTER PLOT
sp1 <- ggplot(uber_Trip_Status, aes(x = Rank, y = Freq)) + geom_point()+geom_line()
sp1 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#RANK FREQUENCY PLOTS OF UNORDERED CATEGORICAL VARIABLES: DRIVER_ID
uber_Driver_ID <- subset(uber,Driver_ID != 0)
uber_Driver_ID <- uber_Driver_ID[c(3)]
uber_Driver_ID <- aggregate(data = transform(uber_Driver_ID, Freq = seq_len(nrow(uber_Driver_ID))), Freq ~ ., length)
uber_Driver_ID$Rank <- NA
uber_Driver_ID$Rank[order(uber_Driver_ID$Freq,decreasing = T)] <- 1:nrow(uber_Driver_ID)
#SCATTER PLOT
sp2 <- ggplot(uber_Driver_ID, aes(x = Rank, y = Freq)) + geom_point()+geom_line()
sp2 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#RANK FREQUENCY PLOTS OF UNORDERED CATEGORICAL VARIABLES: PICKUP_POINT
uber_Pickup_Point <- uber[c(2)]
uber_Pickup_Point <- aggregate(data = transform(uber_Pickup_Point, Freq = seq_len(nrow(uber_Pickup_Point))), Freq ~ ., length)
uber_Pickup_Point$Rank <- NA
uber_Pickup_Point$Rank[order(uber_Pickup_Point$Freq,decreasing = T)] <- 1:nrow(uber_Pickup_Point)
#SCATTER PLOT
sp3 <- ggplot(uber_Pickup_Point, aes(x = Rank, y = Freq)) + geom_point()+geom_line()
sp3 + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

######################################################################################
#UNIVARIATE ANALYSIS OF ORDERED CATEGORICAL VARIABLES : WEEKDAY
#Conclusion: There is no much variations in the trip count based on the weekdays
plot1 <- ggplot(uber,aes(x=uber$Weekday))+geom_bar()
plot1

#UNIVARIATE ANALYSIS OF ORDERED CATEGORICAL VARIABLES : REQUEST_HOUR
plot12 <- ggplot(uber,aes(x=uber$Request_Hour))+geom_bar()
plot12

#UNIVARIATE ANALYSIS OF ORDERED CATEGORICAL VARIABLES : DROP_HOUR
plot13 <- ggplot(uber,aes(x=uber$Drop_Hour))+geom_bar()
plot13

#UNIVARIATE ANALYSIS ON ORDERED CATEGORICAL VARIABLES: TRAVEL_TIME_MINS
#FILTERING ALL ZERO MINS TRIPS I.E NO CARS AVAILABLE/CANCELLED TRIPS
uber_Travel_Time_GRT0 <- subset(uber,Travel_Time_Mins > 0)
plot14 <- ggplot(uber_Travel_Time_GRT0,aes(x=uber_Travel_Time_GRT0$Travel_Time_Mins))+geom_bar()
plot14
#SAMILAR ANALYSIS USING HISTOGRAM
#uber_Travel_Time <- uber_Travel_Time_GRT0[c(11)]
#plot14 <- ggplot(uber_Travel_Time,aes(x=uber_Travel_Time$Travel_Time_Mins))+geom_histogram(binwidth = 3)

#UNIVARIATE ANALYSIS ON ORDERED CATEGORICAL VARIABLES: REQUEST_TIME_STAMP
plot15 <- ggplot(uber,aes(x=uber$Request_Time_Stamp))+geom_histogram()
plot15

#UNIVARIATE ANALYSIS ON ORDERED CATEGORICAL VARIABLES: DROP_TIME_STAMP
plot16 <- ggplot(uber,aes(x=uber$Drop_Time_Stamp))+geom_histogram()
plot16

#######################################################################################
#SEGMENTED UNIVARIATE ANALYSIS
#NO OF TRIPS ASSINGNED TO EACH DRIVER
plot2 <- ggplot(uber_Driver_ID,aes(x=uber_Driver_ID$Driver_ID,y=uber_Driver_ID$Freq))+geom_point()
plot2

#No. OF TRIPS BASED ON THE TRIP STATUS
plot22 <- ggplot(uber,aes(x=uber$Trip_Status))+geom_bar()
plot22

#NO OF TRIPS REQUSTED FROM EITHER AIRPORT/CITY
plot23 <- ggplot(uber,aes(x=uber$Pickup_Point))+geom_bar(fill="#CC6600")+xlab("Pickup Point")+ylab("No. of Trips Requested")+coord_flip()
plot23

#NO OF TRIPS REQUSTED FROM EITHER AIRPORT/CITY BY WEEKDAY
plot24 <- ggplot(uber,aes(x=uber$Pickup_Point))+geom_bar(fill="#66CC99")+xlab("Pickup Point")+ylab("No. of Trips Requested")+facet_wrap(~uber$Weekday,nrow = 1)
plot24

#NO OF TRIPS UNABLE TO SATISFY BY UBER (UNABLE TO MEET THIS DEMAND)
uber_Unaccopmlised <- uber[which(uber$User_Request == "Not Accomplished"),]
plot25 <- ggplot(uber_Unaccopmlised,aes(x=uber_Unaccopmlised$Pickup_Point))+geom_bar(fill="#CC6600")+xlab("Pickup Point")+ylab("No. of Trips Cancelled/No Cars Available")+coord_flip()
plot25

#NO OF TRIPS UNABLE TO SATISFY BY UBER (UNABLE TO MEET THIS DEMAND) by WEEKDAY
plot26 <- ggplot(uber_Unaccopmlised,aes(x=uber_Unaccopmlised$Pickup_Point))+geom_bar(fill="#66CC99")+xlab("Pickup Point")+ylab("No. of Trips Cancelled/No Car Available")+facet_wrap(~uber_Unaccopmlised$Weekday,nrow = 1)
plot26

#######################################################################################
#BIVARIATE ANALYSIS
#NO OF PICKUP REQUESTS ON WEEDAY BASIS AT AIRPORT AND CITY
plot3 <- ggplot(uber,aes(x=uber$Weekday))+geom_bar()+facet_wrap(~uber$Pickup_Point)
plot3

#WHAT TIME OF THE DAY MOST PICKUP REQUEST COME IN AT AIRPORT OR CITY
#FOR AIRPORT/CITY: IT"S THE NO OF CARS LEAVING THE AIRPORT/CITY RESPECTIVELY (OUTFLOW PATTERN)
plot31 <- ggplot(uber,aes(x=uber$Request_Hour))+geom_bar()+facet_wrap(~uber$Pickup_Point)+xlab("Time of Request 24 Hr Clock")+ylab("No. of Trips Requested")
plot31

#WHAT TIME OF THE DAY MOST DROP HAPPENS AT AIRPORT OR CITY
#FOR AIRPORT/CITY: IT"S THE NO OF CARS ARRIVED AT THE AIRPORT/CITY RESPECTIVELY (INFLOW PATTERN)
plot32 <- ggplot(uber,aes(x=uber$Drop_Hour))+geom_bar()+facet_wrap(~uber$Pickup_Point)
plot32

#######################################################################################
#OVERALL NO. OF TRIPS ACCOMPLISHED OR NOT ACCROSS WEEDAYS AND HOUR OF REQUEST BY ACCOMPLISHMENT STATUS
plot33 <- ggplot(uber,aes(x=uber$Request_Hour,fill=uber$User_Request))+geom_bar()+facet_wrap(~ uber$Weekday)
plot33
#OVERALL NO. OF TRIPS ACCOMPLISHED OR NOT ACCROSS WEEDAYS AND HOUR OF REQUEST BY TRIP STATUS
plot34 <- ggplot(uber,aes(x=uber$Request_Hour,fill=uber$Trip_Status))+geom_bar()+facet_wrap(~ uber$Weekday)+xlab("Request Hour")+ylab("No. of Trips")+scale_fill_manual(values=c("#FF6666", "#9999CC", "#66CC99"))+guides(fill=guide_legend(title="Trip Status"))
plot34
#PLOTTING THE SAME BY SWAPPING THE VARIABLES FOR MORE ANALYSIS
plot35 <- ggplot(uber,aes(x=uber$Weekday,fill=uber$User_Request))+geom_bar()+facet_wrap(~ uber$Request_Hour)
plot35
#USING TRIP STATUS AS THE FILL CATEGORY
plot36 <- ggplot(uber,aes(x=uber$Weekday,fill=uber$Trip_Status))+geom_bar()+facet_wrap(~ uber$Request_Hour)+xlab("Request Hour")+ylab("No. of Trips")+scale_fill_manual(values=c("#FF6666", "#9999CC", "#66CC99"))+guides(fill=guide_legend(title="Trip Status"))
plot36

######################################################################################
#AIRPORT ANALYSIS
#NO. OF TRIPS ACCOMPLISHED OR NOT ACCROSS WEEDAYS AND HOUR OF REQUEST AT THE AIRPORT
uber_Airport_Pickups <- uber[which(uber$Pickup_Point == "Airport"),]
plot37 <- ggplot(uber_Airport_Pickups,aes(x=uber_Airport_Pickups$Request_Hour,fill=uber_Airport_Pickups$User_Request))+geom_bar()+facet_wrap(~uber_Airport_Pickups$Weekday)
plot37
#SAME AIRPORT ANALYSIS WITH TRIP STATUS
plot38 <- ggplot(uber_Airport_Pickups,aes(x=uber_Airport_Pickups$Request_Hour,fill=uber_Airport_Pickups$Trip_Status))+geom_bar()+facet_wrap(~uber_Airport_Pickups$Weekday)+xlab("Request Hour at Airport")+ylab("No. of Trips")+scale_fill_manual(values=c("#FF6666", "#9999CC", "#66CC99"))+guides(fill=guide_legend(title="Trip Status"))
plot38
#SWAPPING VARIABLES FOR MORE ANALYSIS
plot39 <- ggplot(uber_Airport_Pickups,aes(x=uber_Airport_Pickups$Weekday,fill=uber_Airport_Pickups$Trip_Status))+geom_bar()+facet_wrap(~uber_Airport_Pickups$Request_Hour)+xlab("Request Hour at Airport")+ylab("No. of Trips")+scale_fill_manual(values=c("#FF6666", "#9999CC", "#66CC99"))+guides(fill=guide_legend(title="Trip Status"))
plot39

######################################################################################
#CITY ANALYSIS
#NO. OF TRIPS ACCOMPLISHED OR NOT ACCROSS WEEDAYS AND HOUR OF REQUEST AT THE CITY
uber_City_Pickups <- uber[which(uber$Pickup_Point == "City"),]
plot40 <- ggplot(uber_City_Pickups,aes(x=uber_City_Pickups$Request_Hour,fill=uber_City_Pickups$User_Request))+geom_bar()+facet_wrap(~uber_City_Pickups$Weekday)
plot40
#SAME ANALYSIS WITH TRIP STATUS AS THE FILL COLOR
plot41 <- ggplot(uber_City_Pickups,aes(x=uber_City_Pickups$Request_Hour,fill=uber_City_Pickups$Trip_Status))+geom_bar()+facet_wrap(~uber_City_Pickups$Weekday)+xlab("Request Hour at City")+ylab("No. of Trips")+scale_fill_manual(values=c("#FF6666", "#9999CC", "#66CC99"))+guides(fill=guide_legend(title="Trip Status"))
plot41
#SWAPPING THE VARIABLES FOR GETTING DEEPER INTO THE DATA
plot42 <- ggplot(uber_City_Pickups,aes(x=uber_City_Pickups$Weekday,fill=uber_City_Pickups$Trip_Status))+geom_bar()+facet_wrap(~uber_City_Pickups$Request_Hour)+xlab("Request Hour at City")+ylab("No. of Trips")+scale_fill_manual(values=c("#FF6666", "#9999CC", "#66CC99"))+guides(fill=guide_legend(title="Trip Status"))
plot42

################################################################################
#ANALYZE WHICH TIME OF THE DAY CAUSES LONGEST TRIPS
plot5 <- ggplot(uber_Travel_Time,aes(x=uber_Travel_Time$Travel_Time_Mins))+geom_histogram(binwidth = 3)+facet_wrap(~uber_Travel_Time1$Request_Hour)
plot5
