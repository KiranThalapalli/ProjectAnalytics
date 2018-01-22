#Linear Regression Car Prices Assignment Submitted by Kiran T DDA1710335

#Business Objective
#Goal is to find the significant variables that helps in predicting the price of a car
#Understanding the factors affecting the pricing of cars in American Market
#Management can accordingly use this parameters to design their cars and also come up with some business strategies
#APPROACH
#Since the price of car is a continuous variable hence choosing Linear Regression model in this assignment

#***********************************************************************************************#
#Loading the required packages
library(MASS)
library(car)
library(corrplot)
library(ggplot2)
library(DescTools)

#***********************************************************************************************#
#Data Gathering
cars <- read.csv("CarPrice_Assignment.csv")
str(cars)
View(cars)

#**********************************************************************************************#
#Data Cleansing

#Checking for duplicates in the dataset
sum(duplicated(cars))
#Since the sum is zero there are no duplicate records in the dataset

#Checking for Missing values if any
sum(is.na(cars))
#There are no missing values in the dataset as the sum is 0

#Splitting the Company and Car Name from the column carcompany
#Removing special chareacters from the CarName Variable and extracting the company name
cars$CarName <- gsub("-"," ",cars$CarName)
cars$CarName <- gsub(" .*", "", cars$CarName)
#Changing the column name
colnames(cars)[3] <- "carcompany"

#Dropping off Car_ID since it does not provide any significance meaning and all the records are unique with no relation
cars <- cars[,-1]

#Checking for unique brand names
unique(cars$company)

#Since There are multiple entries for same company name, fixing all such entries
cars$carcompany[which(cars$carcompany == "maxda")] <- "mazda"
cars$carcompany[which(cars$carcompany == "Nissan")] <- "nissan"
cars$carcompany[which(cars$carcompany == "porcshce")] <- "porsche"
cars$carcompany[which(cars$carcompany == "toyouta")] <- "toyota"
cars$carcompany[which(cars$carcompany == "vokswagen" | cars$carcompany == "vw")] <- "volkswagen"

#carcompany is a categorical hence converting it to factor type
cars$carcompany <- as.factor(cars$carcompany)

#Checking if any such spell errors in other variables which has more than two levels of data
unique(cars$carbody)
unique(cars$enginetype)
unique(cars$cylindernumber)
unique(cars$fuelsystem)
#preserving the spfi level variable even though it is for one record. Considering all the fuel system to be unique
summary(cars$fuelsystem)

#Checking for outliers in the data
#SINCE OUR DATA SET HAS DATA FOR ALL KINDS OF AUTOMOBILES, IT WOULD BE OBVIOUS THAT THERE CAN EXIST VARIED RANGES IN EACH ATTRIBUTE
#ALL THIS OUTLIERS ARE EXPECTED SINCE THE AUTOMOBILES IN THIS DATA SET CONTAINS DATA FOR VARIOUS CAR TYPES
#EACH CAR TYPE CAN RANGE FROM A NORMAL HATCHBACK SIZE TO A MIGHTY WAGAN SIZE ACCORDINGLY OTHER ATTRIBUTES TO SUIT THEM
#BASED ON THESE VARAIATIONS, PRICES CAN ALSO VARY AND HENCE OUTLIER REMOVAL MAY NOT BE SUITABLE IN THIS AS THE VARIABLES MAY NOT BE ABLE TO EXPLAIN THE ACTUAL PRICES
#HENCE CONSIDERING ALL OUTLIERS TO BE VALID PREDICTORS FOR THE PRICE DEPENDENT VARIABLES
#However possible outliers that exist are commented

boxplot(cars$wheelbase)
quantile(cars$wheelbase,seq(0,1,0.01))
#No outliers in the wheelbase

boxplot(cars$carlength)
quantile(cars$carlength,seq(0,1,0.01))
#Fixing outliers present at 2-3% and 99-100% jump
#cars$carlength[which(cars$carlength < 155.900)] <- 155.900
#cars$carlength[which(cars$carlength > 202.480)] <- 202.480

boxplot(cars$carwidth)
quantile(cars$carwidth,seq(0,1,0.01))
#No outliers in carwidth

boxplot(cars$carheight)
quantile(cars$carheight,seq(0,1,0.01))
#No signoficant change in carheight however similar 0.7 value jumps are there inbetween, hence keeping as it is

boxplot(cars$curbweight)
quantile(cars$curbweight,seq(0,1,0.01))
#Fixing the outlier present at 0-15 jump
#cars$curbweight[which(cars$curbweight < 1819.72)] <- 1819.72

boxplot(cars$enginesize)
quantile(cars$enginesize,seq(0,1,0.01))
#10 to 20 amounts of jump in engine size happens at some data intervals. so fixing all those outliers when there is a significant change compared to 20
#Which happens at 98-99%
#cars$enginesize[which(cars$enginesize > 256.08)] <- 256.08

boxplot(cars$boreratio)
quantile(cars$boreratio,seq(0,1,0.01))
#Fixing the outliers fore boreratio at 0-1% and 99-100%
#cars$boreratio[which(cars$boreratio < 2.9100)] <- 2.9100 
#cars$boreratio[which(cars$boreratio > 3.8000)] <- 3.800

boxplot(cars$stroke)
quantile(cars$stroke,seq(0,1,0.01))
#Fixing the outlier for stroke at 1-2% and 99-100%
#cars$stroke[which(cars$stroke < 2.6400)] <- 2.6400
#cars$stroke[which(cars$stroke > 3.9000)] <- 3.9000

quantile(cars$compressionratio)
quantile(cars$compressionratio,seq(0,1,0.01))
#Fixing all the values after 90%, since only 10% of data shoots up
#cars$compressionratio[which(cars$compressionratio > 10.9400)] <- 10.9400

boxplot(cars$horsepower)
quantile(cars$horsepower,seq(0,1,0.01))
#Fixing horsepower at 99-100% jump
#cars$horsepower[which(cars$horsepower > 207.00)] <- 207.00

boxplot(cars$peakrpm)
quantile(cars$peakrpm,seq(0,1,0.01))
#No outliers in peakrpm

boxplot(cars$citympg)
quantile(cars$citympg,seq(0,1,0.01))
#Fixing outliers fo citympg at 98-99%
#cars$citympg[which(cars$citympg > 38.00)] <- 38.00

boxplot(cars$highwaympg)
quantile(cars$highwaympg,seq(0,1,0.01))
#Fixing outliers fo highwaympg at 98-99%
#cars$highwaympg[which(cars$highwaympg > 46.92)] <- 46.92

#**********************************************************************************************#
#Derived Metrics
#Calculating the Torque from the known parameters as Torque is one of the major spec that is looked into while buying a car
#Formula for computing : Torque = (Horsepower*5252)/rpm
cars$torque <- round((cars$horsepower*5252)/cars$peakrpm,2)

#Calculating Power-to-Weight ratio (Acceleration) Higher the value better it is
cars$powerWeightRatio <- cars$horsepower/cars$curbweight

#**********************************************************************************************#
#Univariate Analysis of significant variables

#UNIVARIATE ANALYSIS OF CATEGORICAL VARIABLE
#Count of Variants in each brand
ggplot(cars,aes(x=cars$carcompany))+geom_bar()+theme_minimal()

#Fuel type
ggplot(cars,aes(x=cars$fueltype))+geom_bar()+theme_minimal()

#carbody
ggplot(cars,aes(x=cars$carbody))+geom_bar()+theme_minimal()

#No of Cylinders
ggplot(cars,aes(x=cars$cylindernumber))+geom_bar()+theme_minimal()


#UNIVARIATE ANALYSIS OF CONTINUOUS VARIABLES
#Engine Size
ggplot(cars,aes(x=cars$enginesize))+geom_histogram(bins = 50)+theme_minimal()
Desc(cars$enginesize,main="Engine Size")

#Horsepower
ggplot(cars,aes(x=cars$horsepower))+geom_histogram(bins = 50)+theme_minimal()
Desc(cars$horsepower,main="Horse Power")

#City Mielege
ggplot(cars,aes(x=cars$citympg))+geom_histogram()+theme_minimal()
Desc(cars$citympg,main="City Mielage")

#Highway Mielage
ggplot(cars,aes(x=cars$highwaympg))+geom_histogram()+theme_minimal()
Desc(cars$highwaympg,main="Highway Mielage")

#Car Price Distribution
ggplot(cars,aes(x=cars$price))+geom_histogram()+theme_minimal()
Desc(cars$price,main="Car Price Distribution")

#Peakrpm
Desc(cars$peakrpm,main="Peak RPM")

#Stroke
Desc(cars$stroke,main="Stroke")

#Power to Weight Ratio or Acceleration
Desc(cars$powerWeightRatio,main = "Power to Weight Ratio")

#Torque
Desc(cars$torque,main = "Torque")

#BIVARIATE ANALYSIS
#Observe the price range models that each company manufactures
ggplot(cars,aes(x=cars$price))+geom_histogram()+facet_wrap(~cars$carcompany)+theme_minimal()

#Price vs carbody
ggplot(cars,aes(x=cars$price))+geom_histogram()+facet_wrap(~cars$carbody)+theme_minimal()

#Price vs engine type
ggplot(cars,aes(x=cars$price))+geom_histogram()+facet_wrap(~cars$enginetype)+theme_minimal()

#Price vs cylinder number
ggplot(cars,aes(x=cars$price))+geom_histogram()+facet_wrap(~cars$cylindernumber)+theme_minimal()

#Price vs aspiration
ggplot(cars,aes(x=cars$price))+geom_histogram()+facet_wrap(~cars$aspiration)+theme_minimal()

#Price vs fuel type
ggplot(cars,aes(x=cars$price))+geom_histogram()+facet_wrap(~cars$fueltype)+theme_minimal()

#Price vs enginesize
ggplot(cars,aes(y=cars$price,x=cars$enginesize))+geom_point()+theme_minimal()

#Price vs horsepower
ggplot(cars,aes(y=cars$price,x=cars$horsepower))+geom_point()+theme_minimal()

#Price vs peakrpm
ggplot(cars,aes(y=cars$price,x=cars$peakrpm))+geom_point()+theme_minimal()

#Prive vs Stroke
ggplot(cars,aes(y=cars$price,x=cars$stroke))+geom_point()+theme_minimal()
#****************************************************************************************#
#Modifying data suitable to build model. Converting the categorical variables into numeric type

#Symboling is a categorical variable since it is of int type converting to factor
cars$symboling <- as.factor(cars$symboling)
str(cars$symboling)
#Symboling ranges between -3 to +3
#Since there are six levels in the data it would be good to bin them as Safe and Risky
summary(cars$symboling)
levels(cars$symboling)[1:3] <- "safe"
levels(cars$symboling)[2:4] <- "risky"
#Converting this two level variable into numeric, where one represents risky and zero represents safe
levels(cars$symboling) <- c(0,1)
cars$symboling <- as.numeric(levels(cars$symboling))[cars$symboling]


#Creating dummy variables for carcompany 
dummy_carcompany <- data.frame(model.matrix(~carcompany,data = cars))[,-1]
cars <- cbind(cars,dummy_carcompany)[,-2]

#Changing levels of fuel type from diesel and gas to 0 and 1 respectively
summary(cars$fueltype)
levels(cars$fueltype) <- c(0,1)
cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]

#Changing levels of aspiration from std and turbo to 0 and 1 respectively
summary(cars$aspiration)
levels(cars$aspiration) <- c(0,1)
cars$aspiration <- as.numeric(levels(cars$aspiration))[cars$aspiration]

#Changing levels of doornumber from four and two to 0 and 1 respectively
summary(cars$doornumber)
levels(cars$doornumber) <- c(0,1)
cars$doornumber <- as.numeric(levels(cars$doornumber))[cars$doornumber]

#Creating dummy variables for carbody
dummy_carbody <- data.frame(model.matrix(~carbody,data = cars))[,-1]
cars <- cbind(cars,dummy_carbody)[,-5]

#Creating dummy variables for drivewheel
dummy_drivewheel <- data.frame(model.matrix(~drivewheel,data = cars))[,-1]
cars <- cbind(cars,dummy_drivewheel)[,-5]

#Changing levels of enginelocation from front and rear to 0 and 1 respectively
summary(cars$enginelocation)
levels(cars$enginelocation) <- c(0,1)
cars$enginelocation <- as.numeric(levels(cars$enginelocation))[cars$enginelocation]

#Creating dummy variables for engine type
dummy_enginetype <- data.frame(model.matrix(~enginetype,data = cars))[,-1]
cars <- cbind(cars,dummy_enginetype)[,-11]

#Creating dummy variables for cylindernumber
summary(cars$cylindernumber)
dummy_cylindernumber <- data.frame(model.matrix(~cylindernumber,data = cars))[,-1]
cars <- cbind(cars,dummy_cylindernumber)[,-11]

#Converting this text numbers into actual numbers
#cars$cylindernumber <- as.character(cars$cylindernumber)
#cars$cylindernumber <- ifelse(cars$cylindernumber == "eight",8,ifelse(cars$cylindernumber == "five",5,ifelse(cars$cylindernumber == "four",4,ifelse(cars$cylindernumber == "six",6,ifelse(cars$cylindernumber == "three",3,ifelse(cars$cylindernumber == "twelve",12,ifelse(cars$cylindernumber == "two",2,0)))))))

#Creating dummy variables for engine type
dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem,data = cars))[,-1]
cars <- cbind(cars,dummy_fuelsystem)[,-12]

#Finally checking if all the categorical variables are converted properly and all the variables are of int or num type for model building
str(cars)

#***********************************************************************************************************#
#Dividing the data into train and test

#Setting Seed to get same set evertime the code is run
set.seed(50)

#Extracting training indices
train_indices <- sample(1:nrow(cars),0.7*nrow(cars))

#Initializing training data
train_cars <- cars[train_indices,]

#Initializing testing data
test_cars <- cars[-train_indices,]

#***********************************************************************************************************#
#Building Linear Regression Model to predict price of car

#Checking corelation between independent variables whenever there is a tie for high VIF and very low p values
#We could check for high correlation between two variables and remove the least significant variable among the two
correlation <- cor(cars)
corrplot(correlation, type = "upper", order = "hclust",tl.col = "black", tl.srt = 90)
#THIS TABLE WAS REFERRED ALWAYS TO GET THE CORRELATION BETWEEN VARAIABLES HENCE DIDN"T USE COR() ALL THE TIME BELOW 
######################################################################################
#WRITE THE CORELATION DATA FRAME INTO A CSV FILE FOR ANALYZING HIGH COLLINEAR VARIABLES
write.csv(correlation,"CorrelationFile.csv",row.names = F)

######################################################################################
#Considering all independent variables in the first model
model1 <- lm(price~.,data = train_cars)
#Understanding the significance of independent variables by appling summary function
summary(model1)

#Since there are several insignificant variables, we could remove them using step-wise function
#Applying step AIC function to get rid of extremely insignificant variables that are present
step <- stepAIC(model1,direction = "both")
#Checking the variables suggested by StepAIC function
step

#Building the second version of model based on the significant variables suggested by step AIC function
model2 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               wheelbase + carlength + carwidth + carheight + enginesize + 
               boreratio + stroke + horsepower + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
               drivewheelrwd + enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model2)
#As there are still insignificant variables present, we would use VIF to check the multicollinearity of the predictor variables
#Removing variables with high VIF and low significance with the help of model2 summary going forward
#We also use the correlation matrix that was built earlier to choose the variable to drop among the high collinear variables 
vif(model2)

#Removing horsepower as it has very high VIF
model3 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               wheelbase + carlength + carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
               drivewheelrwd + enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumberfour + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model3)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model3)

#Removing cylindernuberfour
model4 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               wheelbase + carlength + carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
               drivewheelrwd + enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model4)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model4)

#Removing wheelbase
model5 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               carlength + carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
               drivewheelrwd + enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model5)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model5)

#Removing Drivewheelrwd
model6 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               carlength + carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
               enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model6)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model6)



#Removing carlength
model7 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelfwd + 
               enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model7)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model7)

#Removing carbodysedan
model8 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodywagon + drivewheelfwd + 
               enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumbersix + 
               fuelsystem2bbl + fuelsystemmpfi, data = train_cars)
summary(model8)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model8)

#Removing fuelsystemmpfi
model9 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               carwidth + carheight + enginesize + 
               boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
               carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
               carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
               carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
               carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
               carbodyhatchback + carbodywagon + drivewheelfwd + 
               enginetypel + enginetypeohcv + enginetyperotor + 
               cylindernumberfive + cylindernumbersix + 
               fuelsystem2bbl, data = train_cars)
summary(model9)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model9)

#Removing carheight
model10 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + enginesize + 
                boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumbersix + 
                fuelsystem2bbl, data = train_cars)
summary(model10)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model10)

#Removing cylindernumbersix
model11 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + enginesize + 
                boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model11)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model11)

#Removing enginetypel
model12 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + enginesize + 
                boreratio + stroke + peakrpm + citympg + powerWeightRatio + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model12)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model12)

#Removing citympg
model13 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + enginesize + 
                boreratio + stroke + peakrpm + powerWeightRatio + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model13)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model13)

#Removing carwidth
model14 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                enginesize + 
                boreratio + stroke + peakrpm + powerWeightRatio + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model14)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model14)

#Removing powerweightratio
model15 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                enginesize + 
                boreratio + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model15)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model15)

#Removing boreratio
model16 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                enginesize + 
                stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelfwd + 
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model16)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model16)

#Removing drivewheelfwd
model17 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                enginesize + 
                stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive +
                fuelsystem2bbl, data = train_cars)
summary(model17)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model17)

#Removing fuelsystem2bbl
model18 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                enginesize + 
                stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetypeohcv + enginetyperotor + 
                cylindernumberfive, data = train_cars)
summary(model18)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model18)

#Removing cylindernumberfive
model19 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                enginesize + 
                stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetypeohcv + enginetyperotor, data = train_cars)
summary(model19)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model19)

#Removing enginelocation
model20 <- lm(formula = price ~ symboling + aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + enginetypeohcv + enginetyperotor, data = train_cars)
summary(model20)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model20)

#Removing enginetypeohcv
model21 <- lm(formula = price ~ symboling + aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymazda + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetyperotor, data = train_cars)
summary(model21)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model21)

#Snce all the high VIF values are strongly significant removing variables by pvalue
#Removing  carcompanymazda
model22 <- lm(formula = price ~ symboling + aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + carbodywagon + enginetyperotor, data = train_cars)
summary(model22)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model22)

#Removing carbodywagon
model23 <- lm(formula = price ~ symboling + aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanyisuzu + carcompanymitsubishi + 
                carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model23)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model23)

#Removing carcompanyisuzu
model24 <- lm(formula = price ~ symboling + aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model24)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model24)

#Removing symboling
model25 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysaab + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model25)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model25)

#Removing carcompanysaab
model26 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanymitsubishi + carcompanynissan + carcompanypeugeot + carcompanyplymouth + 
                carcompanyporsche + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model26)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model26)

#Removing carcompanypeugeot
model27 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + carcompanydodge + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyporsche + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model27)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model27)

#Removing carcompanydodge
model28 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanymitsubishi + carcompanynissan + carcompanyplymouth + 
                carcompanyporsche + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model28)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model28)

#Removing carcompanyplymouth
model29 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanymitsubishi + carcompanynissan +
                carcompanyporsche + carcompanysubaru + carbodyhardtop + 
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model29)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model29)

#Removing carbodyhardtop
model30 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanymitsubishi + carcompanynissan +
                carcompanyporsche + carcompanysubaru +  
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model30)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model30)

#Removing carcompanynissan
model31 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanymitsubishi + carcompanyporsche + carcompanysubaru +  
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model31)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model31)

#Removingcarcompanymitsubishi
model32 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw + carcompanybuick + 
                carcompanyporsche + carcompanysubaru +  
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model32)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model32)

#Removing carcompanybuick as removing carcompanyporsche decreases the adjusted R squared value abruptly
model33 <- lm(formula = price ~ aspiration +
                enginesize + stroke + peakrpm + 
                carcompanyaudi + carcompanybmw +
                carcompanyporsche + carcompanysubaru +  
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model33)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model33)

#Removing peakrpm
model34 <- lm(formula = price ~ aspiration +
                enginesize + stroke + carcompanyaudi + carcompanybmw +
                carcompanyporsche + carcompanysubaru +  
                carbodyhatchback + enginetyperotor, data = train_cars)
summary(model34)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model34)

#Removing carbodyhatchback
model35 <- lm(formula = price ~ aspiration +
                enginesize + stroke + carcompanyaudi + carcompanybmw +
                carcompanyporsche + carcompanysubaru +  
                enginetyperotor, data = train_cars)
summary(model35)
#Adjusted R-squared hasn't changed significantly, checking further VIF
vif(model35)

#Now all the values for VIF for these variables are less than 2 and also all the predictor variables are stongly significant
#hence consdering model 35 as the final model for this problem

#***********************************************************************************************#
#Predicting Prices on Test data
Predict <- predict(model35,test_cars[,-19])
test_cars$test_price <- Predict 

#***********************************************************************************************#
#Assessing the model buit
#Calculating the error
test_cars$price_error <- test_cars$price - test_cars$test_price

# Plotting - Actual vs Predicted Prices
ggplot(test_cars,aes(x=test_cars$price,y=test_cars$test_price))+geom_point()+theme_minimal()
#Looks both the variables correlate well

#Plot errors
ggplot(test_cars,aes(price,price_error))+geom_line()+theme_minimal()
Desc(test_cars$price_error)
ggplot(test_cars,aes(x=price,y=price_error))+geom_point()+theme_minimal()
#Since the points in scaatter plot are randomly distributed no other variable that can be added to explain the data

#Verifying the R-squared value of test_price and Actual price
r <- cor(test_cars$price,test_cars$test_price)
rsquared <- r^2
rsquared


#************************************************************************************************#
#RESULT
#Model35
#Multile R-square = 0.903 and Adjusted R-squared = 0.8972
#R-square of the test data that is found using correlation is = 0.8623
#0.8623 and 0.8972 which has difference less than 5 hence acceptable, and it seems to predict the data well

#The China based company can concentrate on Technical Aspects such as
#Engine Size
#Std/turbo based aspiration
#Stroke
#EngineTypeRotor, to predict the prices well in the market using these technical aspects

#Car Manufacturing companies like Audi, BMW, Porsche, Subaru are strong brand predictors for prices

#*************************************************************************************************#


